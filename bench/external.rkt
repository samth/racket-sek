#lang racket/base
;; Benchmarks borrowed from other implementations of related sequence types.
;;
;;   racket -y bench/external.rkt                 run everything
;;   racket -y bench/external.rkt apprepend map   run named scenarios
;;   racket -y bench/external.rkt --quick         fewer / smaller operations
;;   racket -y bench/external.rkt --json FILE     also append results as JSON
;;
;; main.rkt covers the shapes the paper and the OCaml library measure.  This
;; file covers the shapes that the *other* chunked-sequence libraries measure,
;; transcribed from their own suites so that the comparison is theirs and not
;; one chosen to flatter sek.  Three sources:
;;
;;   Scala   scala/scala, test/benchmarks/.../immutable/VectorBenchmark2.scala,
;;           the JMH suite Stefan Zeiger wrote for the 2.13 Vector rewrite
;;           ("radix-balanced finger tree vectors", scala/scala#8534).
;;           Contributes: apply-sequential, update-sequential, apprepend, ends,
;;           slice, bulk-append, map, filter.
;;
;;   immer   arximboldi/immer, benchmark/vector/{access,assoc,take,drop}.hpp,
;;           from Juan Pedro Bolivar Puente's "Persistence for the Masses:
;;           RRB-Vectors in a Systems Language" (ICFP 2017).  Its `_move` and
;;           `_mut` variants are its transients, and are the reason its suite
;;           is worth borrowing here at all.
;;           Contributes: take-drop, push-move.
;;
;;   bifurcan  lacuna/bifurcan, test/bifurcan/benchmark_test.clj, the suite
;;           Zach Tellman wrote to compare the JVM's sequence libraries and
;;           which clojure/core.rrb-vector reuses for its published numbers.
;;           Contributes: split-parts.
;;
;; Two of Scala's benchmarks are deliberately left out.  `vBadApplySequential`
;; differs from `vApplySequential` only in reading a field rather than a local,
;; which is a JIT question with no Racket counterpart; and `nvSliding` measures
;; an API (`sliding`) that none of the Racket structures have.
;;
;; Every measurement is in nanoseconds, per the unit named in the table title.
;; A dash means the structure has no efficient version of the operation.

(require racket/list
         racket/vector
         racket/treelist
         racket/mutable-treelist
         data/gvector
         "../sek-lib/sek/main.rkt"
         "main.rkt")     ; contenders (all-impls) and harness (measure, table)

;; ================================================================ scenarios

(define ext-scenarios (make-hash))
(define ext-scenario-order '())
(define-syntax-rule (define-ext-scenario name doc body ...)
  (begin (hash-set! ext-scenarios 'name (lambda () body ...))
         (set! ext-scenario-order (cons (cons 'name doc) ext-scenario-order))))

;; A row per contender for which `pick` yields an operation, plus whatever
;; extra rows the caller supplies.  A contender with no measurement at any
;; size is left out rather than shown as a row of dashes -- with every
;; structure carrying every operation it can perform, that now happens only
;; where the operation genuinely does not exist.
(define (rows-for sizes pick #:only [only (lambda (i) #t)] #:extra [extra '()])
  (append
   (for*/list ([i (in-list all-impls)]
               [vs (in-value (for/list ([n (in-list sizes)])
                               (and (only i) (pick i n))))]
               #:when (ormap values vs))
     (cons (impl-name i) vs))
   extra))

;; ------------------------------------------------------- Scala: element access

;; vApplySequential: `v(i % size)` for a fixed number of lookups, i ascending.
;; A structure that caches the chunk it last touched answers all but one in K
;; from cache; one that walks from the root every time does not.  sek has no
;; such cache on `ref` -- that is what its first-class iterators are for -- so
;; the last row of the table shows what the same walk costs through `in-pseq`.
(define-ext-scenario apply-sequential
  "index in ascending order, Scala's vApplySequential and immer's access_idx"
  (define sizes (sizes-m))
  (define k (scale 1000000))
  ;; A structure whose `ref` walks the sequence gets fewer reads, but they have
  ;; to stay spread over the whole range: twenty reads at indices 0 to 19 would
  ;; measure the head of a cons list rather than the list.
  (define (sweep rf s n [reads k])
    (define step (max 1 (quotient n reads)))
    (measure reads
             (lambda ()
               (let loop ([j 0] [i 0] [acc 0])
                 (cond [(= j reads) acc]
                       [(>= i n) (loop (add1 j) 0 (+ acc (rf s 0)))]
                       [else (loop (add1 j) (+ i step) (+ acc (rf s i)))])))))
  (table
   "apply-sequential: ref at i for i ascending, ns per lookup"
   sizes
   (rows-for
    sizes
    (lambda (i n) (sweep (impl-ref i) (build-of i n) n (cap i 'ref n k)))
    #:extra
    (list
     (cons "  vector"
           (for/list ([n (in-list sizes)])
             (sweep vector-ref (build-vector n values) n)))
     (cons "  pseq via iterator"
           (for/list ([n (in-list sizes)])
             (define s (build-pseq n values))
             (measure k
                      (lambda ()
                        (let loop ([j 0] [acc 0])
                          (if (>= j k)
                              acc
                              (loop (+ j n)
                                    (for/fold ([acc acc]) ([x (in-pseq s)])
                                      (+ acc x)))))))))))))

;; ------------------------------------------------------- Scala: element update

;; vUpdateSequential: a persistent `updated` at ascending indices, threading
;; the result.  Scala measures this separately from vUpdateRandom because a
;; sequential index walk is the case a tree can exploit and a random one is not.
(define-ext-scenario update-sequential
  "persistent set at ascending indices, Scala's vUpdateSequential, immer's assoc"
  (define sizes (sizes-m))
  (define m (scale 100000))
  (table
   "update-sequential: set at i for i ascending, ns per update"
   sizes
   (rows-for
    sizes
    (lambda (i n)
      (define st (impl-set i))
      (define writes (cap i 'set n m))
      (define step (max 1 (quotient n writes)))
      (and st
           (let ([s (build-of i n)])
             (measure writes
                      (lambda ()
                        (let loop ([j 0] [k 0] [s s])
                          (cond [(= j writes) s]
                                [(>= k n) (loop (add1 j) 0 (st s 0 0))]
                                [else (loop (add1 j) (+ k step) (st s k 0))])))))))
    #:extra
    (list
     (cons "  vector"
           (for/list ([n (in-list sizes)])
             (define v (build-vector n values))
             (measure m (lambda ()
                          (let loop ([j 0] [k 0])
                            (cond [(= j m) (void)]
                                  [(= k n) (vector-set! v 0 0) (loop (add1 j) 0)]
                                  [else (vector-set! v k 0) (loop (add1 j) (add1 k))]))))))))))

;; ------------------------------------------------------------ Scala: both ends

;; vApprepend: alternate `appended` and `prepended`.  An RRB tree pays for a
;; prepend what it pays for an append; a structure with a chunk at each end,
;; like sek, should not notice which end it is growing at.
(define-ext-scenario apprepend
  "alternate push-back and push-front, Scala's vApprepend"
  (define sizes (sizes-s))
  (define total (scale 1000000))
  (table
   "apprepend: push-back and push-front in turn, ns per push"
   sizes
   (rows-for
    sizes
    (lambda (i n)
      (define pb (impl-push-back i))
      (define pf (impl-push-front i))
      (and pb pf
           (if (or (linear? i 'push-back) (linear? i 'push-front))
               ;; growing to n one Theta(n) push at a time is quadratic: push a
               ;; bounded number onto a sequence already at length n, and pop
               ;; them off again so the next repetition starts where this did
               (let* ([m (max 1 (min 100 (quotient 1000000 (max n 1))))]
                      [qb (impl-pop-back i)]
                      [qf (impl-pop-front i)]
                      [base (build-of i n)])
                 (measure m
                          #:max-reps 20000
                          (lambda ()
                            (define up
                              (for/fold ([s base]) ([k (in-range m)])
                                (if (even? k) (pb s k) (pf s k))))
                            (for/fold ([s up]) ([k (in-range m)])
                              (if (even? k) (qf s) (qb s))))))
               (let* ([rounds (max 1 (quotient total n))]
                      [ops (* rounds n)])
                 (measure ops
                          (lambda ()
                            (let loop ([r 0])
                              (unless (= r rounds)
                                (let inner ([k 0] [s ((impl-empty i) n)])
                                  (if (= k n)
                                      (void)
                                      (inner (add1 k)
                                             (if (even? k) (pb s k) (pf s k)))))
                                (loop (add1 r)))))))))))))

;; vHead / vLast, then vTail: peeking at each end, and then walking the whole
;; sequence off the front one persistent pop at a time.
(define-ext-scenario ends
  "peek at each end, and repeated persistent tail, Scala's vHead/vLast/vTail"
  (define sizes (sizes-m))
  (define k (scale 1000000))
  ;; Scala calls `.head` and `.last`, not `apply(0)` and `apply(size-1)`, and
  ;; every structure here has dedicated accessors for its ends, so use them.
  (define (peek-row name build first last [linear? #f])
    (cons name
          (for/list ([n (in-list sizes)])
            (define s (build n))
            (define reads (if linear? (max 1 (min k (quotient 2000000 n))) k))
            (measure reads (lambda ()
                             (for/fold ([acc 0]) ([_ (in-range reads)])
                               (+ acc (first s) (last s))))))))
  (table
   "peek: first and last, ns per pair of reads"
   sizes
   (list
    (peek-row "eseq" (lambda (n) (build-eseq n values)) sek-first sek-last)
    (peek-row "pseq" (lambda (n) (build-pseq n values)) sek-first sek-last)
    (peek-row "treelist" (lambda (n) (sequence->treelist (in-range n)))
              treelist-first treelist-last)
    (peek-row "mutable-treelist" (lambda (n) (list->mutable-treelist (build-list n values)))
              (lambda (t) (mutable-treelist-ref t 0))
              (lambda (t) (mutable-treelist-ref t (sub1 (mutable-treelist-length t)))))
    (peek-row "gvector" (lambda (n) ((impl-build gvector-impl) (build-list n values)))
              (lambda (g) (gvector-ref g 0))
              (lambda (g) (gvector-ref g (sub1 (gvector-count g)))))
    (peek-row "list" (lambda (n) (build-list n values)) car last #t)
    (peek-row "box of list" (lambda (n) (box (build-list n values)))
              (lambda (b) (car (unbox b)))
              (lambda (b) (last (unbox b)))
              #t)
    (peek-row "array" (lambda (n) ((impl-build array-impl) (build-list n values)))
              (lambda (a) ((impl-ref array-impl) a 0))
              (lambda (a) ((impl-ref array-impl) a (sub1 (arr-n a)))))
    (peek-row "  vector" (lambda (n) (build-vector n values))
              (lambda (v) (vector-ref v 0))
              (lambda (v) (vector-ref v (sub1 (vector-length v)))))))
  (table
   "tail: pop the front until empty, ns per pop"
   sizes
   (rows-for
    sizes
    (lambda (i n)
      (define qf (impl-pop-front i))
      ;; The loop consumes the sequence, so an ephemeral structure pops a copy
      ;; of its own and is charged for making it.
      (define pops (cap i 'pop-front n n))
      (and qf
           (let ([s (build-of i n)])
             (measure pops
                      (lambda ()
                        (for/fold ([s (fresh-of i s)]) ([_ (in-range pops)])
                          (qf s))))))))))

;; ----------------------------------------------------------- Scala: subranges

;; vSlice: every slice on a 10% grid, so O(45) slices whatever the size.
(define-ext-scenario slice
  "all slices on a 10% grid, Scala's vSlice"
  (define sizes (sizes-m))
  (table
   "slice: sub-ranges on a 10% grid, ns per slice"
   sizes
   (rows-for
    sizes
    (lambda (i n)
      (define sp (impl-split i))
      (define inc (quotient n 10))
      (and sp (> inc 0)
           (let* ([s (build-of i n)]
                  [bounds (for*/list ([a (in-range 0 n inc)]
                                      [b (in-range (+ a inc) n inc)])
                            (cons a b))]
                  [count (length bounds)])
             (and (> count 0)
                  (measure count
                           (lambda ()
                             (for ([ab (in-list bounds)])
                               ;; take the prefix, then drop from it: the
                               ;; two-sided slice every library spells this way
                               (let-values ([(front _) (sp (fresh-of i s) (cdr ab))])
                                 (let-values ([(_ mid) (sp front (car ab))])
                                   (void mid)))))))))))))

;; ------------------------------------------------------- Scala: bulk appending

;; vBulkAppend2 / 10p / 100p / Same: `appendedAll` with a second argument of
;; two elements, a tenth of the receiver, all of it, and the receiver itself.
;; The last is the one that separates a structure that can share the argument
;; from one that must copy it.
(define-ext-scenario bulk-append
  "appendedAll with a small, large, and self-sized argument, Scala's vBulkAppend*"
  (define n (if (quick?) 100000 1000000))
  (define variants '("2" "n/10" "n" "self"))
  (table
   (format "bulk-append: append a sequence of the given size to one of ~a, ns per append" n)
   variants
   (append
    (for/list ([i (in-list all-impls)] #:when (impl-append i))
      (cons (impl-name i)
            (let ([s (build-of i n)]
                  [ap (impl-append i)])
              (for/list ([v (in-list variants)])
                (define m (case v [("2") 2] [("n/10") (quotient n 10)] [else n]))
                (define other (if (equal? v "self") s (build-of i m)))
                ;; a destructive append eats its left argument, so it gets a
                ;; copy -- and is charged for it
                (measure 1 #:max-reps 2000 (lambda () (ap (fresh-of i s) other)))))))
    (list
     (cons "  vector"
           (let ([v (build-vector n values)])
             (for/list ([w (in-list variants)])
               (define m (case w [("2") 2] [("n/10") (quotient n 10)] [else n]))
               (define other (if (equal? w "self") v (build-vector m values)))
               (measure 1 #:max-reps 2000 (lambda () (vector-append v other))))))))))

;; ---------------------------------------------------------- Scala: map, filter

;; vMapIdentity / vMapNew, then vFilter100p / 50p / 0p.  Scala measures three
;; filter ratios because a structure that can share unchanged runs wins at 100%
;; and a structure that allocates per surviving element wins at 0%.
(define-ext-scenario map
  "build a new sequence element by element, Scala's vMapNew"
  (define sizes (sizes-m))
  (define (rows f)
    (rows-for
     sizes
     (lambda (i n)
       (define mp (impl-map i))
       (and mp (let ([s (build-of i n)]) (measure n (lambda () (mp s f))))))
     #:extra
     (list
      (cons "  vector" (for/list ([n (in-list sizes)])
                         (define v (build-vector n values))
                         (measure n (lambda () (vector-map f v))))))))
  (table "map: apply a function to every element, ns per element" sizes (rows add1)))

(define-ext-scenario filter-ratio
  "filter keeping all, half, and none, Scala's vFilter100p/50p/0p"
  (define n (if (quick?) 100000 1000000))
  (define ratios '("100%" "50%" "0%"))
  (define (pred r)
    (case r
      [("100%") (lambda (x) #t)]
      [("50%") (lambda (x) (even? x))]
      [else (lambda (x) #f)]))
  (table
   (format "filter: keep the given fraction of ~a elements, ns per input element" n)
   ratios
   (append
    (for/list ([i (in-list all-impls)] #:when (impl-filter i))
      (cons (impl-name i)
            (let ([s (build-of i n)]
                  [fl (impl-filter i)])
              (for/list ([r (in-list ratios)])
                (measure n (lambda () (fl s (pred r))))))))
    (list
     (cons "  vector" (let ([v (build-vector n values)])
                        (for/list ([r (in-list ratios)])
                          (measure n (lambda () (vector-filter (pred r) v))))))))))

;; ---------------------------------------------------------- immer: take, drop

;; benchmark_take_lin / benchmark_drop_lin: shorten the sequence by a tenth,
;; then shorten *that*, and so on -- as opposed to taking every prefix of the
;; original, which never asks the structure to re-split its own output.  immer
;; pairs each with a `_mut` variant that does the same through a transient;
;; here that is eseq against pseq.
(define-ext-scenario take-drop
  "repeatedly shorten the result, immer's take_lin and drop_lin"
  (define sizes (sizes-m))
  (define steps 10)
  ;; immer pairs each of these with a `_mut` variant on a transient, which here
  ;; is just the ephemeral row: a destructive split gets a copy of its own, and
  ;; `eseq-copy` being O(1) is exactly what immer's `_mut` is claiming.
  (define (shrink title one-step vector-step)
    (table
     title sizes
     (rows-for
      sizes
      (lambda (i n)
        (define op (one-step i))
        (and op
             (let ([s (build-of i n)] [cut (quotient n steps)])
               (and (> cut 0)
                    (measure steps
                             (lambda ()
                               (for/fold ([s (fresh-of i s)]) ([_ (in-range (sub1 steps))])
                                 (op s cut))))))))
      #:extra
      (list
       (cons "  vector"
             (for/list ([n (in-list sizes)])
               (define v (build-vector n values))
               (define cut (quotient n steps))
               (and (> cut 0)
                    (measure steps
                             (lambda ()
                               (for/fold ([v v]) ([_ (in-range (sub1 steps))])
                                 (vector-step v cut)))))))))))
  (shrink "take-lin: drop a tenth off the back, ten times, ns per step"
          (lambda (i)
            (define tk (impl-take i))
            (define ln (impl-len i))
            (and tk ln (lambda (s cut) (tk s (- (ln s) cut)))))
          (lambda (v cut) (vector-copy v 0 (- (vector-length v) cut))))
  (shrink "drop-lin: drop a tenth off the front, ten times, ns per step"
          (lambda (i)
            (define dp (impl-drop i))
            (and dp (lambda (s cut) (dp s cut))))
          (lambda (v cut) (vector-copy v cut))))

;; ---------------------------------------------------------- immer: push_move

;; benchmark_push against benchmark_push_move: building by repeated persistent
;; push, against building through a transient and freezing at the end.  This is
;; the comparison immer's paper leads with, and the one Clojure spells
;; `(persistent! (reduce conj! (transient []) xs))`.
(define-ext-scenario push-move
  "persistent build against transient build, immer's push vs push_move"
  (define sizes (sizes-m))
  (define (row name build) (cons name (for/list ([n (in-list sizes)]) (build n))))
  (table
   "push vs push_move: build n elements, ns per element"
   sizes
   (list
    (row "pseq push-back"
         (lambda (n) (measure n (lambda ()
                                  (for/fold ([s empty-pseq]) ([k (in-range n)])
                                    (pseq-push-back s k))))))
    (row "eseq push!, snapshot"
         (lambda (n) (measure n (lambda ()
                                  (define e (make-eseq))
                                  (for ([k (in-range n)]) (eseq-push-back! e k))
                                  (eseq-snapshot e)))))
    (row "treelist-add"
         (lambda (n) (measure n (lambda ()
                                  (for/fold ([t empty-treelist]) ([k (in-range n)])
                                    (treelist-add t k))))))
    (row "mut-treelist, snapshot"
         (lambda (n) (measure n (lambda ()
                                  (define t (make-mutable-treelist 0))
                                  (for ([k (in-range n)]) (mutable-treelist-add! t k))
                                  (mutable-treelist-snapshot t)))))
    (row "gvector-add!"
         (lambda (n) (measure n (lambda ()
                                  (define g (make-gvector))
                                  (for ([k (in-range n)]) (gvector-add! g k))))))
    (row "array push-back"
         (lambda (n) (measure n (lambda ()
                                  (define a ((impl-empty array-impl) 8))
                                  (define pb (impl-push-back array-impl))
                                  (for ([k (in-range n)]) (pb a k))))))
    ;; the idiomatic way to build a list in order: cons at the front, reverse
    (row "list cons, reverse"
         (lambda (n) (measure n (lambda ()
                                  (reverse (for/fold ([l '()]) ([k (in-range n)])
                                             (cons k l)))))))
    (row "  vector fill"
         (lambda (n) (measure n (lambda ()
                                  (define v (make-vector n 0))
                                  (for ([k (in-range n)]) (vector-set! v k k)))))))))

;; ------------------------------------------------------- bifurcan: split-parts

;; ICollection.split(parts): cut the sequence into k roughly equal pieces and
;; hand each to a consumer, which is how bifurcan sets up a parallel fold.  The
;; measurement is the split plus one full traversal of every piece, so it says
;; what preparing for a parallel fold costs on top of a sequential one.
(define-ext-scenario split-parts
  "cut into k pieces and traverse each, bifurcan's ICollection.split"
  (define sizes (sizes-m))
  (define k 8)
  (table
   (format "split-parts: cut into ~a pieces and traverse all of them, ns per element" k)
   sizes
   (rows-for
    sizes
    (lambda (i n)
      (define sp (impl-split i))
      (define fe (impl-for-each i))
      (and sp fe (>= n k)
           (let ([s (build-of i n)] [cut (quotient n k)])
             ;; a destructive split consumes the sequence, so the ephemeral
             ;; rows cut up a copy of their own and are charged for making it
             (measure n
                      (lambda ()
                        (let loop ([s (fresh-of i s)] [j 0] [acc 0])
                          (cond
                            [(= j (sub1 k))
                             (fe s (lambda (x) (set! acc (+ acc 1))))
                             acc]
                            [else
                             (define-values (a b) (sp s cut))
                             (fe a (lambda (x) (set! acc (+ acc 1))))
                             (loop b (add1 j) acc)])))))))
    #:extra
    (list
     (cons "  vector"
           (for/list ([n (in-list sizes)])
             (define v (build-vector n values))
             (define cut (quotient n k))
             (measure n
                      (lambda ()
                        (for/fold ([acc 0]) ([j (in-range k)])
                          (define lo (* j cut))
                          (define hi (if (= j (sub1 k)) n (+ lo cut)))
                          (for/fold ([acc acc]) ([m (in-range lo hi)])
                            (+ acc (vector-ref v m))))))))))))

(module+ main
  (run-scenarios! "sek benchmarks, borrowed from other libraries"
                  (vector->list (current-command-line-arguments))
                  ext-scenarios ext-scenario-order))
