#lang racket/base
;; A benchmark suite for sek, modelled on the one that accompanies the OCaml
;; library (its stack, reach, iteration, traversal, construction, fill,
;; flatten and split benchmarks) and on Figures 17 and 18 of the paper.
;;
;;   racket -y bench/main.rkt              run everything
;;   racket -y bench/main.rkt stack queue  run named scenarios
;;   racket -y bench/main.rkt --quick      fewer operations
;;
;; Every measurement is reported in nanoseconds per operation, so numbers are
;; comparable down a column.  A dash means the structure does not support the
;; operation efficiently enough to be worth timing (a linear-time step inside
;; a loop that the others do in constant time).

(require racket/list
         racket/vector
         racket/treelist
         racket/mutable-treelist
         data/gvector
         "../sek/main.rkt")

;; ---------------------------------------------------------------- contenders

;; One record per data structure.  Mutable structures return themselves from
;; the update operations, so the same loop drives both flavours.
(struct impl (name kind
                   empty        ; nat -> s          (nat is a size hint)
                   build        ; list -> s
                   construct    ; nat proc -> s     (build n elements from scratch)
                   push-back    ; s x -> s
                   push-front   ; s x -> s
                   pop-back     ; s -> s
                   pop-front    ; s -> s
                   ref          ; s i -> v
                   set          ; s i v -> s
                   len          ; s -> nat
                   append       ; s1 s2 -> s
                   split        ; s i -> (values s1 s2)
                   for-each     ; s proc -> void
                   snapshot)    ; s -> immutable version, or #f
  #:transparent)

(define (mk name kind
            #:empty [empty #f] #:build [build #f] #:construct [construct #f]
            #:push-back [pb #f] #:push-front [pf #f]
            #:pop-back [qb #f] #:pop-front [qf #f]
            #:ref [rf #f] #:set [st #f] #:len [ln #f]
            #:append [ap #f] #:split [sp #f] #:for-each [fe #f]
            #:snapshot [sn #f])
  (impl name kind empty build construct pb pf qb qf rf st ln ap sp fe sn))

(define eseq-impl
  (mk "eseq" 'ephemeral
      #:empty (lambda (_) (make-eseq))
      #:build list->eseq
      #:construct build-eseq
      #:push-back (lambda (s x) (eseq-push-back! s x) s)
      #:push-front (lambda (s x) (eseq-push-front! s x) s)
      #:pop-back (lambda (s) (eseq-pop-back! s) s)
      #:pop-front (lambda (s) (eseq-pop-front! s) s)
      #:ref eseq-ref
      #:set (lambda (s i v) (eseq-set! s i v) s)
      #:len eseq-length
      #:append (lambda (a b) (eseq-append! a b) a)
      #:split (lambda (s i) (eseq-split! s i))
      #:for-each (lambda (s f) (sek-for-each s f))
      #:snapshot eseq-snapshot))

(define pseq-impl
  (mk "pseq" 'persistent
      #:empty (lambda (_) empty-pseq)
      #:build list->pseq
      #:construct build-pseq
      #:push-back pseq-push-back
      #:push-front pseq-push-front
      #:pop-back (lambda (s) (let-values ([(x s*) (pseq-pop-back s)]) s*))
      #:pop-front (lambda (s) (let-values ([(x s*) (pseq-pop-front s)]) s*))
      #:ref pseq-ref
      #:set pseq-set
      #:len pseq-length
      #:append pseq-append
      #:split pseq-split
      #:for-each (lambda (s f) (sek-for-each s f))))

(define treelist-impl
  (mk "treelist" 'persistent
      #:empty (lambda (_) empty-treelist)
      #:build list->treelist
      #:construct (lambda (n f) (for/fold ([t empty-treelist]) ([i (in-range n)]) (treelist-add t (f i))))
      #:push-back treelist-add
      #:push-front treelist-cons
      #:pop-back (lambda (s) (treelist-drop-right s 1))
      #:pop-front (lambda (s) (treelist-drop s 1))
      #:ref treelist-ref
      #:set treelist-set
      #:len treelist-length
      #:append treelist-append
      #:split (lambda (s i) (values (treelist-take s i) (treelist-drop s i)))
      #:for-each treelist-for-each))

(define mtreelist-impl
  (mk "mutable-treelist" 'ephemeral
      #:empty (lambda (_) (make-mutable-treelist 0))
      #:build list->mutable-treelist
      #:construct (lambda (n f)
                    (define t (make-mutable-treelist 0))
                    (for ([i (in-range n)]) (mutable-treelist-add! t (f i)))
                    t)
      #:push-back (lambda (s x) (mutable-treelist-add! s x) s)
      #:push-front (lambda (s x) (mutable-treelist-cons! s x) s)
      #:pop-back (lambda (s) (mutable-treelist-drop-right! s 1) s)
      #:pop-front (lambda (s) (mutable-treelist-drop! s 1) s)
      #:ref mutable-treelist-ref
      #:set (lambda (s i v) (mutable-treelist-set! s i v) s)
      #:len mutable-treelist-length
      #:append (lambda (a b) (mutable-treelist-append! a b) a)
      #:for-each mutable-treelist-for-each
      #:snapshot mutable-treelist-snapshot))

(define gvector-impl
  (mk "gvector" 'ephemeral
      #:empty (lambda (_) (make-gvector))
      #:build (lambda (xs) (define g (make-gvector)) (for ([x (in-list xs)]) (gvector-add! g x)) g)
      #:construct (lambda (n f)
                    (define g (make-gvector))
                    (for ([i (in-range n)]) (gvector-add! g (f i)))
                    g)
      #:push-back (lambda (s x) (gvector-add! s x) s)
      #:pop-back (lambda (s) (gvector-remove-last! s) s)
      #:ref gvector-ref
      #:set (lambda (s i v) (gvector-set! s i v) s)
      #:len gvector-count
      #:for-each (lambda (s f) (for ([x (in-gvector s)]) (f x)))))

(define list-impl
  (mk "list" 'persistent
      #:empty (lambda (_) '())
      #:build (lambda (xs) xs)
      #:construct build-list
      #:push-front (lambda (s x) (cons x s))
      #:pop-front cdr
      #:ref list-ref
      #:len length
      #:append append
      #:split (lambda (s i) (split-at s i))
      #:for-each (lambda (s f) (for-each f s))))

;; A mutable box holding an immutable list: the idiomatic Racket stack.
(define boxlist-impl
  (mk "box of list" 'ephemeral
      #:empty (lambda (_) (box '()))
      #:build (lambda (xs) (box xs))
      #:construct (lambda (n f) (box (build-list n f)))
      #:push-front (lambda (s x) (set-box! s (cons x (unbox s))) s)
      #:pop-front (lambda (s) (set-box! s (cdr (unbox s))) s)
      #:ref (lambda (s i) (list-ref (unbox s) i))
      #:len (lambda (s) (length (unbox s)))
      #:for-each (lambda (s f) (for-each f (unbox s)))))

(define all-impls
  (list eseq-impl pseq-impl treelist-impl mtreelist-impl gvector-impl
        list-impl boxlist-impl))

;; ------------------------------------------------------------------ harness

(define quick? (make-parameter #f))
(define (scale n) (if (quick?) (max 1 (quotient n 8)) n))

;; The two size ladders the scenarios use.  Quick mode drops the largest step,
;; which is what dominates both the running time and the memory pressure.
(define (sizes-s) (if (quick?) '(10 1000 100000) '(10 1000 100000 1000000)))
(define (sizes-m) (if (quick?) '(100 10000 100000) '(100 10000 1000000)))
(define (big-n) (if (quick?) 200000 1000000))

;; Run thunk until at least `target` milliseconds have elapsed, and return the
;; nanoseconds per operation, where one call to thunk performs `ops`.
;; Microbenchmarks on a shared machine are noisy in one direction only, so
;; report the best of a few trials rather than a single timing.
(define trials (make-parameter 3))

(define (measure ops thunk #:target [target 150.0] #:max-reps [max-reps +inf.0])
  (define (once reps)
    (collect-garbage)
    (define start (current-inexact-monotonic-milliseconds))
    (for ([_ (in-range reps)]) (thunk))
    (- (current-inexact-monotonic-milliseconds) start))
  ;; find a repetition count that runs long enough to time reliably
  (define reps
    (let loop ([reps 1])
      (define elapsed (once reps))
      (if (and (< elapsed target) (< reps 10000000) (< reps max-reps))
          (loop (* reps (max 2 (inexact->exact (ceiling (/ target (max elapsed 0.05)))))))
          reps)))
  (define best
    (for/fold ([best +inf.0]) ([_ (in-range (trials))])
      (min best (once reps))))
  (/ (* best 1e6) (* reps ops)))

(define (fmt x)
  (cond [(not x) "-"]
        [(>= x 1000) (real->decimal-string x 0)]
        [(>= x 100) (real->decimal-string x 1)]
        [else (real->decimal-string x 2)]))

(define (pad s w)
  (define str (format "~a" s))
  (string-append str (make-string (max 1 (- w (string-length str))) #\space)))

(define (rpad s w)
  (define str (format "~a" s))
  (string-append (make-string (max 1 (- w (string-length str))) #\space) str))

;; Print one table: rows are implementations, columns are sizes.
(define (table title sizes rows)
  (printf "\n~a\n" title)
  (printf "~a" (pad "" 24))
  (for ([n (in-list sizes)]) (printf "~a" (rpad n 12)))
  (newline)
  (for ([row (in-list rows)])
    (printf "~a" (pad (car row) 24))
    (for ([v (in-list (cdr row))]) (printf "~a" (rpad (fmt v) 12)))
    (newline)))

;; --------------------------------------------------------------- scenarios

(define scenarios (make-hash))
(define scenario-order '())
(define-syntax-rule (define-scenario name doc body ...)
  (begin (hash-set! scenarios 'name (lambda () body ...))
         (set! scenario-order (cons (cons 'name doc) scenario-order))))

(define (build-of i n)
  ((impl-build i) (build-list n values)))

;; Figures 17 and 18: repeat "n pushes then n pops" until `total` pushes have
;; happened.  The peak length is n, so this measures how the cost of a stack
;; operation varies with the length of the stack.
(define (stack-scenario title push pop sizes total)
  (table
   title sizes
   (for/list ([i (in-list all-impls)])
     (cons (impl-name i)
           (for/list ([n (in-list sizes)])
             (define pushf (push i))
             (define popf (pop i))
             (and pushf popf
                  (let* ([rounds (max 1 (quotient total n))]
                         [ops (* 2 rounds n)])
                    (measure ops
                             (lambda ()
                               (let loop ([r 0] [s ((impl-empty i) n)])
                                 (unless (= r rounds)
                                   (define s1
                                     (for/fold ([s s]) ([k (in-range n)]) (pushf s k)))
                                   (define s2
                                     (for/fold ([s s1]) ([k (in-range n)]) (popf s)))
                                   (loop (add1 r) s2))))))))))))

(define-scenario stack
  "push and pop at the back (the paper's Figures 17 and 18)"
  (stack-scenario "stack: push-back / pop-back, ns per operation"
                  impl-push-back impl-pop-back
                  (sizes-s) (scale 2000000)))

(define-scenario front-stack
  "push and pop at the front"
  (stack-scenario "front stack: push-front / pop-front, ns per operation"
                  impl-push-front impl-pop-front
                  (sizes-s) (scale 2000000)))

(define-scenario queue
  "push at the back, pop at the front"
  (table
   "queue: push-back / pop-front, ns per operation"
   (sizes-s)
   (for/list ([i (in-list all-impls)])
     (cons (impl-name i)
           (for/list ([n (in-list (sizes-s))])
             (define pushf (impl-push-back i))
             (define popf (impl-pop-front i))
             (and pushf popf
                  (let* ([total (scale 2000000)]
                         [rounds (max 1 (quotient total n))]
                         [ops (* 2 rounds n)])
                    (measure ops
                             (lambda ()
                               (let loop ([r 0] [s ((impl-empty i) n)])
                                 (unless (= r rounds)
                                   (define s1
                                     (for/fold ([s s]) ([k (in-range n)]) (pushf s k)))
                                   (define s2
                                     (for/fold ([s s1]) ([k (in-range n)]) (popf s)))
                                   (loop (add1 r) s2))))))))))))

(define-scenario traversal
  "visit every element in order"
  (define sizes (sizes-m))
  (table
   "traversal: for-each over the whole sequence, ns per element"
   sizes
   (append
    (for/list ([i (in-list all-impls)])
      (cons (impl-name i)
            (for/list ([n (in-list sizes)])
              (define fe (impl-for-each i))
              (and fe
                   (let ([s (build-of i n)] [acc 0])
                     (measure n (lambda () (fe s (lambda (x) (set! acc (+ acc x)))))))))))
    ;; the two ways sek can be swept, to show what segments buy
    (list
     (cons "  pseq via fold"
           (for/list ([n (in-list sizes)])
             (define s (list->pseq (build-list n values)))
             (measure n (lambda () (sek-fold-left s + 0)))))
     (cons "  pseq via iter"
           (for/list ([n (in-list sizes)])
             (define s (list->pseq (build-list n values)))
             (measure n
                      (lambda ()
                        (define it (sek-iterator s 'forward))
                        (let loop ([acc 0])
                          (if (sek-iter-finished? it)
                              acc
                              (loop (+ acc (sek-iter-get-and-move! it 'forward)))))))))
     (cons "  vector"
           (for/list ([n (in-list sizes)])
             (define v (build-vector n values))
             (measure n (lambda () (for/fold ([acc 0]) ([x (in-vector v)]) (+ acc x))))))))))

(define-scenario random-access
  "read at uniformly random indices"
  (define sizes (sizes-m))
  (define k 100000)
  (table
   "random access: ref at random indices, ns per read"
   sizes
   (append
    (for/list ([i (in-list all-impls)])
      (cons (impl-name i)
            (for/list ([n (in-list sizes)])
              (define rf (impl-ref i))
              ;; a list is O(n) per read; timing it would say nothing new
              (and rf (not (member (impl-name i) '("list" "box of list")))
                   (let ([s (build-of i n)]
                         [ix (build-vector (scale k) (lambda (_) (random n)))])
                     (measure (vector-length ix)
                              (lambda ()
                                (for/fold ([acc 0]) ([j (in-vector ix)])
                                  (+ acc (rf s j))))))))))
    (list
     (cons "  vector"
           (for/list ([n (in-list sizes)])
             (define v (build-vector n values))
             (define ix (build-vector (scale k) (lambda (_) (random n))))
             (measure (vector-length ix)
                      (lambda ()
                        (for/fold ([acc 0]) ([j (in-vector ix)]) (+ acc (vector-ref v j)))))))))))

(define-scenario hops
  "read at a fixed stride, the reference's `reach` benchmark"
  (define n (big-n))
  (define k (scale 100000))
  (define strides '(1 8 64 4096))
  (define (destinations stride)
    (define v (make-vector k 0))
    (let loop ([i 0] [pos (random n)])
      (unless (= i k)
        (vector-set! v i pos)
        (loop (add1 i) (modulo (+ pos stride) n))))
    v)
  (define ixs (for/list ([d (in-list strides)]) (destinations d)))
  (table
   (format "hops over ~a elements: ref at a fixed stride, ns per read" n)
   (map (lambda (d) (format "+~a" d)) strides)
   (append
    (for/list ([i (in-list (list eseq-impl pseq-impl treelist-impl mtreelist-impl gvector-impl))])
      (cons (impl-name i)
            (for/list ([ix (in-list ixs)])
              (define rf (impl-ref i))
              (define s (build-of i n))
              (measure k (lambda () (for/fold ([acc 0]) ([j (in-vector ix)]) (+ acc (rf s j))))))))
    (list
     ;; an iterator remembers where it is, so a short hop stays in one chunk
     (cons "pseq iterator"
           (for/list ([ix (in-list ixs)])
             (define s (list->pseq (build-list n values)))
             (define it (sek-iterator s 'forward))
             (measure k
                      (lambda ()
                        (for/fold ([acc 0]) ([j (in-vector ix)])
                          (sek-iter-reach! it j)
                          (+ acc (sek-iter-get it)))))))))))

(define-scenario update
  "write at uniformly random indices"
  (define sizes (sizes-m))
  (define k (scale 100000))
  (table
   "update: set at random indices, ns per write"
   sizes
   (append
    (for/list ([i (in-list (list eseq-impl pseq-impl treelist-impl mtreelist-impl gvector-impl))])
      (cons (impl-name i)
            (for/list ([n (in-list sizes)])
              (define st (impl-set i))
              (and st
                   (let ([s0 (build-of i n)]
                         [ix (build-vector k (lambda (_) (random n)))])
                     (measure k
                              (lambda ()
                                (for/fold ([s s0]) ([j (in-vector ix)]) (st s j 0)))))))))
    (list
     (cons "  vector"
           (for/list ([n (in-list sizes)])
             (define v (build-vector n values))
             (define ix (build-vector k (lambda (_) (random n))))
             (measure k
                      (lambda ()
                        (for ([j (in-vector ix)]) (vector-set! v j 0))))))))))

(define-scenario construction
  "build a sequence of n elements"
  (define sizes (sizes-m))
  (table
   "construction: n elements from scratch, ns per element"
   sizes
   (append
    (for/list ([i (in-list all-impls)])
      (cons (impl-name i)
            (for/list ([n (in-list sizes)])
              (define c (impl-construct i))
              (and c (measure n (lambda () (c n values)) #:max-reps 6)))))
    (list
     (cons "  vector"
           (for/list ([n (in-list sizes)])
             (measure n (lambda () (build-vector n values)))))))))

(define-scenario concat
  "concatenate two sequences repeatedly"
  (define sizes (sizes-m))
  (table
   "concat: append two sequences of n/2 elements, ns per concatenation"
   sizes
   (for/list ([i (in-list (list pseq-impl treelist-impl list-impl))])
     (cons (impl-name i)
           (for/list ([n (in-list sizes)])
             (define ap (impl-append i))
             (define a (build-of i (quotient n 2)))
             (define b (build-of i (quotient n 2)))
             (and ap (measure 1 (lambda () (ap a b)) #:max-reps 2000)))))))

(define-scenario split
  "split a sequence at a random index"
  (define sizes (sizes-m))
  (table
   "split: split at a random index, ns per split"
   sizes
   (for/list ([i (in-list (list pseq-impl treelist-impl list-impl))])
     (cons (impl-name i)
           (for/list ([n (in-list sizes)])
             (define sp (impl-split i))
             (define s (build-of i n))
             (define ix (build-vector (if (> n 50000) 100 1000) (lambda (_) (random n))))
             (and sp
                  (measure (vector-length ix)
                           (lambda ()
                             (for ([j (in-vector ix)])
                               (call-with-values (lambda () (sp s j)) void))))))))))

(define-scenario snapshot
  "taking a persistent snapshot of a sequence being built in place"
  (define sizes (sizes-m))
  ;; What one snapshot costs, after a change, as the sequence grows.  This is
  ;; the operation the paper's ownership identifiers make free: the answer
  ;; should not depend on n.
  (table
   "snapshot: one change plus one snapshot of an n-element sequence, ns"
   sizes
   (list
    (cons "eseq"
          (for/list ([n (in-list sizes)])
            (define e (list->eseq (build-list n values)))
            (define keep #f)
            (measure 1 (lambda ()
                         (eseq-set! e 0 1)
                         (set! keep (eseq-snapshot e))))))
    (cons "mutable-treelist"
          (for/list ([n (in-list sizes)])
            (define t (list->mutable-treelist (build-list n values)))
            (define keep #f)
            (measure 1 #:max-reps 2000
                     (lambda ()
                       (mutable-treelist-set! t 0 1)
                       (set! keep (mutable-treelist-snapshot t))))))))
  ;; And the scenario that motivates the whole design: build a sequence in
  ;; place while keeping a persistent snapshot every m pushes.
  (define n (scale 100000))
  (define periods '(1 10 1000 100000))
  (define (affordable? m) (<= (quotient n m) 2000))
  (table
   (format "snapshot: ~a pushes, keeping a snapshot every m of them, ns per push" n)
   (map (lambda (m) (format "m=~a" m)) periods)
   (list
    (cons "eseq"
          (for/list ([m (in-list periods)])
            (measure n #:max-reps 8
                     (lambda ()
                       (define e (make-eseq))
                       (define keep '())
                       (for ([k (in-range n)])
                         (eseq-push-back! e k)
                         (when (zero? (modulo k m))
                           (set! keep (cons (eseq-snapshot e) keep))))
                       keep))))
    (cons "mutable-treelist"
          (for/list ([m (in-list periods)])
            ;; its snapshots are copies, so keeping many of them is quadratic
            (and (affordable? m)
                 (measure n #:max-reps 8
                          (lambda ()
                            (define e (make-mutable-treelist 0))
                            (define keep '())
                            (for ([k (in-range n)])
                              (mutable-treelist-add! e k)
                              (when (zero? (modulo k m))
                                (set! keep (cons (mutable-treelist-snapshot e) keep))))
                            keep)))))
    (cons "pseq (persistent)"
          (for/list ([m (in-list periods)])
            (measure n #:max-reps 8
                     (lambda ()
                       (for/fold ([s empty-pseq]) ([k (in-range n)])
                         (pseq-push-back s k))))))
    (cons "treelist (persistent)"
          (for/list ([m (in-list periods)])
            (measure n #:max-reps 8
                     (lambda ()
                       (for/fold ([s empty-treelist]) ([k (in-range n)])
                         (treelist-add s k)))))))))

(define-scenario fill
  "overwrite a range of elements"
  (define n (big-n))
  (define sizes (if (quick?) '(10 1000) '(10 1000 100000)))
  (table
   (format "fill: overwrite k consecutive elements of ~a, ns per element" n)
   sizes
   (list
    (cons "eseq (sek-fill!)"
          (for/list ([k (in-list sizes)])
            (define e (list->eseq (build-list n values)))
            (measure k (lambda () (sek-fill! e 0 k 0)))))
    (cons "eseq (set! loop)"
          (for/list ([k (in-list sizes)])
            (define e (list->eseq (build-list n values)))
            (measure k (lambda () (for ([j (in-range k)]) (eseq-set! e j 0))))))
    (cons "mutable-treelist"
          (for/list ([k (in-list sizes)])
            (define t (list->mutable-treelist (build-list n values)))
            (measure k (lambda () (for ([j (in-range k)]) (mutable-treelist-set! t j 0))))))
    (cons "gvector"
          (for/list ([k (in-list sizes)])
            (define g ((impl-build gvector-impl) (build-list n values)))
            (measure k (lambda () (for ([j (in-range k)]) (gvector-set! g j 0))))))
    (cons "vector"
          (for/list ([k (in-list sizes)])
            (define v (build-vector n values))
            (measure k (lambda () (for ([j (in-range k)]) (vector-set! v j 0)))))))))

;; ------------------------------------------------------------------- driver

(define-scenario capacities
  "how the tunable chunk capacities trade one operation against another"
  (define n (big-n))
  (define k (scale 50000))
  (define configs (list (cons 128 16) (cons 64 16) (cons 32 16)
                        (cons 16 16) (cons 8 8) (cons 256 32)))
  (define (with-config c thunk)
    (sek-configure! #:leaf-capacity (car c) #:node-capacity (cdr c)
                    #:short-threshold (min 32 (car c)))
    (begin0 (thunk)
            (sek-configure! #:leaf-capacity 128 #:node-capacity 16
                            #:short-threshold 32)))
  (define ix (build-vector k (lambda (_) (random n))))
  (table
   (format "capacities over ~a elements: leaf/node capacity, ns per operation" n)
   (map (lambda (c) (format "~a/~a" (car c) (cdr c))) configs)
   (list
    (cons "pseq-ref"
          (for/list ([c (in-list configs)])
            (with-config c
              (lambda ()
                (define s (build-pseq n values))
                (measure k (lambda ()
                             (for/fold ([acc 0]) ([j (in-vector ix)])
                               (+ acc (pseq-ref s j)))))))))
    (cons "pseq-set"
          (for/list ([c (in-list configs)])
            (with-config c
              (lambda ()
                (define s0 (build-pseq n values))
                (measure k (lambda ()
                             (for/fold ([s s0]) ([j (in-vector ix)])
                               (pseq-set s j 0))))))))
    (cons "eseq push/pop"
          (for/list ([c (in-list configs)])
            (with-config c
              (lambda ()
                (measure (* 2 n)
                         (lambda ()
                           (define e (make-eseq))
                           (for ([j (in-range n)]) (eseq-push-back! e j))
                           (for ([_ (in-range n)]) (eseq-pop-back! e))))))))
    (cons "traversal"
          (for/list ([c (in-list configs)])
            (with-config c
              (lambda ()
                (define s (build-pseq n values))
                (measure n (lambda () (sek-fold-left s + 0))))))))))

(define-scenario transient
  "the round trip: edit a persistent sequence, update it in place, snapshot"
  (define n (big-n))
  (define counts '(1 10 1000 100000))
  (define ix (build-vector 100000 (lambda (_) (random n))))
  (define p (build-pseq n values))
  (define tl (sequence->treelist (in-range n)))
  ;; Racket's treelist has the same pair of conversions: treelist-copy is its
  ;; edit, mutable-treelist-snapshot its snapshot.
  (table
   (format "transient: edit, m in-place updates, snapshot, over ~a elements, ns per update" n)
   (map (lambda (m) (format "m=~a" m)) counts)
   (list
    (cons "sek edit/snapshot"
          (for/list ([m (in-list counts)])
            (measure m #:max-reps 200
                     (lambda ()
                       (define e (pseq-edit p))
                       (for ([k (in-range m)])
                         (eseq-set! e (vector-ref ix (modulo k 100000)) 0))
                       (eseq-snapshot e)))))
    (cons "treelist copy/snapshot"
          (for/list ([m (in-list counts)])
            (measure m #:max-reps 200
                     (lambda ()
                       (define t (treelist-copy tl))
                       (for ([k (in-range m)])
                         (mutable-treelist-set! t (vector-ref ix (modulo k 100000)) 0))
                       (mutable-treelist-snapshot t)))))
    (cons "sek persistent set"
          (for/list ([m (in-list counts)])
            (measure m #:max-reps 200
                     (lambda ()
                       (for/fold ([s p]) ([k (in-range m)])
                         (pseq-set s (vector-ref ix (modulo k 100000)) 0))))))
    (cons "treelist persistent set"
          (for/list ([m (in-list counts)])
            (measure m #:max-reps 200
                     (lambda ()
                       (for/fold ([t tl]) ([k (in-range m)])
                         (treelist-set t (vector-ref ix (modulo k 100000)) 0)))))))))

(define-scenario filter
  "the paper's motivating example: filter a persistent sequence"
  (define sizes (sizes-m))
  (define (keep? x) (zero? (modulo x 3)))
  (table
   "filter: keep one element in three, ns per input element"
   sizes
   (list
    (cons "pseq"
          (for/list ([n (in-list sizes)])
            (define s (build-pseq n values))
            (measure n (lambda () (sek-filter s keep?)))))
    (cons "eseq"
          (for/list ([n (in-list sizes)])
            (define s (build-eseq n values))
            (measure n (lambda () (sek-filter s keep?)))))
    (cons "treelist"
          (for/list ([n (in-list sizes)])
            (define t (sequence->treelist (in-range n)))
            (measure n (lambda () (treelist-filter keep? t)))))
    (cons "  list"
          (for/list ([n (in-list sizes)])
            (define l (build-list n values))
            (measure n (lambda () (filter keep? l)))))
    (cons "  vector"
          (for/list ([n (in-list sizes)])
            (define v (build-vector n values))
            (measure n (lambda () (vector-filter keep? v))))))))

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (when (member "--quick" args) (quick? #t))
  (when (member "--careful" args) (trials 7))
  (define named (filter (lambda (a) (not (regexp-match? #rx"^--" a))) args))
  (define chosen
    (if (null? named)
        (map car (reverse scenario-order))
        (map string->symbol named)))
  (printf "sek benchmarks -- Racket ~a~a\n" (version) (if (quick?) " (quick)" ""))
  (for ([name (in-list chosen)])
    (define run (hash-ref scenarios name #f))
    (cond
      [run (collect-garbage) (run)]
      [else (printf "\nno such scenario: ~a\navailable: ~a\n"
                    name (map car (reverse scenario-order)))])))
