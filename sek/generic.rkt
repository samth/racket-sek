#lang racket/base
;; The derived operations of the OCaml library's SEK signature.
;;
;; Everything here is written once, over iterators, and works on either
;; flavour of sequence.  Operations that build a sequence return the same
;; flavour as the sequence they were given, which is how the OCaml library's
;; two parallel modules are collapsed into one set of names.
;;
;; Traversal goes through segments rather than one element at a time: an
;; iterator hands out the whole contiguous run it is sitting on, and the run
;; is processed with a tight vector loop.  That is what keeps `fold` and
;; friends close to the speed of the same loop over an array.

(require (for-syntax racket/base)
         (only-in racket/unsafe/ops
                  unsafe-vector*-ref unsafe-fx+ unsafe-fx< unsafe-fx>)
         racket/vector
         "config.rkt"
         "persistent.rkt"
         "ephemeral.rkt"
         "iterator.rkt"
         "segment.rkt")

(provide sek?
         sek-length
         sek-empty?
         sek-ref
         sek-first
         sek-last
         in-sek
         in-pseq
         in-eseq
         sek-segments-for-each
         sek-for-each
         sek-for-each/index
         sek-fold-left
         sek-fold-right
         sek-find
         sek-find-index
         sek-find-map
         sek-for-all?
         sek-exists?
         sek-member?
         sek-memq?
         sek->list
         sek->vector
         sek-equal?
         sek-compare
         sek-segments-for-each2
         sek-for-each2
         sek-fold-left2
         sek-fold-right2
         sek-for-all2?
         sek-exists2?
         sek-map
         sek-map/index
         sek-map2
         sek-filter
         sek-filter-map
         sek-partition
         sek-reverse
         sek-zip
         sek-unzip
         sek-append*
         sek-append-map
         sek-sort
         sek-uniq
         sek-merge
         sek-sub
         sek-take
         sek-drop
         sek-copy
         sek-fill!
         sek-blit!
         make-pseq
         build-pseq
         build-eseq
         sequence->pseq
         sequence->eseq
         for/eseq
         for*/eseq
         for/pseq
         for*/pseq)

(define (sek? v)
  (or (pseq? v) (eseq? v)))

(define (check-sek who v)
  (unless (sek? v)
    (raise-argument-error who "(or/c pseq? eseq?)" v))
  v)

(define (sek-length s)
  (if (pseq? s)
      (pseq-length s)
      (eseq-length s)))

(define (sek-empty? s)
  (eqv? 0 (sek-length s)))

(define (sek-ref s i)
  (if (pseq? s)
      (pseq-ref s i)
      (eseq-ref s i)))

(define (sek-first s)
  (if (pseq? s)
      (pseq-first s)
      (eseq-first s)))
(define (sek-last s)
  (if (pseq? s)
      (pseq-last s)
      (eseq-last s)))

;; ------------------------------------------------------------- builders

;; Results are accumulated in an ephemeral sequence -- push at the back is the
;; cheapest thing this structure does -- and frozen at the end if the caller
;; started from a persistent sequence.
(define (open-builder)
  (make-eseq))
(define (close-builder b like)
  (if (pseq? like)
      (eseq-snapshot-and-clear! b)
      b))

(define (build-from like xs-thunk)
  (define b (open-builder))
  (xs-thunk (lambda (x) (eseq-push-back! b x)))
  (close-builder b like))

;; ------------------------------------------------------------- traversal

;; Hand each maximal run of contiguous elements to proc, as a segment.
(define (sek-segments-for-each s proc [dir 'forward])
  (check-sek 'sek-segments-for-each s)
  (define it (sek-iterator s dir))
  (let loop ()
    (unless (sek-iter-finished? it)
      (proc (sek-iter-segment-and-jump! it dir))
      (loop))))

(define (sek-for-each s proc [dir 'forward])
  (check-sek 'sek-for-each s)
  (sek-segments-for-each s (lambda (sg) (segment-for-each sg proc dir)) dir))

(define (sek-for-each/index s proc [dir 'forward])
  (check-sek 'sek-for-each/index s)
  (define it (sek-iterator s dir))
  (let loop ()
    (unless (sek-iter-finished? it)
      (define i (sek-iter-index it))
      (proc i (sek-iter-get it))
      (sek-iter-move! it dir)
      (loop))))

(define (sek-fold-left s proc init)
  (check-sek 'sek-fold-left s)
  (define it (sek-iterator s 'forward))
  (let loop ([acc init])
    (cond
      [(sek-iter-finished? it) acc]
      [else
       (define sg (sek-iter-segment-and-jump! it 'forward))
       (define v (segment-vector sg))
       (define o (segment-start sg))
       (loop (for/fold ([acc acc]) ([j (in-range (segment-length sg))])
               (proc acc (vector-ref v (+ o j)))))])))

(define (sek-fold-right s proc init)
  (check-sek 'sek-fold-right s)
  (define it (sek-iterator s 'backward))
  (let loop ([acc init])
    (cond
      [(sek-iter-finished? it) acc]
      [else
       (define sg (sek-iter-segment-and-jump! it 'backward))
       (define v (segment-vector sg))
       (define o (segment-start sg))
       (loop (for/fold ([acc acc]) ([j (in-range (sub1 (segment-length sg)) -1 -1)])
               (proc (vector-ref v (+ o j)) acc)))])))

;; The generic-dispatch version, used when a sequence value is passed around
;; rather than written directly in a for clause.
(define (in-sek/proc s [dir 'forward])
  (check-sek 'in-sek s)
  (make-do-sequence (lambda ()
                      (define it (sek-iterator s dir))
                      (values (lambda (_) (sek-iter-get it))
                              (lambda (_)
                                (sek-iter-move! it dir)
                                #f)
                              #f
                              (lambda (_) (not (sek-iter-finished? it)))
                              #f
                              #f))))

;; Refill the loop state from the iterator's next run of contiguous storage.
;; Returns the vector, the position to read, the limit, and whether there is
;; anything left.
(define (sek-loop-refill it dir)
  (define sg (sek-iter-segment-and-jump*! it dir))
  (if sg
      (let ([o (segment-start sg)] [k (segment-length sg)])
        (if (eq? dir 'forward)
            (values (segment-vector sg) o (+ o k) #t)
            ;; going backward the run is read from its far end inwards
            (values (segment-vector sg) (+ o k -1) (- o 1) #t)))
      (values #f 0 0 #f)))

;; In a for clause these expand to a loop over the underlying storage, so the
;; common step is a vector reference and an increment -- the same shape the
;; segment-based traversals use, rather than one iterator call per element.
;;
;; The four loop variables are the run's vector, the position to read, the
;; limit, and the step (+1 forward, -1 backward).  All the work happens in one
;; inner binding, because :do-in binds its inner clauses in parallel.
(begin-for-syntax
  (define (sek-for-clause stx)
    (define (expand seq-expr dir-expr id)
      (with-syntax ([seq-expr seq-expr] [dir-expr dir-expr] [id id])
        #'[(id)
           (:do-in
            ([(it dir)
              (let ([d dir-expr]) (values (sek-iterator seq-expr d) d))])
            #t
            ([v #f] [i 0] [n 0] [step 1])
            #t
            ([(v* i* n* step* id)
              (if (if (unsafe-fx< step 0) (unsafe-fx> i n) (unsafe-fx< i n))
                  (values v i n step (unsafe-vector*-ref v i))
                  (let-values ([(v2 i2 n2 ok?) (sek-loop-refill it dir)])
                    (if ok?
                        (values v2 i2 n2
                                (if (eq? dir 'forward) 1 -1)
                                (unsafe-vector*-ref v2 i2))
                        (values #f 0 0 1 #f))))])
            (if (unsafe-fx< step* 0) (unsafe-fx> i* n*) (unsafe-fx< i* n*))
            #t
            (v* (unsafe-fx+ i* step*) n* step*))]))
    (syntax-case stx ()
      [[(id) (_ seq-expr)] (expand #'seq-expr #''forward #'id)]
      [[(id) (_ seq-expr dir-expr)] (expand #'seq-expr #'dir-expr #'id)]
      [_ #f])))

(define-sequence-syntax in-sek
  (lambda () #'in-sek/proc)
  sek-for-clause)

;; The same loop, restricted to one flavour.  These shadow the plain readers
;; that persistent.rkt and ephemeral.rkt define.
(define (in-pseq/proc s)
  (unless (pseq? s) (raise-argument-error 'in-pseq "pseq?" s))
  (in-sek/proc s 'forward))

(define (in-eseq/proc e)
  (unless (eseq? e) (raise-argument-error 'in-eseq "eseq?" e))
  (in-sek/proc e 'forward))

(define-sequence-syntax in-pseq
  (lambda () #'in-pseq/proc)
  sek-for-clause)

(define-sequence-syntax in-eseq
  (lambda () #'in-eseq/proc)
  sek-for-clause)

(define (sek->list s [dir 'forward])
  (check-sek 'sek->list s)
  (define acc '())
  (sek-for-each s (lambda (x) (set! acc (cons x acc))) (if (eq? dir 'forward) 'backward 'forward))
  acc)

(define (sek->vector s)
  (check-sek 'sek->vector s)
  (define v (make-vector (sek-length s) #f))
  (define i 0)
  (sek-for-each s
                (lambda (x)
                  (vector-set! v i x)
                  (set! i (add1 i))))
  v)

;; ------------------------------------------------------------- searching

;; The first element satisfying pred, or #f.  Use sek-find-index when an
;; element could itself be #f.
(define (sek-find s pred [dir 'forward])
  (check-sek 'sek-find s)
  (define it (sek-iterator s dir))
  (let loop ()
    (cond
      [(sek-iter-finished? it) #f]
      [else
       (define x (sek-iter-get it))
       (cond
         [(pred x) x]
         [else
          (sek-iter-move! it dir)
          (loop)])])))

;; The index of the first (or last) element satisfying pred, or #f.
(define (sek-find-index s pred [dir 'forward])
  (check-sek 'sek-find-index s)
  (define it (sek-iterator s dir))
  (let loop ()
    (cond
      [(sek-iter-finished? it) #f]
      [(pred (sek-iter-get it)) (sek-iter-index it)]
      [else
       (sek-iter-move! it dir)
       (loop)])))

(define (sek-find-map s proc [dir 'forward])
  (check-sek 'sek-find-map s)
  (define it (sek-iterator s dir))
  (let loop ()
    (cond
      [(sek-iter-finished? it) #f]
      [else
       (define r (proc (sek-iter-get it)))
       (cond
         [r r]
         [else
          (sek-iter-move! it dir)
          (loop)])])))

(define (sek-for-all? s pred)
  (check-sek 'sek-for-all? s)
  (define it (sek-iterator s 'forward))
  (let loop ()
    (cond
      [(sek-iter-finished? it) #t]
      [(pred (sek-iter-get it))
       (sek-iter-move! it 'forward)
       (loop)]
      [else #f])))

(define (sek-exists? s pred)
  (check-sek 'sek-exists? s)
  (define it (sek-iterator s 'forward))
  (let loop ()
    (cond
      [(sek-iter-finished? it) #f]
      [(pred (sek-iter-get it)) #t]
      [else
       (sek-iter-move! it 'forward)
       (loop)])))

(define (sek-member? x s [same? equal?])
  (sek-exists? s (lambda (y) (same? x y))))
(define (sek-memq? x s)
  (sek-exists? s (lambda (y) (eq? x y))))

;; --------------------------------------------------------- binary traversal

;; Hand matching runs of the two sequences to proc, as a pair of segments of
;; equal length -- the fast path for a binary loop.
(define (sek-segments-for-each2 s1 s2 proc [dir 'forward])
  (check-sek 'sek-segments-for-each2 s1)
  (check-sek 'sek-segments-for-each2 s2)
  (define it1 (sek-iterator s1 dir))
  (define it2 (sek-iterator s2 dir))
  (define (cut sg k)
    ;; going backward, a segment ends at the cursor, so trim it at the front
    (if (eq? dir 'forward)
        (segment (segment-vector sg) (segment-start sg) k)
        (segment (segment-vector sg)
                 (- (+ (segment-start sg) (segment-length sg)) k)
                 k)))
  (let loop ()
    (unless (or (sek-iter-finished? it1) (sek-iter-finished? it2))
      (define a (sek-iter-segment it1 dir))
      (define b (sek-iter-segment it2 dir))
      (define k (min (segment-length a) (segment-length b)))
      (proc (cut a k) (cut b k))
      (sek-iter-jump! it1 dir k)
      (sek-iter-jump! it2 dir k)
      (loop))))

(define (sek-for-each2 s1 s2 proc [dir 'forward])
  (check-sek 'sek-for-each2 s1)
  (check-sek 'sek-for-each2 s2)
  (define it1 (sek-iterator s1 dir))
  (define it2 (sek-iterator s2 dir))
  (let loop ()
    (unless (or (sek-iter-finished? it1) (sek-iter-finished? it2))
      (proc (sek-iter-get it1) (sek-iter-get it2))
      (sek-iter-move! it1 dir)
      (sek-iter-move! it2 dir)
      (loop))))

(define (sek-fold-left2 s1 s2 proc init)
  (define it1 (sek-iterator s1 'forward))
  (define it2 (sek-iterator s2 'forward))
  (let loop ([acc init])
    (if (or (sek-iter-finished? it1) (sek-iter-finished? it2))
        acc
        (let ([acc (proc acc (sek-iter-get it1) (sek-iter-get it2))])
          (sek-iter-move! it1 'forward)
          (sek-iter-move! it2 'forward)
          (loop acc)))))

(define (sek-fold-right2 s1 s2 proc init)
  (define n (min (sek-length s1) (sek-length s2)))
  (define it1 (sek-iterator s1 'backward))
  (define it2 (sek-iterator s2 'backward))
  ;; align the two cursors on the last n elements of each
  (sek-iter-reach! it1 (sub1 n))
  (sek-iter-reach! it2 (sub1 n))
  (let loop ([acc init]
             [k n])
    (if (eqv? k 0)
        acc
        (let ([acc (proc (sek-iter-get it1) (sek-iter-get it2) acc)])
          (sek-iter-move! it1 'backward)
          (sek-iter-move! it2 'backward)
          (loop acc (sub1 k))))))

(define (sek-for-all2? s1 s2 pred)
  (define it1 (sek-iterator s1 'forward))
  (define it2 (sek-iterator s2 'forward))
  (let loop ()
    (cond
      [(or (sek-iter-finished? it1) (sek-iter-finished? it2)) #t]
      [(pred (sek-iter-get it1) (sek-iter-get it2))
       (sek-iter-move! it1 'forward)
       (sek-iter-move! it2 'forward)
       (loop)]
      [else #f])))

(define (sek-exists2? s1 s2 pred)
  (define it1 (sek-iterator s1 'forward))
  (define it2 (sek-iterator s2 'forward))
  (let loop ()
    (cond
      [(or (sek-iter-finished? it1) (sek-iter-finished? it2)) #f]
      [(pred (sek-iter-get it1) (sek-iter-get it2)) #t]
      [else
       (sek-iter-move! it1 'forward)
       (sek-iter-move! it2 'forward)
       (loop)])))

(define (sek-equal? s1 s2 [same? equal?])
  (and (= (sek-length s1) (sek-length s2)) (sek-for-all2? s1 s2 same?)))

;; -1, 0 or 1, comparing element by element and then by length.
;; cmp compares two elements and returns a negative number, zero, or a
;; positive number.
(define (sek-compare s1 s2 cmp)
  (define it1 (sek-iterator s1 'forward))
  (define it2 (sek-iterator s2 'forward))
  (let loop ()
    (define d1 (sek-iter-finished? it1))
    (define d2 (sek-iter-finished? it2))
    (cond
      [(and d1 d2) 0]
      [d1 -1]
      [d2 1]
      [else
       (define c (cmp (sek-iter-get it1) (sek-iter-get it2)))
       (cond
         [(eqv? c 0)
          (sek-iter-move! it1 'forward)
          (sek-iter-move! it2 'forward)
          (loop)]
         [else (if (negative? c) -1 1)])])))

;; ------------------------------------------------------------- producers

(define (sek-map s proc)
  (check-sek 'sek-map s)
  (build-from s (lambda (emit) (sek-for-each s (lambda (x) (emit (proc x)))))))

(define (sek-map/index s proc)
  (check-sek 'sek-map/index s)
  (build-from s (lambda (emit) (sek-for-each/index s (lambda (i x) (emit (proc i x)))))))

(define (sek-map2 s1 s2 proc)
  (build-from s1 (lambda (emit) (sek-for-each2 s1 s2 (lambda (x y) (emit (proc x y)))))))

(define (sek-filter s pred)
  (check-sek 'sek-filter s)
  (build-from s
              (lambda (emit)
                (sek-for-each s
                              (lambda (x)
                                (when (pred x)
                                  (emit x)))))))

(define (sek-filter-map s proc)
  (check-sek 'sek-filter-map s)
  (build-from s
              (lambda (emit)
                (sek-for-each s
                              (lambda (x)
                                (let ([y (proc x)])
                                  (when y
                                    (emit y))))))))

(define (sek-partition s pred)
  (check-sek 'sek-partition s)
  (define yes (open-builder))
  (define no (open-builder))
  (sek-for-each s (lambda (x) (eseq-push-back! (if (pred x) yes no) x)))
  (values (close-builder yes s) (close-builder no s)))

(define (sek-reverse s)
  (check-sek 'sek-reverse s)
  (build-from s (lambda (emit) (sek-for-each s emit 'backward))))

(define (sek-zip s1 s2)
  (sek-map2 s1 s2 cons))

(define (sek-unzip s)
  (check-sek 'sek-unzip s)
  (define as (open-builder))
  (define bs (open-builder))
  (sek-for-each s
                (lambda (p)
                  (eseq-push-back! as (car p))
                  (eseq-push-back! bs (cdr p))))
  (values (close-builder as s) (close-builder bs s)))

;; Concatenate a sequence of sequences.  On the ephemeral side this is a fold
;; of eseq-append!, exactly as in the reference library, so -- as there -- it
;; empties every sequence it is given, including the outer one.  Handing over
;; each sequence's representation rather than copying its elements is also the
;; faster way to do it.  On the persistent side nothing is consumed.
(define (sek-append* s)
  (check-sek 'sek-append* s)
  (cond
    [(pseq? s)
     (build-from s (lambda (emit) (sek-for-each s (lambda (t) (sek-for-each t emit)))))]
    [else
     (define acc (make-eseq))
     (sek-for-each s (lambda (t) (unless (eq? t acc) (eseq-append! acc t))))
     (eseq-clear! s)
     acc]))

(define (sek-append-map s proc)
  (check-sek 'sek-append-map s)
  (build-from s (lambda (emit) (sek-for-each s (lambda (x) (sek-for-each (proc x) emit))))))

;; ---------------------------------------------------------------- ordering

;; A stable sort, by way of a vector.
(define (sek-sort s less?)
  (check-sek 'sek-sort s)
  (define v (sek->vector s))
  (define sorted (vector-sort v less?))
  (build-from s
              (lambda (emit)
                (for ([x (in-vector sorted)])
                  (emit x)))))

;; Drop each element that is equal to the one before it, which removes all
;; duplicates when the sequence is sorted.
(define (sek-uniq s [same? equal?])
  (check-sek 'sek-uniq s)
  (define b (open-builder))
  (define first? #t)
  (define previous #f)
  (sek-for-each s
                (lambda (x)
                  (when (or first? (not (same? previous x)))
                    (eseq-push-back! b x))
                  (set! first? #f)
                  (set! previous x)))
  (close-builder b s))

;; Merge two sequences that are already sorted.
(define (sek-merge s1 s2 less?)
  (define b (open-builder))
  (define it1 (sek-iterator s1 'forward))
  (define it2 (sek-iterator s2 'forward))
  (let loop ()
    (cond
      [(sek-iter-finished? it1)
       (let drain ()
         (unless (sek-iter-finished? it2)
           (eseq-push-back! b (sek-iter-get-and-move! it2 'forward))
           (drain)))]
      [(sek-iter-finished? it2)
       (let drain ()
         (unless (sek-iter-finished? it1)
           (eseq-push-back! b (sek-iter-get-and-move! it1 'forward))
           (drain)))]
      [else
       ;; take from the left on a tie, which is what makes the merge stable
       (if (less? (sek-iter-get it2) (sek-iter-get it1))
           (eseq-push-back! b (sek-iter-get-and-move! it2 'forward))
           (eseq-push-back! b (sek-iter-get-and-move! it1 'forward)))
       (loop)]))
  (close-builder b s1))

;; ---------------------------------------------------------------- slicing

;; The `size` elements starting at `start`.  Unlike take/drop this costs
;; O(size + K) rather than O(K.log n + log^2 n), so it is the cheaper choice
;; when the slice is short.
(define (sek-sub s start size)
  (check-sek 'sek-sub s)
  (define n (sek-length s))
  (unless (and (exact-nonnegative-integer? start)
               (exact-nonnegative-integer? size)
               (<= (+ start size) n))
    (raise-arguments-error 'sek-sub
                           "slice outside the sequence"
                           "start"
                           start
                           "size"
                           size
                           "length"
                           n))
  (define b (open-builder))
  (unless (eqv? size 0)
    (define it (sek-iterator-at-sentinel s 'front))
    (sek-iter-reach! it start)
    (let loop ([remaining size])
      (unless (eqv? remaining 0)
        (define sg (sek-iter-segment it 'forward))
        (define k (min remaining (segment-length sg)))
        (define v (segment-vector sg))
        (define o (segment-start sg))
        (for ([j (in-range k)])
          (eseq-push-back! b (vector-ref v (+ o j))))
        (sek-iter-jump! it 'forward k)
        (loop (- remaining k)))))
  (close-builder b s))

;; Neither of these modifies s; the in-place versions are eseq-take! and
;; eseq-drop!.
(define (sek-take s n)
  (check-sek 'sek-take s)
  (if (pseq? s)
      (let-values ([(a b) (pseq-split s n)])
        a)
      (let-values ([(a b) (pseq-split (eseq-snapshot s) n)])
        (pseq-edit a))))

(define (sek-drop s n)
  (check-sek 'sek-drop s)
  (if (pseq? s)
      (let-values ([(a b) (pseq-split s n)])
        b)
      (let-values ([(a b) (pseq-split (eseq-snapshot s) n)])
        (pseq-edit b))))

(define (sek-copy s #:mode [mode 'share])
  (if (pseq? s)
      s
      (eseq-copy s #:mode mode)))

;; ------------------------------------------------------- bulk writes

;; Overwrite `size` elements starting at `start` with x.  Uses writable
;; segments, so the cost is O(size + K.log n) rather than one tree descent
;; per element.
(define (sek-fill! e start size x)
  (unless (eseq? e)
    (raise-argument-error 'sek-fill! "eseq?" e))
  (define n (eseq-length e))
  (unless (and (exact-nonnegative-integer? start)
               (exact-nonnegative-integer? size)
               (<= (+ start size) n))
    (raise-arguments-error 'sek-fill!
                           "range outside the sequence"
                           "start"
                           start
                           "size"
                           size
                           "length"
                           n))
  (unless (eqv? size 0)
    (define it (sek-iterator-at-sentinel e 'front))
    (sek-iter-reach! it start)
    (let loop ([remaining size])
      (unless (eqv? remaining 0)
        (define sg (sek-iter-writable-segment it 'forward))
        (define k (min remaining (segment-length sg)))
        (for ([j (in-range k)])
          (segment-set! sg j x))
        (sek-iter-jump! it 'forward k)
        (loop (- remaining k))))))

;; Copy `size` elements from src starting at src-start into dst at dst-start.
(define (sek-blit! src src-start dst dst-start size)
  (unless (eseq? dst)
    (raise-argument-error 'sek-blit! "eseq?" dst))
  (check-sek 'sek-blit! src)
  (unless (and (exact-nonnegative-integer? src-start)
               (exact-nonnegative-integer? dst-start)
               (exact-nonnegative-integer? size)
               (<= (+ src-start size) (sek-length src))
               (<= (+ dst-start size) (eseq-length dst)))
    (raise-arguments-error 'sek-blit!
                           "range outside a sequence"
                           "src-start"
                           src-start
                           "dst-start"
                           dst-start
                           "size"
                           size))
  (unless (eqv? size 0)
    ;; take a snapshot when the two sequences might be the same, so that
    ;; overlapping ranges behave as if the source were read first
    (define source
      (if (eq? src dst)
          (eseq-snapshot dst)
          src))
    (define si (sek-iterator-at-sentinel source 'front))
    (sek-iter-reach! si src-start)
    (define di (sek-iterator-at-sentinel dst 'front))
    (sek-iter-reach! di dst-start)
    (let loop ([remaining size])
      (unless (eqv? remaining 0)
        (define dseg (sek-iter-writable-segment di 'forward))
        (define sseg (sek-iter-segment si 'forward))
        (define k (min remaining (segment-length dseg) (segment-length sseg)))
        (define sv (segment-vector sseg))
        (define so (segment-start sseg))
        (for ([j (in-range k)])
          (segment-set! dseg j (vector-ref sv (+ so j))))
        (sek-iter-jump! si 'forward k)
        (sek-iter-jump! di 'forward k)
        (loop (- remaining k))))))

;; ------------------------------------------------------------ construction

;; Build from any Racket sequence -- the OCaml library's of_seq.
(define (sequence->eseq seq [n #f])
  (define e (make-eseq))
  (if n
      (for ([x seq] [_ (in-range n)]) (eseq-push-back! e x))
      (for ([x seq]) (eseq-push-back! e x)))
  e)

(define (sequence->pseq seq [n #f])
  (eseq-snapshot-and-clear! (sequence->eseq seq n)))

(define (build-eseq n proc)
  (define e (make-eseq))
  (for ([i (in-range n)])
    (eseq-push-back! e (proc i)))
  e)

(define (build-pseq n proc)
  (eseq-snapshot-and-clear! (build-eseq n proc)))

(define (make-pseq n [v #f])
  (build-pseq n (lambda (_) v)))

;; Comprehensions, in the shape of for/gvector and for/mutable-treelist.
(define-syntax (for/eseq stx)
  (syntax-case stx ()
    [(_ clauses body ... tail-expr)
     (quasisyntax/loc stx
       (let ([acc (make-eseq)])
         (for/fold/derived #,stx () clauses body ... (eseq-push-back! acc tail-expr) (values))
         acc))]))

(define-syntax (for*/eseq stx)
  (syntax-case stx ()
    [(_ clauses body ... tail-expr)
     (quasisyntax/loc stx
       (let ([acc (make-eseq)])
         (for*/fold/derived #,stx () clauses body ... (eseq-push-back! acc tail-expr) (values))
         acc))]))

(define-syntax (for/pseq stx)
  (syntax-case stx ()
    [(_ clauses body ... tail-expr)
     (quasisyntax/loc stx (eseq-snapshot-and-clear! (for/eseq clauses body ... tail-expr)))]))

(define-syntax (for*/pseq stx)
  (syntax-case stx ()
    [(_ clauses body ... tail-expr)
     (quasisyntax/loc stx (eseq-snapshot-and-clear! (for*/eseq clauses body ... tail-expr)))]))
