#lang racket/base
;; Persistent sequences -- Charguéraud & Pottier, ICFP 2026, §3.5.
;;
;; A persistent sequence is one of
;;   * #f, the empty sequence;
;;   * a vector of 1..T elements, the compact representation of a short
;;     sequence (the paper's One and Short constructors, unified here);
;;   * a Sek tree (§3.1), for everything longer.
;; The compact representation exists so that a short sequence costs O(n) words
;; instead of the O(K) implied by a level's two chunks.  It appears only at the
;; top of the structure; middle sequences are always trees.

(require racket/vector
         racket/fixnum
         racket/performance-hint
         (only-in racket/unsafe/ops unsafe-vector*-ref)
         "config.rkt"
         "chunk.rkt"
         "ptree.rkt"
         "iterate.rkt")

;; Compiled in unsafe mode.  Every function here that a caller outside the
;; library can reach checks its arguments explicitly, with `unless` rather
;; than by relying on a struct accessor or a vector reference to raise --
;; in unsafe mode those do not raise, they read whatever is at the offset.
(#%declare #:unsafe)

(provide (rename-out [psq? pseq?] [psq-rep pseq-rep])
         pseq-length
         pseq-empty?
         empty-pseq
         pseq-push-front
         pseq-push-back
         pseq-pop-front
         pseq-pop-back
         pseq-first
         pseq-last
         pseq-ref
         pseq-set
         pseq-append
         pseq-split
         pseq-build
         make-pseq-builder
         pseq-builder-add!
         pseq-builder-close
         pseq-take
         pseq-drop
         pseq->list
         list->pseq
         pseq->vector
         vector->pseq
         in-pseq
         pseq-for-each
         pseq-map
         pseq
         wrap-rep
         pseq-of-tree
         normalize
         rep->tree)

;; ------------------------------------------------------------- the datatype


;; Argument checking is explicit here, because this module is compiled in
;; unsafe mode: a struct accessor no longer raises on the wrong kind of
;; value, it reads whatever happens to be at that offset.
(define-syntax-rule (check-pseq who v)
  (unless (psq? v) (raise-argument-error who "pseq?" v)))

(define (rep->list rep)
  (cond
    [(not rep) '()]
    [(vector? rep) (vector->list rep)]
    [else
     (let ([acc '()])
       (pt-for-each rep 0 (lambda (x) (set! acc (cons x acc))))
       (reverse acc))]))

(struct psq (rep)
  #:constructor-name wrap-rep
  #:authentic #:sealed
  #:property prop:sequence
  (lambda (s) (in-pseq s))
  #:methods gen:equal+hash
  [(define (equal-proc a b rec)
     (rec (pseq->list a) (pseq->list b)))
   (define (hash-proc a rec)
     (rec (pseq->list a)))
   (define (hash2-proc a rec)
     (rec (pseq->list a)))]
  #:methods gen:custom-write
  [(define (write-proc s port mode)
     (define xs (pseq->list s))
     (write-string "#<pseq:" port)
     (let loop ([xs xs]
                [n 0])
       (cond
         [(null? xs) (void)]
         [(= n 10) (write-string " ..." port)]
         [else
          (write-string " " port)
          (if (eq? mode #t)
              (write (car xs) port)
              (display (car xs) port))
          (loop (cdr xs) (add1 n))]))
     (write-string ">" port))])

(define empty-pseq (wrap-rep #f))
(define (pseq-of-tree t)
  (wrap-rep (normalize t)))

(define (pseq-empty? s)
  (check-pseq 'pseq-empty? s)
  (not (psq-rep s)))

;; Build n elements a whole chunk at a time, rather than pushing one element at
;; a time, which is what the reference's `init` does (`create_by_segments`,
;; filling each chunk with `EChunk.init`).  A push tests whether the back chunk
;; is full, invalidates the iterators and consults the ownership id once per
;; element; filling a chunk does that once per K elements and writes the rest
;; straight into a vector.
;;
;; The tree is then assembled directly: the first chunk is the front, the last
;; is the back, and everything between is pushed into the middle sequence,
;; which is the shape `make-level` wants.  Every chunk but the last is full, so
;; the density invariant holds by construction.
(define (pseq-build n proc)
  (define k (capacity-at 0))
  (define (chunk-at i len)
    (chunk-build k len (lambda (j) (proc (+ i j))) no-owner))
  (cond
    [(eqv? n 0) empty-pseq]
    [(<= n (short-threshold)) (wrap-rep (build-vector n proc))]
    [(<= n k) (wrap-rep (make-level (chunk-at 0 n) #f (make-chunk k no-owner)))]
    [else
     (define rest (remainder n k))
     (define last-start (- n (if (eqv? rest 0) k rest)))
     (define front (chunk-at 0 k))
     (define middle
       (for/fold ([t #f]) ([i (in-range k last-start k)])
         (pt-push-back t (chunk-at i k) k 1 no-owner)))
     (wrap-rep (make-level front middle (chunk-at last-start (- n last-start))))]))

;; The same idea as `pseq-build`, for callers that do not know the length in
;; advance: collect elements into a chunk-sized buffer and emit a whole chunk
;; when it fills, rather than pushing each element into a sequence.  One chunk
;; is held back so that the last full one can become the back of the level when
;; the buffer is empty at the end.
;; `cap` is the leaf capacity, which never changes over a builder's life, so
;; the hot path reads it from the builder rather than re-deriving it.
(struct bld ([buf #:mutable] [n #:mutable] cap [front #:mutable]
             [held #:mutable] [middle #:mutable])
  #:authentic #:sealed)

(define (make-pseq-builder)
  (define k (capacity-at 0))
  (bld (make-vector k #f) 0 k #f #f #f))

(define (pseq-builder-emit! b c)
  (cond
    [(not (bld-front b)) (set-bld-front! b c)]
    [(not (bld-held b)) (set-bld-held! b c)]
    [else
     (define h (bld-held b))
     (set-bld-middle! b (pt-push-back (bld-middle b) h (chunk-weight h) 1 no-owner))
     (set-bld-held! b c)]))

;; Adding one element is a store and a bounds test.  It is split so that the
;; part that runs per element is small enough for the inliner to copy into
;; callers in other modules, which is what lets `sek-map` and `sek-filter` run
;; their loops without a call per element; the chunk-full case is a call.
(begin-encourage-inline
  (define (pseq-builder-add! b x)
    (define n (bld-n b))
    (vector-set! (bld-buf b) n x)
    (define n* (fx+ n 1))
    (if (fx< n* (bld-cap b))
        (set-bld-n! b n*)
        (pseq-builder-cut! b))))

;; The buffer is full: hand it over as a chunk and start a new one.  It is
;; replaced here and never read again, so the chunk can adopt it rather than
;; copy it.
(define (pseq-builder-cut! b)
  (pseq-builder-emit! b (chunk-of-fresh-vector (bld-buf b) no-owner))
  (set-bld-buf! b (make-vector (bld-cap b) #f))
  (set-bld-n! b 0))

(define (pseq-builder-close b)
  (define k (bld-cap b))
  (define n (bld-n b))
  (define buf (bld-buf b))
  (define partial
    (and (> n 0) (chunk-build k n (lambda (i) (unsafe-vector*-ref buf i)) no-owner)))
  (define front (bld-front b))
  (define held (bld-held b))
  (cond
    ;; nothing emitted: everything is still in the buffer.  That does not make
    ;; it short enough for the compact representation -- the threshold may be
    ;; below the chunk capacity -- so it still has to be tested.
    [(not front)
     (cond
       [(eqv? n 0) empty-pseq]
       [(<= n (short-threshold)) (wrap-rep (vector-copy (bld-buf b) 0 n))]
       [else (wrap-rep (make-level partial #f (make-chunk k no-owner)))])]
    [else
     (define-values (mid back)
       (cond
         [partial
          (values (if held
                      (pt-push-back (bld-middle b) held (chunk-weight held) 1 no-owner)
                      (bld-middle b))
                  partial)]
         [held (values (bld-middle b) held)]
         [else (values (bld-middle b) (make-chunk k no-owner))]))
     (wrap-rep (normalize (make-level front mid back)))]))

(define (pseq-length s)
  (check-pseq 'pseq-length s)
  (define r (psq-rep s))
  (cond
    [(not r) 0]
    [(vector? r) (vector-length r)]
    [else (lvl-weight r)]))

;; ------------------------------------------------------- representation swaps

;; A tree that has become short enough is folded back into a vector, which is
;; what keeps the space bound of short sequences independent of K.
(define (normalize t)
  (cond
    [(not t) #f]
    [(<= (lvl-weight t) (short-threshold)) (tree->vector t)]
    [else t]))

(define (tree->vector t)
  (define v (make-vector (pt-weight t) #f))
  (define i 0)
  (pt-for-each t
               0
               (lambda (x)
                 (vector-set! v i x)
                 (set! i (add1 i))))
  v)

(define (vector->tree v)
  (define k (capacity-at 0))
  (define n (vector-length v))
  (cond
    [(eqv? n 0) #f]
    [(<= n k) (make-level (chunk-of-vector v k unit-measure no-owner) #f (make-chunk k no-owner))]
    [else
     (for/fold ([t #f]) ([x (in-vector v)])
       (pt-push-back t x 1 0 no-owner))]))

;; The rep of s as a tree, converting the compact form if necessary.
(define (rep->tree r)
  (if (vector? r)
      (vector->tree r)
      r))

;; ------------------------------------------------------------------ push/pop

(define (pseq-push-front s x)
  (check-pseq 'pseq-push-front s)
  (define r (psq-rep s))
  (define T (short-threshold))
  (wrap-rep (cond
          [(not r)
           (if (>= T 1)
               (vector x)
               (pt-push-front #f x 1 0 no-owner))]
          [(vector? r)
           (if (< (vector-length r) T)
               (let ([v (make-vector (add1 (vector-length r)) x)])
                 (vector-copy! v 1 r)
                 v)
               (pt-push-front (vector->tree r) x 1 0 no-owner))]
          [else (pt-push-front r x 1 0 no-owner)])))

(define (pseq-push-back s x)
  (check-pseq 'pseq-push-back s)
  (define r (psq-rep s))
  (define T (short-threshold))
  (wrap-rep (cond
          [(not r)
           (if (>= T 1)
               (vector x)
               (pt-push-back #f x 1 0 no-owner))]
          [(vector? r)
           (if (< (vector-length r) T)
               (let ([v (make-vector (add1 (vector-length r)) x)])
                 (vector-copy! v 0 r)
                 v)
               (pt-push-back (vector->tree r) x 1 0 no-owner))]
          [else (pt-push-back r x 1 0 no-owner)])))

(define (pseq-pop-front s)
  (define r (psq-rep s))
  (cond
    [(not r) (raise-arguments-error 'pseq-pop-front "sequence is empty")]
    [(vector? r)
     (values (vector-ref r 0)
             (wrap-rep (if (eqv? (vector-length r) 1)
                       #f
                       (vector-drop r 1))))]
    [else
     (define-values (x t) (pt-pop-front r 0 no-owner))
     (values x (wrap-rep (normalize t)))]))

(define (pseq-pop-back s)
  (define r (psq-rep s))
  (cond
    [(not r) (raise-arguments-error 'pseq-pop-back "sequence is empty")]
    [(vector? r)
     (define n (vector-length r))
     (values (vector-ref r (sub1 n))
             (wrap-rep (if (eqv? n 1)
                       #f
                       (vector-copy r 0 (sub1 n)))))]
    [else
     (define-values (x t) (pt-pop-back r 0 no-owner))
     (values x (wrap-rep (normalize t)))]))

(define (pseq-first s)
  (define r (psq-rep s))
  (cond
    [(not r) (raise-arguments-error 'pseq-first "sequence is empty")]
    [(vector? r) (vector-ref r 0)]
    [else (pt-ref r 0 0)]))

(define (pseq-last s)
  (define r (psq-rep s))
  (cond
    [(not r) (raise-arguments-error 'pseq-last "sequence is empty")]
    [(vector? r) (vector-ref r (sub1 (vector-length r)))]
    [else (pt-ref r (sub1 (lvl-weight r)) 0)]))

;; ------------------------------------------------------------------ get/set

(define (check-index who s i)
  (unless (and (exact-nonnegative-integer? i) (< i (pseq-length s)))
    (raise-arguments-error who "index out of range"
                           "index" i "length" (pseq-length s))))

;; The bounds check and the dispatch both have to look at the representation,
;; so do it once: `check-index` would go back through `pseq-length`, which
;; re-tests whether the rep is a vector or a tree.
(define (pseq-ref s i)
  (check-pseq 'pseq-ref s)
  (define r (psq-rep s))
  (cond
    [(and (lvl? r) (exact-nonnegative-integer? i) (< i (lvl-weight r)))
     (pt-ref r i 0)]
    [(and (vector? r) (exact-nonnegative-integer? i) (< i (vector-length r)))
     (vector-ref r i)]
    [else
     (check-index 'pseq-ref s i)
     (if (vector? r) (vector-ref r i) (pt-ref r i 0))]))

(define (pseq-set s i x)
  (check-index 'pseq-set s i)
  (define r (psq-rep s))
  (wrap-rep (if (vector? r)
            (let ([v (vector-copy r)])
              (vector-set! v i x)
              v)
            (pt-set r i x 0 no-owner))))

;; ------------------------------------------------------- concat and split

(define (pseq-append s1 s2)
  (define r1 (psq-rep s1))
  (define r2 (psq-rep s2))
  (cond
    [(not r1) s2]
    [(not r2) s1]
    [(and (vector? r1) (vector? r2) (<= (+ (vector-length r1) (vector-length r2)) (short-threshold)))
     (wrap-rep (vector-append r1 r2))]
    [else (wrap-rep (normalize (pt-concat (rep->tree r1) (rep->tree r2) 0 no-owner)))]))

;; Split into the first i elements and the rest.
(define (pseq-split s i)
  (define n (pseq-length s))
  (unless (and (exact-nonnegative-integer? i) (<= i n))
    (raise-arguments-error 'pseq-split "index out of range"
                           "index" i "length" n))
  (define r (psq-rep s))
  (cond
    [(eqv? i 0) (values empty-pseq s)]
    [(eqv? i n) (values s empty-pseq)]
    [(vector? r) (values (wrap-rep (vector-copy r 0 i)) (wrap-rep (vector-copy r i)))]
    [else
     (define-values (t1 t2) (pt-split r i no-owner))
     (values (wrap-rep (normalize t1)) (wrap-rep (normalize t2)))]))

;; One-sided versions, which build only the half that is wanted.  The reference
;; specialises `three_way_split` the same way, into `take`, `drop` and `get`
;; (ShareableSequence.ml); `get` is `pseq-ref` here and was already separate.
(define (pseq-take s i)
  (define n (pseq-length s))
  (unless (and (exact-nonnegative-integer? i) (<= i n))
    (raise-arguments-error 'pseq-take "index out of range"
                           "index" i "length" n))
  (define r (psq-rep s))
  (cond
    [(eqv? i 0) empty-pseq]
    [(eqv? i n) s]
    [(vector? r) (wrap-rep (vector-copy r 0 i))]
    [else (wrap-rep (normalize (pt-take r i no-owner)))]))

(define (pseq-drop s i)
  (define n (pseq-length s))
  (unless (and (exact-nonnegative-integer? i) (<= i n))
    (raise-arguments-error 'pseq-drop "index out of range"
                           "index" i "length" n))
  (define r (psq-rep s))
  (cond
    [(eqv? i 0) s]
    [(eqv? i n) empty-pseq]
    [(vector? r) (wrap-rep (vector-copy r i))]
    [else (wrap-rep (normalize (pt-drop r i no-owner)))]))

;; -------------------------------------------------------------- conversions

(define (pseq->list s)
  (check-pseq 'pseq->list s)
  (rep->list (psq-rep s)))

(define (pseq->vector s)
  (check-pseq 'pseq->vector s)
  (define r (psq-rep s))
  (cond
    [(not r) (vector)]
    [(vector? r) (vector-copy r)]
    [else (tree->vector r)]))

(define (pseq-for-each s proc)
  (check-pseq 'pseq-for-each s)
  (define r (psq-rep s))
  (cond
    [(not r) (void)]
    [(vector? r)
     (for ([x (in-vector r)])
       (proc x))]
    [else (pt-for-each r 0 proc)]))

(define (pseq-map s proc)
  (check-pseq 'pseq-map s)
  (list->pseq (map proc (pseq->list s))))

;; Streaming traversal: `for` over a sequence walks the tree directly instead
;; of building an intermediate list.
(define (in-pseq s)
  (define r (psq-rep s))
  (cond
    [(not r) (in-list '())]
    [(vector? r) (in-vector r)]
    [else (reader->sequence (lambda () (make-reader (list (cons r 0)))))]))

;; Bulk construction claims a private ownership id and builds the tree with
;; in-place pushes, then drops the id.  Because ids are never reused, the
;; chunks are unowned from then on -- this is just a cheaper way of writing the
;; same repeated pseq-push-back.
(define (list->pseq xs)
  (define n (length xs))
  (cond
    [(eqv? n 0) empty-pseq]
    [(<= n (short-threshold)) (wrap-rep (list->vector xs))]
    [else (wrap-rep (pt-of-list xs (fresh-id!)))]))

(define (vector->pseq v) (list->pseq (vector->list v)))

(define (pseq . xs)
  (list->pseq xs))
