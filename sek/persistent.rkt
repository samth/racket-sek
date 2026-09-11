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
         "config.rkt"
         "chunk.rkt"
         "ptree.rkt"
         "iterate.rkt")

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
  (not (psq-rep s)))

(define (pseq-length s)
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
  (rep->list (psq-rep s)))

(define (pseq->vector s)
  (define r (psq-rep s))
  (cond
    [(not r) (vector)]
    [(vector? r) (vector-copy r)]
    [else (tree->vector r)]))

(define (pseq-for-each s proc)
  (define r (psq-rep s))
  (cond
    [(not r) (void)]
    [(vector? r)
     (for ([x (in-vector r)])
       (proc x))]
    [else (pt-for-each r 0 proc)]))

(define (pseq-map s proc)
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
