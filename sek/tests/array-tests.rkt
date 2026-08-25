#lang racket/base
;; Differential random testing of transient arrays (§2) against Racket vectors.

(require rackunit
         racket/vector
         "../config.rkt"
         "../array.rkt")

(provide run-array-random-tests)

;; kind is 'p or 'e; arr is a parray or an earray; vec is the reference
(struct entry (kind arr vec) #:transparent)

(define (entry->vector en)
  (if (eq? (entry-kind en) 'p)
      (parray->vector (entry-arr en))
      (earray->vector (entry-arr en))))

(define (check-entry en where)
  (unless (equal? (entry->vector en) (entry-vec en))
    (error 'array-tests "~a: candidate ~s, reference ~s" where (entry->vector en) (entry-vec en)))
  en)

(define counter 0)
(define (next!)
  (set! counter (add1 counter))
  counter)

(define (random-step! pool)
  (define n (vector-length pool))
  (define i (random n))
  (define en (vector-ref pool i))
  (define a (entry-arr en))
  (define v (entry-vec en))
  (define len (vector-length v))
  (define ephemeral? (eq? (entry-kind en) 'e))
  (define op (random 10))
  (define en*
    (case op
      [(0 1 2 3 4 5)
       (cond
         [(zero? len) en]
         [else
          (define j (random len))
          (define x (next!))
          (define got
            (if ephemeral?
                (earray-ref a j)
                (parray-ref a j)))
          (unless (equal? got (vector-ref v j))
            (error 'array-tests "ref ~a gave ~s, expected ~s" j got (vector-ref v j)))
          (define v* (vector-copy v))
          (vector-set! v* j x)
          (cond
            [ephemeral?
             (earray-set! a j x)
             (entry 'e a v*)]
            [else (entry 'p (parray-set a j x) v*)])])]
      ;; conversions: both versions must remain valid afterwards
      [(6 7)
       (define j (random n))
       (unless (eqv? j i)
         (vector-set! pool
                      j
                      (if ephemeral?
                          (entry 'p (earray-snapshot a) v)
                          (entry 'e (parray-edit a) v))))
       en]
      [(8)
       (define m (add1 (random 40)))
       (define v* (build-vector m (lambda (_) (next!))))
       (if (zero? (random 2))
           (entry 'e (vector->earray v*) v*)
           (entry 'p (vector->parray v*) v*))]
      [else en]))
  (vector-set! pool i en*)
  (when (and (eq? (entry-kind en*) 'p) (zero? (random 5)))
    (vector-set! pool (random n) en*))
  (for ([e (in-vector pool)])
    (check-entry e (format "op ~a" op))))

(define (run-array-random-tests #:steps [steps 2000] #:slots [slots 5] #:seed [seed 1])
  (random-seed seed)
  (define pool (make-vector slots #f))
  (for ([i (in-range slots)])
    (define v (build-vector 20 (lambda (_) (next!))))
    (vector-set! pool
                 i
                 (if (even? i)
                     (entry 'e (vector->earray v) v)
                     (entry 'p (vector->parray v) v))))
  (for ([_ (in-range steps)])
    (random-step! pool))
  (void))

(module+ test
  (for ([cfg (in-list '((2 2) (4 2) (8 4) (128 16)))])
    (sek-configure! #:leaf-capacity (car cfg) #:node-capacity (cadr cfg) #:short-threshold 0)
    (for ([seed (in-range 1 4)])
      (run-array-random-tests #:steps 800 #:seed seed)))
  (sek-configure! #:leaf-capacity 128 #:node-capacity 16 #:short-threshold 32)

  (define a (make-earray 1000 0))
  (for ([i (in-range 1000)])
    (earray-set! a i i))
  (define p (earray-snapshot a))
  (for ([i (in-range 1000)])
    (earray-set! a i (- i)))
  ;; the snapshot is unaffected by later writes to its source
  (check-equal? (parray->list p) (build-list 1000 values))
  (check-equal? (earray->list a) (build-list 1000 -))
  ;; and a persistent update leaves the original alone
  (define p2 (parray-set p 500 'x))
  (check-equal? (parray-ref p 500) 500)
  (check-equal? (parray-ref p2 500) 'x)
  (check-equal? (parray-length p2) 1000))
