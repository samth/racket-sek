#lang racket/base
;; Differential random testing of persistent sequences against a list-based
;; reference implementation, in the spirit of the paper's use of Monolith
;; (§4.2): build long, non-linear scenarios in which several versions of the
;; structure -- sharing common ancestors -- are operated upon, and validate the
;; invariants after every step.

(require rackunit
         racket/list
         "../config.rkt"
         "../persistent.rkt"
         "../check.rkt")

(provide run-persistent-random-tests)

;; A "model" pairs a candidate sequence with the list it should represent.
(struct model (seq list) #:transparent)

(define (check-model m where)
  (define s (model-seq m))
  (define xs (model-list m))
  (check-pseq s)
  (unless (equal? (pseq->list s) xs)
    (error 'persistent-tests "~a: candidate ~s, reference ~s" where (pseq->list s) xs))
  ;; the streaming traversal must agree with the push-based one
  (let ([ys (for/list ([x (in-pseq s)]) x)])
    (unless (equal? ys xs)
      (error 'persistent-tests "~a: in-pseq gave ~s, reference ~s" where ys xs)))
  (unless (= (pseq-length s) (length xs))
    (error 'persistent-tests "~a: length ~a, reference ~a" where (pseq-length s) (length xs)))
  m)

(define counter 0)
(define (next!)
  (set! counter (add1 counter))
  counter)

(define max-length 400)

(define (random-step! pool)
  ;; pool : (vectorof model), mutated in place; models are persistent so old
  ;; versions stay live and keep sharing structure with the new ones
  (define n (vector-length pool))
  (define i (random n))
  (define m (vector-ref pool i))
  (define s (model-seq m))
  (define xs (model-list m))
  (define len (length xs))
  (define op (random 12))
  (define m*
    (case op
      [(0 1) (let ([x (next!)]) (model (pseq-push-front s x) (cons x xs)))]
      [(2 3 4) (let ([x (next!)]) (model (pseq-push-back s x) (append xs (list x))))]
      [(5)
       (if (zero? len)
           m
           (let-values ([(x s*) (pseq-pop-front s)])
             (unless (equal? x (car xs))
               (error 'persistent-tests "pop-front gave ~s, expected ~s" x (car xs)))
             (model s* (cdr xs))))]
      [(6)
       (if (zero? len)
           m
           (let-values ([(x s*) (pseq-pop-back s)])
             (unless (equal? x (last xs))
               (error 'persistent-tests "pop-back gave ~s, expected ~s" x (last xs)))
             (model s* (drop-right xs 1))))]
      [(7)
       (if (zero? len)
           m
           (let* ([j (random len)]
                  [x (next!)])
             (unless (equal? (pseq-ref s j) (list-ref xs j))
               (error 'persistent-tests
                      "ref ~a gave ~s, expected ~s"
                      j
                      (pseq-ref s j)
                      (list-ref xs j)))
             (model (pseq-set s j x) (append (take xs j) (list x) (drop xs (add1 j))))))]
      [(8 9)
       ;; appending two pool members doubles the length, so cap the growth
       (let* ([j (random n)]
              [m2 (vector-ref pool j)])
         (if (> (+ len (length (model-list m2))) max-length)
             m
             (model (pseq-append s (model-seq m2)) (append xs (model-list m2)))))]
      [(10 11)
       (let ([j (random (add1 len))])
         (define-values (s1 s2) (pseq-split s j))
         (check-model (model s1 (take xs j)) "split-left")
         (check-model (model s2 (drop xs j)) "split-right")
         ;; keep one half, at random
         (if (zero? (random 2))
             (model s1 (take xs j))
             (model s2 (drop xs j))))]
      [else m]))
  (check-model m* (format "op ~a" op))
  ;; sometimes overwrite a *different* slot, so that old versions survive
  (vector-set! pool
               (if (zero? (random 4))
                   (random n)
                   i)
               m*))

(define (run-persistent-random-tests #:steps [steps 3000] #:slots [slots 6] #:seed [seed 1])
  (random-seed seed)
  (define pool (make-vector slots (model empty-pseq '())))
  (for ([_ (in-range steps)])
    (random-step! pool))
  (void))

(module+ test
  ;; Small capacities force deep trees and exercise the cascading cases.
  (for ([cfg (in-list '((2 2 0) (3 2 2) (4 4 4) (8 4 6) (16 8 12) (128 16 32)))])
    (define-values (k0 k1 t) (values (car cfg) (cadr cfg) (caddr cfg)))
    (sek-configure! #:leaf-capacity k0 #:node-capacity k1 #:short-threshold t)
    (for ([seed (in-range 1 4)])
      (run-persistent-random-tests #:steps 1500 #:seed seed)))
  (sek-configure! #:leaf-capacity 128 #:node-capacity 16 #:short-threshold 32)

  ;; a few deterministic smoke tests
  (define s (list->pseq (build-list 1000 values)))
  (check-equal? (pseq-length s) 1000)
  (check-equal? (pseq->list s) (build-list 1000 values))
  (check-equal? (pseq-ref s 777) 777)
  (define-values (a b) (pseq-split s 500))
  (check-equal? (pseq->list a) (build-list 500 values))
  (check-equal? (pseq->list (pseq-append a b)) (build-list 1000 values))
  (check-equal? (pseq->list (pseq 1 2 3)) '(1 2 3))
  (check-equal? (for/list ([x (in-pseq (pseq 'a 'b))])
                  x)
                '(a b))
  (check-true (equal? (pseq 1 2 3) (list->pseq '(1 2 3)))))
