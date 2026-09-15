#lang racket/base
;; The generic protocols, checked against the structures sek is meant to be an
;; alternative to.  racket/treelist is the reference: an immutable treelist is
;; a sequence, a stream, serializable, and structurally equal?; a mutable one
;; is all of those but a stream.  These tests pin the same shape here, since
;; the participation is easy to lose silently -- a struct property dropped
;; during a refactor produces no error, just a value that no longer works with
;; `for` or `serialize`.
(require rackunit
         racket/stream
         racket/serialize
         racket/sequence
         sek)

(provide run-generic-protocol-tests)

(define (elems n) (for/list ([i (in-range n)]) i))

(define (run-generic-protocol-tests)

  ;; ---- sequences -------------------------------------------------------
  (for ([make (list (lambda (n) (list->pseq (elems n)))
                    (lambda (n) (list->eseq (elems n)))
                    (lambda (n) (vector->parray (build-vector n values)))
                    (lambda (n) (vector->earray (build-vector n values))))]
        [name '(pseq eseq parray earray)])
    (for ([n (in-list '(0 1 2 17 200))])
      (define v (make n))
      (check-true (sequence? v) (format "~a is a sequence" name))
      (check-equal? (for/list ([x v]) x) (elems n)
                    (format "~a iterates in order at ~a" name n))
      (check-equal? (sequence-length v) n
                    (format "~a has the right sequence length at ~a" name n))))

  ;; the named forms agree with using the value directly
  (check-equal? (for/list ([x (in-parray (vector->parray (vector 1 2 3)))]) x) '(1 2 3))
  (check-equal? (for/list ([x (in-earray (vector->earray (vector 1 2 3)))]) x) '(1 2 3))

  ;; ---- streams ---------------------------------------------------------
  ;; A persistent sequence is a stream, as an immutable treelist is.
  (check-true (stream? (pseq 1 2 3)))
  (check-equal? (stream-first (pseq 1 2 3)) 1)
  (check-equal? (stream->list (stream-rest (pseq 1 2 3))) '(2 3))
  (check-true (stream-empty? empty-pseq))
  (check-equal? (stream->list (pseq 1 2 3)) '(1 2 3))
  ;; An ephemeral one is not, for the reason a mutable treelist is not.
  (check-false (stream? (eseq 1 2 3)))

  ;; ---- structural equality and hashing ---------------------------------
  (check-equal? (pseq 1 2 3) (pseq 1 2 3))
  (check-equal? (eseq 1 2 3) (eseq 1 2 3))
  (check-equal? (vector->parray (vector 1 2)) (vector->parray (vector 1 2)))
  (check-equal? (vector->earray (vector 1 2)) (vector->earray (vector 1 2)))
  (check-not-equal? (pseq 1 2 3) (pseq 1 2))
  (check-not-equal? (pseq 1 2 3) (pseq 1 2 4))
  (check-not-equal? (eseq 1 2 3) (eseq 1 2 4))
  ;; equal? values must hash alike, or they break hash tables
  (for ([pair (in-list (list (cons (pseq 1 2 3) (pseq 1 2 3))
                             (cons (eseq 1 2 3) (eseq 1 2 3))
                             (cons (vector->parray (vector 4 5))
                                   (vector->parray (vector 4 5)))))])
    (check-equal? (equal-hash-code (car pair)) (equal-hash-code (cdr pair))))
  ;; and they must actually work as hash keys
  (let ([h (hash (pseq 1 2) 'a)])
    (check-equal? (hash-ref h (pseq 1 2) #f) 'a))

  ;; ---- serialization ---------------------------------------------------
  (for ([n (in-list '(0 1 2 17 200))])
    (check-equal? (pseq->list (deserialize (serialize (list->pseq (elems n)))))
                  (elems n))
    (check-equal? (eseq->list (deserialize (serialize (list->eseq (elems n)))))
                  (elems n))
    (check-equal? (parray->list
                   (deserialize (serialize (vector->parray (build-vector n values)))))
                  (elems n))
    (check-equal? (earray->list
                   (deserialize (serialize (vector->earray (build-vector n values)))))
                  (elems n)))
  (check-true (serializable? (pseq 1 2 3)))
  (check-true (serializable? (eseq 1 2 3)))

  ;; a deserialized ephemeral sequence is independent of the original
  (let* ([e (eseq 1 2 3)] [e2 (deserialize (serialize e))])
    (eseq-push-back! e2 4)
    (check-equal? (eseq->list e) '(1 2 3))
    (check-equal? (eseq->list e2) '(1 2 3 4))))

(module+ test (run-generic-protocol-tests))
