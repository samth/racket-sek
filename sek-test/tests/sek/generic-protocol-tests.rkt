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
  (for ([make (list (lambda (n) (list->psek (elems n)))
                    (lambda (n) (list->esek (elems n))))]
        [name '(psek esek)])
    (for ([n (in-list '(0 1 2 17 200))])
      (define v (make n))
      (check-true (sequence? v) (format "~a is a sequence" name))
      (check-equal? (for/list ([x v]) x) (elems n)
                    (format "~a iterates in order at ~a" name n))
      (check-equal? (sequence-length v) n
                    (format "~a has the right sequence length at ~a" name n))))

  ;; ---- streams ---------------------------------------------------------
  ;; A persistent sequence is a stream, as an immutable treelist is.
  (check-true (stream? (psek 1 2 3)))
  (check-equal? (stream-first (psek 1 2 3)) 1)
  (check-equal? (stream->list (stream-rest (psek 1 2 3))) '(2 3))
  (check-true (stream-empty? empty-psek))
  (check-equal? (stream->list (psek 1 2 3)) '(1 2 3))
  ;; An ephemeral one is not, for the reason a mutable treelist is not.
  (check-false (stream? (esek 1 2 3)))

  ;; ---- structural equality and hashing ---------------------------------
  (check-equal? (psek 1 2 3) (psek 1 2 3))
  (check-equal? (esek 1 2 3) (esek 1 2 3))
  (check-not-equal? (psek 1 2 3) (psek 1 2))
  (check-not-equal? (psek 1 2 3) (psek 1 2 4))
  (check-not-equal? (esek 1 2 3) (esek 1 2 4))
  ;; equal? values must hash alike, or they break hash tables
  (for ([pair (in-list (list (cons (psek 1 2 3) (psek 1 2 3))
                             (cons (esek 1 2 3) (esek 1 2 3))))])
    (check-equal? (equal-hash-code (car pair)) (equal-hash-code (cdr pair))))
  ;; and they must actually work as hash keys
  (let ([h (hash (psek 1 2) 'a)])
    (check-equal? (hash-ref h (psek 1 2) #f) 'a))

  ;; ---- serialization ---------------------------------------------------
  (for ([n (in-list '(0 1 2 17 200))])
    (check-equal? (psek->list (deserialize (serialize (list->psek (elems n)))))
                  (elems n))
    (check-equal? (esek->list (deserialize (serialize (list->esek (elems n)))))
                  (elems n))
    )
  (check-true (serializable? (psek 1 2 3)))
  (check-true (serializable? (esek 1 2 3)))

  ;; a deserialized ephemeral sequence is independent of the original
  (let* ([e (esek 1 2 3)] [e2 (deserialize (serialize e))])
    (esek-push-back! e2 4)
    (check-equal? (esek->list e) '(1 2 3))
    (check-equal? (esek->list e2) '(1 2 3 4))))

(module+ test (run-generic-protocol-tests))
