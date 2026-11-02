#lang racket/base
;; The operations taken from racket/treelist, checked against it.
;;
;; These were added so that a reader coming from treelists finds what they
;; expect under the name they expect, which only holds if the behavior matches
;; too -- including the corner cases, which is where "insert at the end" and
;; "take-right of 0" live.  Running both implementations on the same inputs is
;; the cheapest way to keep that true.
(require rackunit
         racket/treelist
         racket/list
         sek)

(provide run-treelist-parity-tests)

(define (both l)
  (values (list->psek l) (list->treelist l)))

(define (run-treelist-parity-tests)
  (for ([n (in-list '(0 1 2 3 17 200))])
    (define l
      (for/list ([i (in-range n)])
        i))
    (define-values (p t) (both l))

    ;; add at the end, cons at the front, and the push aliases for each
    (check-equal? (psek->list (psek-add p 'x)) (treelist->list (treelist-add t 'x)))
    (check-equal? (psek->list (psek-cons p 'x)) (treelist->list (treelist-cons t 'x)))
    (check-equal? (psek->list (psek-push-back p 'x)) (psek->list (psek-add p 'x)))
    (check-equal? (psek->list (psek-push-front p 'x)) (psek->list (psek-cons p 'x)))

    ;; insert accepts the length itself, meaning "at the end"
    (for ([i (in-range (add1 n))])
      (check-equal? (psek->list (sek-insert p i 'x))
                    (treelist->list (treelist-insert t i 'x))
                    (format "insert at ~a of ~a" i n)))

    ;; delete does not
    (for ([i (in-range n)])
      (check-equal? (psek->list (sek-delete p i))
                    (treelist->list (treelist-delete t i))
                    (format "delete at ~a of ~a" i n)))

    ;; the four takes and drops, including 0 and the whole length
    (for ([k (in-range (add1 n))])
      (check-equal? (psek->list (sek-take p k)) (treelist->list (treelist-take t k)))
      (check-equal? (psek->list (sek-drop p k)) (treelist->list (treelist-drop t k)))
      (check-equal? (psek->list (sek-take-right p k))
                    (treelist->list (treelist-take-right t k))
                    (format "take-right ~a of ~a" k n))
      (check-equal? (psek->list (sek-drop-right p k))
                    (treelist->list (treelist-drop-right t k))
                    (format "drop-right ~a of ~a" k n)))

    ;; member?, which takes the sequence first as treelist-member? does
    (for ([v (in-list (append l (list 'absent)))])
      (check-equal? (sek-member? p v) (treelist-member? t v)
                    (format "member? ~s in ~a" v n)))

    ;; index-of, hit and miss, and with a supplied comparison
    (for ([v (in-list (append l (list 'absent)))])
      (check-equal? (sek-index-of p v) (treelist-index-of t v)))
    (when (positive? n)
      (check-equal? (sek-index-of p (exact->inexact (first l)) =)
                    (treelist-index-of t (exact->inexact (first l)) =))))

  ;; out-of-range indices are rejected
  (check-exn exn:fail? (lambda () (sek-insert (psek 1 2) 3 'x)))
  (check-exn exn:fail? (lambda () (sek-delete (psek 1 2) 2)))
  (check-exn exn:fail? (lambda () (sek-take-right (psek 1 2) 3)))
  (check-exn exn:fail? (lambda () (sek-drop-right (psek 1 2) 3)))

  ;; the same operations on an ephemeral sequence give an ephemeral sequence
  (check-equal? (esek->list (sek-insert (esek 1 2 3) 1 'x)) '(1 x 2 3))
  (check-equal? (esek->list (sek-delete (esek 1 2 3) 1)) '(1 3))
  (check-equal? (esek->list (sek-take-right (esek 1 2 3) 2)) '(2 3))
  (check-equal? (esek->list (sek-drop-right (esek 1 2 3) 2)) '(1))
  (check-equal? (sek-index-of (esek 'a 'b) 'b) 1)

  ;; the ephemeral add!/cons! pair, and their aliases
  (let ([e (esek 1 2)])
    (esek-add! e 3)
    (esek-cons! e 0)
    (check-equal? (esek->list e) '(0 1 2 3)))
  (let ([e (esek 1 2)])
    (esek-push-back! e 3)
    (esek-push-front! e 0)
    (check-equal? (esek->list e) '(0 1 2 3))))

(module+ test
  (run-treelist-parity-tests))
