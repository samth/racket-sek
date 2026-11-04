#lang racket/base
;; Behavior of the public interface that the other files, written mostly
;; against the internal modules, do not reach: the sequence forms as values as
;; well as in for clauses, binary folds over sequences of different lengths,
;; what a comparator's zero means, and traversals that race an update.

(require rackunit
         racket/list
         racket/sequence
         sek)

(define (both xs)
  (list (list->psek xs) (list->esek xs)))

(test-case "in-psek, in-esek and in-sek go either way, as values and as clauses"
  (for ([s (in-list (both '(1 2 3)))])
    (define in-flavor (if (psek? s) in-psek in-esek))
    (check-equal? (sequence->list (in-flavor s)) '(1 2 3))
    (check-equal? (sequence->list (in-flavor s 'forward)) '(1 2 3))
    (check-equal? (sequence->list (in-flavor s 'backward)) '(3 2 1))
    (check-equal? (sequence->list (in-sek s 'backward)) '(3 2 1))
    (check-equal? (if (psek? s)
                      (for/list ([x (in-psek s 'backward)])
                        x)
                      (for/list ([x (in-esek s 'backward)])
                        x))
                  '(3 2 1))
    (check-equal? (for/list ([x (in-sek s 'backward)])
                    x)
                  '(3 2 1))))

;; The reference defines fold_right2 as iter2 backward, so the two sequences
;; are paired from their last elements.
(test-case "sek-fold-right2 pairs the sequences from their ends"
  (define (pairs a b)
    (sek-fold-right2 a b (lambda (x y acc) (cons (list x y) acc)) '()))
  (for* ([a (in-list (both '(1 2 3)))]
         [b (in-list (both '(10 20)))])
    (check-equal? (pairs a b) '((2 10) (3 20)))
    (check-equal? (pairs b a) '((10 2) (20 3)))
    (check-equal? (pairs a (list->psek '())) '())
    (check-equal? (pairs (list->esek '()) b) '()))
  ;; and agrees with a backward binary traversal at every pair of lengths
  (for* ([n (in-range 0 40 7)]
         [m (in-range 0 300 37)])
    (define a (list->psek (range n)))
    (define b (list->esek (range 1000 (+ 1000 m))))
    (define backward '())
    (sek-for-each2 a b (lambda (x y) (set! backward (cons (list x y) backward))) 'backward)
    (check-equal? (pairs a b) backward)))

(test-case "sek-fold-left2 pairs the sequences from their starts"
  (check-equal?
   (sek-fold-left2 (psek 1 2 3) (esek 10 20) (lambda (acc x y) (cons (list x y) acc)) '())
   '((2 20) (1 10))))

;; As in SRFI 67 and OCaml, only an exact 0 means equal.
(test-case "sek-compare treats only an exact 0 as equal"
  (check-equal? (sek-compare (psek 1 2) (psek 1 3) -) -1)
  (check-equal? (sek-compare (psek 1 3) (esek 1 2) -) 1)
  (check-equal? (sek-compare (psek 1 2) (esek 1 2) -) 0)
  (check-equal? (sek-compare (psek 1 2) (psek 1 2 0) -) -1)
  (check-equal? (sek-compare (psek 1 2) (psek 1 2) (lambda (a b) (if (= a b) 0.0 -1.0))) 1))

(test-case "sequence->psek takes at most n elements"
  (check-equal? (sek->list (sequence->psek '(a b) 5)) '(a b))
  (check-equal? (sek->list (sequence->esek '(a b c) 2)) '(a b))
  (check-equal? (sek->list (sequence->psek (in-naturals) 4)) '(0 1 2 3)))

;; An update to an ephemeral sequence during a traversal is an error.  Handed
;; the sequence directly, or as the value of in-esek, `for` reports it at the
;; next read, as an iterator does; the for-clause form checks each time it
;; moves to a new run of storage, so a loop that crosses one reports it.
(test-case "a traversal that races an update raises"
  (define (updated-during traverse)
    (define e (list->esek (range 1000)))
    (lambda ()
      (traverse e
                (lambda (i)
                  (when (= i 0)
                    (esek-set! e 500 'changed))))))
  (for ([traverse (in-list (list (lambda (e hook)
                                   (for ([x e]
                                         [i (in-naturals)])
                                     (hook i)))
                                 (lambda (e hook)
                                   (for ([x (sequence-map values e)]
                                         [i (in-naturals)])
                                     (hook i)))
                                 (lambda (e hook)
                                   (define s (in-esek e))
                                   (for ([x s]
                                         [i (in-naturals)])
                                     (hook i)))
                                 (lambda (e hook)
                                   (define s (in-sek e))
                                   (for ([x s]
                                         [i (in-naturals)])
                                     (hook i)))
                                 (lambda (e hook)
                                   (for ([x (in-esek e)]
                                         [i (in-naturals)])
                                     (hook i)))
                                 (lambda (e hook)
                                   (for ([x (in-sek e 'backward)]
                                         [i (in-naturals)])
                                     (hook i)))))])
    (check-exn exn:fail:contract? (updated-during traverse)))
  ;; reading alone is not an update
  (define e (list->esek (range 1000)))
  (check-equal? (for/sum ([x e]) x) (for/sum ([x (in-range 1000)]) x))
  (check-equal? (for/sum ([x (in-esek e)] [y e]) (- x y)) 0))
