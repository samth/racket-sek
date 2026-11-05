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

;; An update to an ephemeral sequence during a traversal is an error, which
;; the traversal reports before it hands over another element.  A traversal
;; that read ahead and did not check would show the update, and after a pop
;; could hand over the private value left in the emptied slot.
(define-syntax-rule (traversals (e see!) [who expr] ...)
  (list (list 'who (lambda (e see!) expr)) ...))

(define all-traversals
  (traversals
   (e see!)
   [in-esek (for ([x (in-esek e)]) (see! x))]
   [in-esek (for ([x (in-esek e 'backward)]) (see! x))]
   [in-sek (for ([x (in-sek e)]) (see! x))]
   [in-esek (let ([s (in-esek e)]) (for ([x s]) (see! x)))]
   [in-sek (let ([s (in-sek e)]) (for ([x s]) (see! x)))]
   [in-esek (for ([x e]) (see! x))]
   [esek-for-each (esek-for-each e see!)]
   [sek-for-each (sek-for-each e see!)]
   [sek-for-each (sek-for-each e see! 'backward)]
   [sek-for-each/index (sek-for-each/index e (lambda (i x) (see! x)))]
   [sek-fold-left (sek-fold-left e (lambda (a x) (see! x) a) 0)]
   [sek-fold-right (sek-fold-right e (lambda (x a) (see! x) a) 0)]
   [sek-map (sek-map e (lambda (x) (see! x) x))]
   [sek-map/index (sek-map/index e (lambda (i x) (see! x) x))]
   [sek-filter (sek-filter e (lambda (x) (see! x) #t))]
   [sek-filter-map (sek-filter-map e (lambda (x) (see! x) x))]
   [sek-partition (sek-partition e (lambda (x) (see! x) #t))]
   [sek-append-map (sek-append-map e (lambda (x) (see! x) (psek x)))]
   [sek-uniq (sek-uniq e (lambda (a b) (see! b) #f))]
   [sek-find (sek-find e (lambda (x) (see! x) #f))]
   [sek-find-index (sek-find-index e (lambda (x) (see! x) #f))]
   [sek-find-map (sek-find-map e (lambda (x) (see! x) #f))]
   [sek-for-all? (sek-for-all? e (lambda (x) (see! x) #t))]
   [sek-exists? (sek-exists? e (lambda (x) (see! x) #f))]
   [sek-member? (sek-member? e 'z (lambda (v x) (see! x) #f))]
   [sek-index-of (sek-index-of e 'z (lambda (v x) (see! x) #f))]
   [sek-for-each2 (sek-for-each2 e (list->esek (range 300)) (lambda (x y) (see! x)))]
   [sek-fold-left2 (sek-fold-left2 e (list->psek (range 300)) (lambda (a x y) (see! x) a) 0)]
   [sek-fold-right2 (sek-fold-right2 e (list->psek (range 300)) (lambda (x y a) (see! x) a) 0)]
   [sek-for-all2? (sek-for-all2? e (list->psek (range 300)) (lambda (x y) (see! x) #t))]
   [sek-exists2? (sek-exists2? e (list->psek (range 300)) (lambda (x y) (see! x) #f))]
   [sek-equal? (sek-equal? e (list->psek (range 300)) (lambda (x y) (see! x) #t))]
   [sek-map2 (sek-map2 e (list->psek (range 300)) (lambda (x y) (see! x) x))]
   [sek-compare (sek-compare e (list->psek (range 300)) (lambda (x y) (see! x) 0))]
   [sek-merge (sek-merge e (list->psek (range 300)) (lambda (x y) (see! x) #f))]))

(test-case "a traversal reports an update before it hands over another element"
  (for* ([t (in-list all-traversals)]
         [update (in-list (list (lambda (e) (esek-pop-back! e))
                                (lambda (e) (esek-pop-front! e))
                                (lambda (e) (esek-set! e 150 'changed))
                                (lambda (e) (esek-add! e 'added))
                                (lambda (e) (esek-clear! e))))])
    (define who (car t))
    (define e (list->esek (range 300)))
    (define seen '())
    (define (see! x)
      (when (null? seen) (update e))
      (set! seen (cons x seen)))
    (check-exn (lambda (x)
                 (and (exn:fail:contract? x)
                      (regexp-match? (string-append "^" (regexp-quote (symbol->string who)) ":")
                                     (exn-message x))))
               (lambda () ((cadr t) e see!))
               (format "~a" who))
    (check-equal? (length seen) 1 (format "~a" who))))

(test-case "reading is not an update"
  (define e (list->esek (range 1000)))
  (check-equal? (for/sum ([x e]) x) (for/sum ([x (in-range 1000)]) x))
  (check-equal? (for/sum ([x (in-esek e)] [y e]) (- x y)) 0)
  (check-equal? (sek-fold-left e (lambda (a x) (esek-snapshot e) (+ a x)) 0) 499500)
  (check-equal? (sek-map e (lambda (x) (esek-ref e x))) e))

