#lang racket/base
;; Lengths at the edges of the representation -- around the short-sequence
;; threshold and around one and two leaf capacities -- under several
;; settings, for split, append, take and drop, the conversions between the
;; flavors, writes through an iterator, and the overlapping bulk copy.

(require rackunit
         racket/list
         sek
         (submod sek config)
         (submod sek validate))

(define (with-settings leaf node threshold thunk)
  (dynamic-wind
   (lambda () (sek-configure! #:leaf-capacity leaf #:node-capacity node #:short-threshold threshold))
   thunk
   (lambda () (sek-configure! #:leaf-capacity 128 #:node-capacity 16 #:short-threshold 32))))

(define (lengths k t)
  (remove-duplicates (filter (lambda (n) (>= n 0))
                             (list 0
                                   1
                                   2
                                   (sub1 t)
                                   t
                                   (add1 t)
                                   (sub1 k)
                                   k
                                   (add1 k)
                                   (sub1 (* 2 k))
                                   (* 2 k)
                                   (add1 (* 2 k))
                                   (+ (* 4 k) 3)))))

(define (check-boundaries k t)
  (for ([n (in-list (lengths k t))])
    (define xs (range n))
    (define p (list->psek xs))
    (for ([i (in-range (add1 n))])
      (define-values (a b) (psek-split p i))
      (sek-validate-psek a)
      (sek-validate-psek b)
      (check-equal? (psek->list a) (take xs i))
      (check-equal? (psek->list b) (drop xs i))
      (check-equal? (psek->list (psek-append b a)) (append (drop xs i) (take xs i)))
      (check-equal? (psek->list (psek-take p i)) (take xs i))
      (check-equal? (psek->list (psek-drop p i)) (drop xs i)))
    ;; push and pop past a capacity at the front, which fills and empties the
    ;; inner chunks and wraps the leaf supports
    (define e (list->esek xs))
    (for ([i (in-range (+ k 3))])
      (esek-cons! e (- -1 i)))
    (for ([i (in-range (+ k 3))])
      (esek-pop-front! e))
    (define snap (esek-snapshot e))
    (for ([v (list (esek-copy e #:mode 'share) (esek-copy e) (psek-edit snap))])
      (esek-add! v 'end)
      (esek-cons! v 'front)
      (check-equal? (esek->list v) (append '(front) xs '(end)))
      (sek-validate-esek v))
    (when (> n 0)
      (define it (sek-iterator e (quotient n 2)))
      (define other (sek-iterator e))
      (sek-iter-set! it 'new)
      (check-equal? (sek-iter-get it) 'new)
      (check-exn exn:fail:contract? (lambda () (sek-iter-get other))))
    (check-equal? (psek->list p) xs)
    (check-equal? (psek->list snap) xs)
    ;; a copy within one sequence reads its source as it was, in both
    ;; directions of overlap
    (for ([start (in-list (if (> n 3)
                              '(0 1 2)
                              '(0)))])
      (define dst (list->esek xs))
      (define before (esek-snapshot dst))
      (define size (- n start))
      (esek-copy! dst start dst 0 size)
      (check-equal? (esek->list dst) (append (take xs start) (take xs size)))
      (check-equal? (psek->list before) xs)
      (define dst2 (list->esek xs))
      (esek-copy! dst2 0 dst2 start n)
      (check-equal? (esek->list dst2) (append (drop xs start) (drop xs size))))
    (define fill (list->esek xs))
    (define fill-before (esek-snapshot fill))
    (esek-fill! fill 'filled)
    (check-equal? (esek->list fill) (make-list n 'filled))
    (check-equal? (psek->list fill-before) xs)))

(for ([settings (in-list '((2 2 0) (3 2 1) (4 3 2) (8 4 6) (128 16 32)))])
  (test-case (format "lengths at the edges, with capacities and threshold ~a" settings)
    (with-settings (car settings)
                   (cadr settings)
                   (caddr settings)
                   (lambda () (check-boundaries (car settings) (caddr settings))))))

(test-case "a sequence made from a chaperoned vector is a copy of it"
  (define raw (vector 1 2 3))
  (define chaperoned (chaperone-vector raw (lambda (v i x) x) (lambda (v i x) x)))
  (define p (vector->psek chaperoned))
  (vector-set! raw 1 'changed)
  (check-equal? (psek->list p) '(1 2 3)))
