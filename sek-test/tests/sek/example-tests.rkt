#lang racket/base
;; The examples from the documentation, plus a few larger-scale checks that run
;; at the default chunk capacities (so the trees reach depth 3 or 4).

(require rackunit
         racket/list
         sek
         (submod sek validate))

(module+ test
  ;; ---- the overview example ------------------------------------------------
  (define p (list->psek '(1 2 3 4 5)))
  (check-equal? (psek->list (psek-push-front p 0)) '(0 1 2 3 4 5))
  (check-equal? (psek->list p) '(1 2 3 4 5))

  (define e (psek-edit p))
  (esek-push-back! e 6)
  (esek-set! e 0 'a)
  (define q (esek-snapshot e))
  (check-equal? (psek->list q) '(a 2 3 4 5 6))
  (check-equal? (psek->list p) '(1 2 3 4 5))

  ;; ---- generic sequence and equality integration --------------------------
  (check-equal? (for/list ([x (in-psek (psek 1 2 3))])
                  x)
                '(1 2 3))
  (check-equal? (for/list ([x (psek 1 2 3)])
                  x)
                '(1 2 3))
  (check-equal? (for/list ([x (esek 1 2 3)])
                  x)
                '(1 2 3))
  (check-equal? (psek 1 2 3) (list->psek '(1 2 3)))
  (check-not-equal? (psek 1 2 3) (psek 1 2))
  (check-equal? (hash-ref (hash (psek 1 2) 'yes) (list->psek '(1 2))) 'yes)
  (check-equal? (psek->list (psek-map (psek 1 2 3) add1)) '(2 3 4))

  ;; ---- errors -------------------------------------------------------------
  (check-exn exn:fail? (lambda () (psek-pop-front empty-psek)))
  (check-exn exn:fail? (lambda () (psek-ref (psek 1 2) 2)))
  (check-exn exn:fail? (lambda () (esek-pop-back! (make-esek))))
  (check-exn exn:fail? (lambda () (esek-ref (esek 1) 5)))

  ;; ---- a larger sequence at the default capacities -------------------------
  (define N 100000)
  (define big (list->psek (build-list N values)))
  (void (sek-validate-psek big))
  (check-equal? (psek-length big) N)
  (check-equal? (psek-ref big 0) 0)
  (check-equal? (psek-ref big (sub1 N)) (sub1 N))
  (check-equal? (psek-ref big 54321) 54321)
  (check-equal? (psek-first big) 0)
  (check-equal? (psek-last big) (sub1 N))

  ;; repeated splitting and reassembly must reproduce the original
  (define pieces
    (let loop ([s big]
               [cuts '(7 1000 13 50000 3)]
               [acc '()])
      (cond
        [(null? cuts) (reverse (cons s acc))]
        [else
         (define-values (a b) (psek-split s (car cuts)))
         (void (sek-validate-psek a))
         (void (sek-validate-psek b))
         (loop b (cdr cuts) (cons a acc))])))
  (check-equal? (apply + (map psek-length pieces)) N)
  (define rejoined
    (for/fold ([s empty-psek]) ([piece (in-list pieces)])
      (psek-append s piece)))
  (void (sek-validate-psek rejoined))
  (check-equal? (psek-length rejoined) N)
  (check-equal? (psek->list rejoined) (build-list N values))
  ;; indexing still works after concatenation, when chunks are no longer packed
  (for ([i (in-list '(0 1 6 7 8 1006 60000 99999))])
    (check-equal? (psek-ref rejoined i) i))

  ;; iteration streams, so taking a prefix does not walk the whole sequence
  (check-equal? (for/list ([x (in-psek big)] [_ (in-range 5)]) x) '(0 1 2 3 4))
  (check-equal? (for/list ([x (in-psek rejoined)] [_ (in-range 3)]) x) '(0 1 2))
  (check-equal? (for/first ([x (in-psek big)]) x) 0)
  (check-equal? (for/list ([x (in-esek (list->esek '(1 2 3)))]) x) '(1 2 3))

  ;; ---- the iterator constructor takes a start index, -1 to the length ----
  (for ([s (list (list->psek '(a b c)) (list->esek '(a b c)))])
    (check-equal? (for/list ([i (in-range -1 4)])
                    (define it (sek-iterator s i))
                    (list (sek-iter-index it)
                          (and (not (sek-iter-finished? it)) (sek-iter-get it))))
                  '((-1 #f) (0 a) (1 b) (2 c) (3 #f)))
    (check-exn exn:fail:contract? (lambda () (sek-iterator s 4)))
    (check-exn exn:fail:contract? (lambda () (sek-iterator s -2))))
  (check-true (sek-iter-finished? (sek-iterator empty-psek)))
  (let ([it (sek-iterator (list->esek '(1 2 3)) 2)])
    (sek-iter-move! it 'backward)
    (check-equal? (sek-iter-get it) 2))

  ;; ---- a queue built ephemerally, snapshotted midway ----------------------
  (define qe (make-esek))
  (for ([i (in-range 20000)])
    (esek-push-back! qe i))
  (define half (esek-snapshot qe))
  (for ([i (in-range 10000)])
    (esek-pop-front! qe))
  (for ([i (in-range 5000)])
    (esek-push-front! qe (- (add1 i))))
  (void (sek-validate-esek qe))
  (void (sek-validate-psek half))
  (check-equal? (psek-length half) 20000)
  (check-equal? (psek->list half) (build-list 20000 values))
  (check-equal? (esek-length qe) 15000)
  (check-equal? (esek->list qe)
                (append (reverse (build-list 5000 (lambda (i) (- (add1 i)))))
                        (build-list 10000 (lambda (i) (+ i 10000))))))
