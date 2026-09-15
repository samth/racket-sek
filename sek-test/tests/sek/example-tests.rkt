#lang racket/base
;; The examples from the documentation, plus a few larger-scale checks that run
;; at the default chunk capacities (so the trees reach depth 3 or 4).

(require rackunit
         racket/list
         sek)

(module+ test
  ;; ---- the overview example ------------------------------------------------
  (define p (list->pseq '(1 2 3 4 5)))
  (check-equal? (pseq->list (pseq-push-front p 0)) '(0 1 2 3 4 5))
  (check-equal? (pseq->list p) '(1 2 3 4 5))

  (define e (pseq-edit p))
  (eseq-push-back! e 6)
  (eseq-set! e 0 'a)
  (define q (eseq-snapshot e))
  (check-equal? (pseq->list q) '(a 2 3 4 5 6))
  (check-equal? (pseq->list p) '(1 2 3 4 5))

  ;; ---- generic sequence and equality integration --------------------------
  (check-equal? (for/list ([x (in-pseq (pseq 1 2 3))])
                  x)
                '(1 2 3))
  (check-equal? (for/list ([x (pseq 1 2 3)])
                  x)
                '(1 2 3))
  (check-equal? (for/list ([x (eseq 1 2 3)])
                  x)
                '(1 2 3))
  (check-equal? (pseq 1 2 3) (list->pseq '(1 2 3)))
  (check-not-equal? (pseq 1 2 3) (pseq 1 2))
  (check-equal? (hash-ref (hash (pseq 1 2) 'yes) (list->pseq '(1 2))) 'yes)
  (check-equal? (pseq->list (pseq-map (pseq 1 2 3) add1)) '(2 3 4))

  ;; ---- errors -------------------------------------------------------------
  (check-exn exn:fail? (lambda () (pseq-pop-front empty-pseq)))
  (check-exn exn:fail? (lambda () (pseq-ref (pseq 1 2) 2)))
  (check-exn exn:fail? (lambda () (eseq-pop-back! (make-eseq))))
  (check-exn exn:fail? (lambda () (eseq-ref (eseq 1) 5)))

  ;; ---- a larger sequence at the default capacities -------------------------
  (define N 100000)
  (define big (list->pseq (build-list N values)))
  (void (sek-validate-pseq big))
  (check-equal? (pseq-length big) N)
  (check-equal? (pseq-ref big 0) 0)
  (check-equal? (pseq-ref big (sub1 N)) (sub1 N))
  (check-equal? (pseq-ref big 54321) 54321)
  (check-equal? (pseq-first big) 0)
  (check-equal? (pseq-last big) (sub1 N))

  ;; repeated splitting and reassembly must reproduce the original
  (define pieces
    (let loop ([s big]
               [cuts '(7 1000 13 50000 3)]
               [acc '()])
      (cond
        [(null? cuts) (reverse (cons s acc))]
        [else
         (define-values (a b) (pseq-split s (car cuts)))
         (void (sek-validate-pseq a))
         (void (sek-validate-pseq b))
         (loop b (cdr cuts) (cons a acc))])))
  (check-equal? (apply + (map pseq-length pieces)) N)
  (define rejoined
    (for/fold ([s empty-pseq]) ([piece (in-list pieces)])
      (pseq-append s piece)))
  (void (sek-validate-pseq rejoined))
  (check-equal? (pseq-length rejoined) N)
  (check-equal? (pseq->list rejoined) (build-list N values))
  ;; indexing still works after concatenation, when chunks are no longer packed
  (for ([i (in-list '(0 1 6 7 8 1006 60000 99999))])
    (check-equal? (pseq-ref rejoined i) i))

  ;; iteration streams, so taking a prefix does not walk the whole sequence
  (check-equal? (for/list ([x (in-pseq big)] [_ (in-range 5)]) x) '(0 1 2 3 4))
  (check-equal? (for/list ([x (in-pseq rejoined)] [_ (in-range 3)]) x) '(0 1 2))
  (check-equal? (for/first ([x (in-pseq big)]) x) 0)
  (check-equal? (for/list ([x (in-eseq (list->eseq '(1 2 3)))]) x) '(1 2 3))

  ;; ---- a queue built ephemerally, snapshotted midway ----------------------
  (define qe (make-eseq))
  (for ([i (in-range 20000)])
    (eseq-push-back! qe i))
  (define half (eseq-snapshot qe))
  (for ([i (in-range 10000)])
    (eseq-pop-front! qe))
  (for ([i (in-range 5000)])
    (eseq-push-front! qe (- (add1 i))))
  (void (sek-validate-eseq qe))
  (void (sek-validate-pseq half))
  (check-equal? (pseq-length half) 20000)
  (check-equal? (pseq->list half) (build-list 20000 values))
  (check-equal? (eseq-length qe) 15000)
  (check-equal? (eseq->list qe)
                (append (reverse (build-list 5000 (lambda (i) (- (add1 i)))))
                        (build-list 10000 (lambda (i) (+ i 10000)))))

  ;; ---- transient arrays ---------------------------------------------------
  (define a (make-earray 5000 'init))
  (for ([i (in-range 5000)])
    (earray-set! a i i))
  (define snap (earray-snapshot a))
  (for ([i (in-range 5000)])
    (earray-set! a i 'overwritten))
  (check-equal? (parray-ref snap 2500) 2500)
  (check-equal? (earray-ref a 2500) 'overwritten)
  (check-equal? (parray-ref (parray-set snap 0 'x) 0) 'x)
  (check-equal? (parray-ref snap 0) 0))
