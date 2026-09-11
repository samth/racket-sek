#lang racket/base
;; Tests for the growable `array` contender in main.rkt.
;;
;;   raco test -y bench/array-tests.rkt
;;
;; Two things are checked: that the operations agree with a list model, and
;; that the memory-safety invariant
;;
;;     (<= (arr-n a) (vector-length (arr-vec a)))
;;
;; holds after every step -- including under concurrent mutation from futures,
;; which is the case it exists for.  Reads are `unsafe-vector*-ref`, so a
;; violation is not a wrong answer but a read past the end of the vector.

(require rackunit
         racket/list
         racket/future
         racket/unsafe/ops
         "main.rkt")

(define (arr->list a)
  (for/list ([x (in-arr a)])
    x))

(define (invariant-holds? a)
  (<= (arr-n a) (vector-length (arr-vec a))))

;; ----------------------------------------------------------- the plain model

(test-case "operations agree with a list model, and the invariant holds after each"
  (for ([trial (in-range 200)])
    (define model '())
    (define a (arr-empty 1))
    (for ([step (in-range 60)])
      (case (random (if (null? model) 2 6))
        [(0 1)
         (define x (random 1000))
         (arr-push-back! a x)
         (set! model (append model (list x)))]
        [(2)
         (arr-push-front! a (random 1000))
         (set! model (cons (arr-ref a 0) model))]
        [(3)
         (set-arr-n! a (sub1 (arr-n a)))
         (set! model (take model (sub1 (length model))))]
        [(4)
         (arr-pop-front! a)
         (set! model (cdr model))]
        [(5)
         (define i (random (length model)))
         (define x (random 1000))
         (arr-set! a i x)
         (set! model (list-set model i x))])
      (check-true (invariant-holds? a) "invariant after step")
      (check-equal? (arr->list a) model))))

(test-case "copy is independent of the original"
  (define a (arr-of (build-list 500 values)))
  (define b (arr-copy a))
  (arr-set! b 0 'CHANGED)
  (check-equal? (arr-ref a 0) 0)
  (check-equal? (arr-ref b 0) 'CHANGED))

(test-case "traversal visits exactly the live prefix"
  (define a (arr-of (build-list 300 values)))
  (set-arr-n! a 100)
  (check-equal? (arr->list a) (build-list 100 values))
  (define seen '())
  (arr-for-each a (lambda (x) (set! seen (cons x seen))))
  (check-equal? (reverse seen) (build-list 100 values)))

;; ------------------------------------------------------------- under futures

;; Grow from several futures at once.  What is being checked is not that every
;; push survives -- it will not, since a slot is never reserved -- but that the
;; count never runs ahead of the vector, so that a concurrent `arr-ref` cannot
;; read past the end.  A watcher future samples the invariant while the writers
;; run, because a violation is a transient state that a check afterwards would
;; miss.
(define (stress-invariant! make-array push! read-n read-vec #:writers [writers 4])
  (define a (make-array))
  (define violated (box #f))
  (define stop (box #f))
  (define watcher
    (future (lambda ()
              (let loop ([spins 0])
                (unless (or (unbox stop) (> spins 20000000))
                  (define n (read-n a))
                  (define v (read-vec a))
                  (when (> n (vector-length v))
                    (set-box! violated #t))
                  (loop (add1 spins)))))))
  (define fs
    (for/list ([w (in-range writers)])
      (future (lambda ()
                (for ([k (in-range 20000)])
                  (push! a k))))))
  (for-each touch fs)
  (set-box! stop #t)
  (touch watcher)
  (values a (unbox violated)))

(test-case "the invariant survives concurrent growth"
  (check-true (futures-enabled?) "futures are available")
  (for ([round (in-range 10)])
    (define-values (a violated)
      (stress-invariant! (lambda () (arr-empty 8)) arr-push-back! arr-n arr-vec))
    (check-false violated "arr-n ran ahead of the vector")
    (check-true (invariant-holds? a) "invariant after the writers finished")
    ;; every slot the count claims is readable, which is the point
    (check-equal? (arr-n a) (length (arr->list a)))))

;; The same stress against the version with no ordering discipline, to show the
;; test can tell the difference.  It is allowed to pass -- a race need not
;; happen -- so this only reports, and never fails the suite on timing.
(module+ main
  (define violations
    (for/sum
     ([round (in-range 20)])
     (define-values (a violated)
       (stress-invariant! (lambda () (usarr (make-vector 8 0) 0)) usarr-push! usarr-n usarr-vec))
     (if violated 1 0)))
  (printf "unsynchronised: invariant violated in ~a of 20 rounds\n" violations)
  (define safe-violations
    (for/sum ([round (in-range 20)])
             (define-values (a violated)
               (stress-invariant! (lambda () (arr-empty 8)) arr-push-back! arr-n arr-vec))
             (if violated 1 0)))
  (printf "memory-safe:    invariant violated in ~a of 20 rounds\n" safe-violations))
