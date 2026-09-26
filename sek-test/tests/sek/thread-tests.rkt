#lang racket/base
;; Persistent sequences may be shared between threads: pushing onto the same
;; persistent sequence from two parallel threads gives each thread the
;; sequence it asked for.  The in-place ("monotonic") push that makes a
;; persistent push cheap writes into storage that other sequences share, so
;; this is the property a data race could break.
;;
;; The threads run in lockstep, one base sequence at a time, so that both try
;; to extend the same storage at the same moment.  Runs only where `thread`
;; takes #:pool, i.e. where threads can run in parallel.
(require rackunit
         racket/list
         sek)

(define parallel?
  (let-values ([(required accepted) (procedure-keywords thread)])
    (and (list? accepted) (memq '#:pool accepted) #t)))

(define (parallel-thread thunk)
  (thread thunk #:pool 'own))

;; Run (work 0 i) and (work 1 i) for each i in [0, n), in two parallel
;; threads that start each i together.  Returns the two vectors of results.
(define (lockstep n work)
  (define arrived (vector 0 0))
  (define results (vector (make-vector n #f) (make-vector n #f)))
  (define (worker me)
    (define other (- 1 me))
    (define out (vector-ref results me))
    (for ([i (in-range n)])
      (vector-set! arrived me (add1 i))
      (let spin () (when (< (vector-ref arrived other) (add1 i)) (spin)))
      (vector-set! out i (work me i))))
  (define ts (for/list ([me (in-range 2)])
               (parallel-thread (lambda () (worker me)))))
  (for-each thread-wait ts)
  (values (vector-ref results 0) (vector-ref results 1)))

(define (base-of n)
  ;; built by pushes, so that its chunks have room to be extended in place
  (for/fold ([s empty-pseq]) ([i (in-range n)])
    (pseq-add s i)))

(when parallel?
  (define N 20000)

  (test-case "pushes at the back of a shared sequence"
    (define bases (for/vector ([i (in-range N)]) (base-of (+ 30 (modulo i 200)))))
    (define-values (as bs)
      (lockstep N (lambda (me i) (pseq-add (vector-ref bases i) (if (= me 0) 'a 'b)))))
    (for ([a (in-vector as)] [b (in-vector bs)] [base (in-vector bases)])
      (check-equal? (pseq->list a) (append (pseq->list base) '(a)))
      (check-equal? (pseq->list b) (append (pseq->list base) '(b)))))

  (test-case "pushes at the front against pushes at the back"
    (define bases (for/vector ([i (in-range N)]) (base-of (+ 30 (modulo i 200)))))
    (define-values (fs bs)
      (lockstep N (lambda (me i)
                    (define base (vector-ref bases i))
                    (for/fold ([s (if (= me 0) (pseq-cons base 'f) (pseq-add base 'b))])
                              ([j (in-range 10)])
                      (if (even? j) (pseq-cons s j) (pseq-add s j))))))
    (define (expected base first)
      (for/fold ([l (if (eq? first 'f)
                        (cons 'f (pseq->list base))
                        (append (pseq->list base) '(b)))])
                ([j (in-range 10)])
        (if (even? j) (cons j l) (append l (list j)))))
    (for ([f (in-vector fs)] [b (in-vector bs)] [base (in-vector bases)])
      (sek-validate-pseq f)
      (sek-validate-pseq b)
      (check-equal? (pseq->list f) (expected base 'f))
      (check-equal? (pseq->list b) (expected base 'b))))

  (test-case "an ephemeral sequence and its snapshot, extended by two threads"
    ;; after a snapshot the two share their chunks, so the ephemeral
    ;; sequence's pushes are persistent pushes into shared storage too
    (define e (list->eseq (range 1000)))
    (define s (eseq-snapshot e))
    (define ta (parallel-thread (lambda () (for ([i (in-range 5000)]) (eseq-push-back! e (- -1 i))))))
    (define versions #f)
    (define tb (parallel-thread
                (lambda ()
                  (set! versions (for/list ([i (in-range 200)]) (pseq-add s i))))))
    (thread-wait ta)
    (thread-wait tb)
    (check-equal? (eseq->list e) (append (range 1000) (for/list ([i (in-range 5000)]) (- -1 i))))
    (check-equal? (pseq->list s) (range 1000))
    (for ([v (in-list versions)] [i (in-naturals)])
      (check-equal? (pseq->list v) (append (range 1000) (list i))))))
