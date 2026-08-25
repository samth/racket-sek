#lang racket/base
;; The benchmark of §4.3: starting from an empty stack, repeat p/n times
;; "n pushes followed by n pops", and report nanoseconds per push operation.
;; p is fixed so that every run does the same amount of work; n is the peak
;; length of the stack.
;;
;; Run with:  racket -y bench.rkt

(require racket/list
         data/gvector
         "main.rkt")

(define p (make-parameter 2000000))

;; Each contender is (name make-empty push! pop!), where push!/pop! take and
;; return the current version so that persistent structures fit the same shape.
(define contenders
  (list (list "eseq (ephemeral Sek)"
              (lambda () (make-eseq))
              (lambda (s x)
                (eseq-push-back! s x)
                s)
              (lambda (s)
                (eseq-pop-back! s)
                s))
        (list "gvector"
              (lambda () (make-gvector))
              (lambda (s x)
                (gvector-add! s x)
                s)
              (lambda (s)
                (gvector-remove-last! s)
                s))
        (list "mutable box of list"
              (lambda () (box '()))
              (lambda (s x)
                (set-box! s (cons x (unbox s)))
                s)
              (lambda (s)
                (set-box! s (cdr (unbox s)))
                s))
        (list "pseq (persistent Sek)"
              (lambda () empty-pseq)
              (lambda (s x) (pseq-push-back s x))
              (lambda (s)
                (let-values ([(x s*) (pseq-pop-back s)])
                  s*)))
        (list "immutable list" (lambda () '()) (lambda (s x) (cons x s)) (lambda (s) (cdr s)))))

(define (run make-empty push! pop! n total)
  (define rounds (max 1 (quotient total n)))
  (define s0 (make-empty))
  (define start (current-inexact-monotonic-milliseconds))
  (let loop ([r 0]
             [s s0])
    (cond
      [(= r rounds) (void s)]
      [else
       (define s1
         (for/fold ([s s]) ([i (in-range n)])
           (push! s i)))
       (define s2
         (for/fold ([s s1]) ([i (in-range n)])
           (pop! s)))
       (loop (add1 r) s2)]))
  (define elapsed (- (current-inexact-monotonic-milliseconds) start))
  ;; nanoseconds per push (each round does n pushes and n pops)
  (/ (* elapsed 1e6) (* rounds n)))

(define (main)
  (define lengths '(10 100 1000 10000 100000 1000000))
  (printf "~a pushes per configuration; ns per push+pop pair\n\n" (p))
  (printf "~a" (~pad "n" 12))
  (for ([c (in-list contenders)])
    (printf "~a" (~pad (car c) 24)))
  (newline)
  (for ([n (in-list lengths)])
    (printf "~a" (~pad (number->string n) 12))
    (for ([c (in-list contenders)])
      (define ns (run (cadr c) (caddr c) (cadddr c) n (p)))
      (printf "~a" (~pad (real->decimal-string ns 1) 24)))
    (newline)))

(define (~pad s w)
  (define str
    (if (string? s)
        s
        (format "~a" s)))
  (string-append str (make-string (max 1 (- w (string-length str))) #\space)))

;; A second benchmark: how much the segment-based traversal buys over walking
;; the sequence one index at a time.
(define (bench-iteration)
  (define n 1000000)
  (define reps 10)
  (define p (list->pseq (build-list n values)))
  (define v (build-vector n values))
  (define l (build-list n values))
  (define (timed name thunk)
    (collect-garbage)
    (define start (current-inexact-monotonic-milliseconds))
    (for ([_ (in-range reps)]) (thunk))
    (define ms (- (current-inexact-monotonic-milliseconds) start))
    (printf "~a~a ns/element\n" (~pad name 34)
            (real->decimal-string (/ (* ms 1e6) (* reps n)) 2)))
  (printf "\nsumming ~a elements, ~a times\n\n" n reps)
  (timed "sek-fold-left (segments)"
         (lambda () (sek-fold-left p + 0)))
  (timed "iterator, one element at a time"
         (lambda ()
           (define it (sek-iterator p 'forward))
           (let loop ([acc 0])
             (if (sek-iter-finished? it)
                 acc
                 (loop (+ acc (sek-iter-get-and-move! it 'forward)))))))
  (timed "pseq-ref at each index"
         (lambda () (for/fold ([acc 0]) ([i (in-range n)]) (+ acc (pseq-ref p i)))))
  (timed "vector"
         (lambda () (for/fold ([acc 0]) ([x (in-vector v)]) (+ acc x))))
  (timed "list"
         (lambda () (for/fold ([acc 0]) ([x (in-list l)]) (+ acc x)))))

(module+ main
  (collect-garbage)
  (main)
  (bench-iteration))
