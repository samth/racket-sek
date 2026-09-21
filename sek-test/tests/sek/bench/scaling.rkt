#lang racket/base
;; How each operation grows with the size of the sequence.
;;
;; `main.rkt` and `external.rkt` measure operations at one size, which tells
;; you what an operation costs but not which of them will still be affordable
;; when the sequence is a hundred times larger. This runs the same operation at
;; four sizes three orders of magnitude apart and fits a slope through the
;; results on log-log axes, where the slope *is* the asymptotic class:
;;
;;   ~0.0   constant time
;;   ~0.2   grows with log N, at the base these structures use
;;   ~1.0   touches every element
;;
;; Reading the slope beats reading the bound off the documentation, because it
;; measures what the implementation does rather than what it promises -- and a
;; structure whose row is flat at a microbenchmark size can still have a slope
;; of 1, which is the case worth catching.
;;
;;   racket -y scaling.rkt             every operation
;;   racket -y scaling.rkt ref split   only these
;;   racket -y scaling.rkt --quick     stop at 100k
;;
;; Every operation here is size-neutral: an insert is paired with the delete
;; that undoes it, a push with a pop. The sequence is therefore the same length
;; at the end of a repetition as at the start, so what is measured is one
;; operation at one size rather than a structure that grew while being timed.

(require racket/list
         racket/string
         racket/fixnum
         "main.rkt") ; impl records, all-impls, measure, fmt

(provide scaling-operations
         run-scaling!)

(define SIZES '(1000 10000 100000 1000000))
(define QUICK-SIZES '(1000 10000 100000))

;; ------------------------------------------------------------------ the rows
;;
;; Each entry: what it needs, and a procedure from an impl and a size to a
;; thunk that performs one size-neutral operation.

(struct row (name blurb needs make) #:transparent)

(define (index-for n k)
  (fxmodulo (fx* k 977) (fxmax 1 n)))

(define ROWS
  (list (row "ref"
             "read at a rotating index"
             '(ref construct len)
             (lambda (i n)
               (define s ((impl-construct i) n (lambda (k) k)))
               (lambda (k) ((impl-ref i) s (index-for n k)))))
        (row "set"
             "write at a rotating index"
             '(set ref construct len)
             (lambda (i n)
               (define s ((impl-construct i) n (lambda (k) k)))
               (lambda (k) ((impl-set i) s (index-for n k) 7))))
        (row "push+pop"
             "add at the end, then take it off again"
             '(push-back pop-back construct)
             (lambda (i n)
               (define s ((impl-construct i) n (lambda (k) k)))
               (lambda (k) ((impl-pop-back i) ((impl-push-back i) s k)))))
        (row "split+append"
             "split at a rotating point and rejoin"
             '(split append construct len)
             (lambda (i n)
               (define s ((impl-construct i) n (lambda (k) k)))
               (lambda (k)
                 (define-values (a b) ((impl-split i) ((impl-fresh i) s) (index-for n k)))
                 ((impl-append i) a b))))
        (row "checkpoint"
             "take a copy you could go back to"
             '(construct)
             (lambda (i n)
               (define s ((impl-construct i) n (lambda (k) k)))
               (lambda (k) ((impl-fresh i) s))))))

(define (row-supported? r i)
  (for/and ([need (in-list (row-needs r))])
    (case need
      [(ref) (and (impl-ref i) #t)]
      [(set) (and (impl-set i) #t)]
      [(push-back) (and (impl-push-back i) #t)]
      [(pop-back) (and (impl-pop-back i) #t)]
      [(split) (and (impl-split i) #t)]
      [(append) (and (impl-append i) #t)]
      [(construct) (and (impl-construct i) #t)]
      [(len) (and (impl-len i) #t)]
      [else #t])))

;; ------------------------------------------------------------------ fitting

;; Least-squares slope of log(cost) against log(size). Four points over three
;; decades is enough to separate constant from logarithmic from linear, which
;; is all this is asked to do.
(define (log-log-slope sizes costs)
  (define pts
    (for/list ([x (in-list sizes)]
               [y (in-list costs)]
               #:when (and y (> y 0)))
      (cons (log x) (log y))))
  (cond
    [(< (length pts) 2) #f]
    [else
     (define n (length pts))
     (define mx (/ (for/sum ([p pts]) (car p)) n))
     (define my (/ (for/sum ([p pts]) (cdr p)) n))
     (define num (for/sum ([p pts]) (* (- (car p) mx) (- (cdr p) my))))
     (define den (for/sum ([p pts]) (sqr (- (car p) mx))))
     (and (> den 0) (/ num den))]))

(define (sqr x)
  (* x x))

;; What a slope means, in words, so the number does not have to be read cold.
(define (slope-name sl)
  (cond
    [(not sl) ""]
    [(< sl 0.10) "constant"]
    [(< sl 0.45) "log n"]
    [(< sl 0.8) "sublinear"]
    [(< sl 1.3) "linear"]
    [else "worse"]))

;; ------------------------------------------------------------------ running

(define (~w t w)
  (let ([t (format "~a" t)])
    (if (>= (string-length t) w)
        (string-append t " ")
        (string-append (make-string (- w (string-length t)) #\space) t))))

;; A repetition count that keeps a linear operation at a million elements from
;; running for minutes, while still giving a constant-time one enough work to
;; measure.
(define (reps-for n)
  (max 3 (min 200000 (quotient 20000000 (max n 1)))))

(define (run-row r sizes)
  (printf "\n~a: ~a, ns per operation\n" (row-name r) (row-blurb r))
  (printf "~a" (~w "" 20))
  (for ([n (in-list sizes)])
    (printf "~a"
            (~w (if (>= n 1000000)
                    "1M"
                    (format "~ak" (quotient n 1000)))
                12)))
  (printf "~a~a\n" (~w "slope" 9) (~w "" 11))
  (for ([i (in-list all-impls)])
    (cond
      [(not (row-supported? r i)) (void)]
      [else
       (printf "~a" (~w (impl-name i) 20))
       (define costs
         (for/list ([n (in-list sizes)])
           (define cost
             (with-handlers ([(lambda (_) #t) (lambda (_) #f)])
               (define thunk ((row-make r) i n))
               (collect-garbage)
               (define reps (reps-for n))
               ;; `measure` already returns nanoseconds per operation,
               ;; given how many one call to the thunk performs.
               (measure reps
                        (lambda ()
                          (for ([k (in-range reps)])
                            (thunk k))))))
           (printf "~a"
                   (~w (if cost
                           (fmt cost)
                           "err")
                       12))
           cost))
       (define sl (log-log-slope sizes costs))
       (printf "~a~a\n"
               (~w (if sl
                       (real->decimal-string sl 2)
                       "-")
                   9)
               (~w (slope-name sl) 11))])))

(define (scaling-operations)
  (map row-name ROWS))

(define (run-scaling! args)
  (define sizes (if (member "--quick" args) QUICK-SIZES SIZES))
  (define asked (filter (lambda (a) (not (regexp-match #rx"^--" a))) args))
  (define rows
    (if (null? asked)
        ROWS
        (for/list ([a (in-list asked)]
                   #:when (member a (map row-name ROWS)))
          (findf (lambda (r) (string=? (row-name r) a)) ROWS))))
  (printf "operation scaling -- Racket ~a\n" (version))
  (cond
    [(null? rows) (printf "no such operation; try: ~a\n" (string-join (scaling-operations) " "))]
    [else
     (for ([r (in-list rows)])
       (run-row r sizes))
     (printf "\nslope is the least-squares fit of log(cost) against log(size):\n")
     (printf "  about 0.0 constant, 0.2 log n at these bases, 1.0 touches every element
")]))

(module+ main
  (run-scaling! (vector->list (current-command-line-arguments))))
