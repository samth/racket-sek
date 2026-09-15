#lang racket/base
;; The classic Scheme nqueens benchmark, over several sequence
;; representations.
;;
;;   racket -y nqueens.rkt          both formulations, 10000 repetitions
;;   racket -y nqueens.rkt 100      fewer repetitions
;;
;; The program is the one from racket-benchmarks/tests/racket/benchmarks/
;; common/nqueens.sch, which counts the solutions to the 8-queens problem and
;; repeats that 10000 times.  It is written against pairs, and everything it
;; does -- null?, car, cdr, cons, append -- is something every sequence type
;; here can do, so the first table is that program with the pair operations
;; swapped out, one variant per structure.
;;
;; That is a demanding translation for a mutable structure, because the search
;; keeps three sequences alive across two recursive calls and so needs every
;; "cons" and "append" to leave its argument alone.  A mutable sequence has to
;; copy where a pair list shares.  The second table therefore also solves the
;; same problem the way one would actually write it with a mutable sequence:
;; one stack of placed rows, pushed and popped as the search walks the tree.

(require racket/treelist
         racket/mutable-treelist
         racket/vector
         data/gvector
         "../sek-lib/sek/main.rkt"
         (only-in "main.rkt" record-table! dump-json! current-scenario))

;; ------------------------------------------------------- the classic program

;; Generates the benchmark specialized to one set of operations, so that each
;; variant compiles to direct calls rather than dispatching through a table.
(define-syntax-rule (define-nqueens name (empty empty? fst rst cns app))
  (define (name n)
    (define (one-to n)
      (let loop ([i n]
                 [l empty])
        (if (= i 0)
            l
            (loop (- i 1) (cns i l)))))
    (define (ok? row dist placed)
      (if (empty? placed)
          #t
          (and (not (= (fst placed) (+ row dist)))
               (not (= (fst placed) (- row dist)))
               (ok? row (+ dist 1) (rst placed)))))
    (define (try-it x y z)
      (if (empty? x)
          (if (empty? y) 1 0)
          (+ (if (ok? (fst x) 1 z)
                 (try-it (app (rst x) y) empty (cns (fst x) z))
                 0)
             (try-it (rst x) (cns (fst x) y) z))))
    (try-it (one-to n) empty empty)))

;; pairs: the original
(define-nqueens nqueens/pairs ('() null? car cdr cons append))

;; vector: immutable in use, so rest, cons and append all copy -- which at
;; eight elements is a handful of words and perfectly competitive
(define (vec-rest v)
  (vector-copy v 1))
(define (vec-cons x v)
  (vector-append (vector x) v))
(define-nqueens nqueens/vector
                ('#() (lambda (v) (eqv? 0 (vector-length v))) (lambda (v) (vector-ref v 0))
                      vec-rest vec-cons vector-append))

;; treelist: an immutable structure, so the translation is direct
(define (tl-rest t)
  (treelist-rest t))
(define (tl-cons x t)
  (treelist-cons t x))
(define-nqueens nqueens/treelist
                (empty-treelist treelist-empty? treelist-first tl-rest tl-cons treelist-append))

;; pseq: likewise
(define (pseq-rest s)
  (let-values ([(x r) (pseq-pop-front s)])
    r))
(define (pseq-cons x s)
  (pseq-push-front s x))
(define-nqueens nqueens/pseq (empty-pseq pseq-empty? pseq-first pseq-rest pseq-cons pseq-append))

;; gvector: no persistence, so rest, cons and append all copy
(define (gv-empty)
  (make-gvector))
(define (gv-empty? g)
  (eqv? 0 (gvector-count g)))
(define (gv-first g)
  (gvector-ref g 0))
(define (gv-rest g)
  (define r (make-gvector #:capacity (max 1 (gvector-count g))))
  (for ([i (in-range 1 (gvector-count g))])
    (gvector-add! r (gvector-ref g i)))
  r)
(define (gv-cons x g)
  (define r (make-gvector #:capacity (add1 (gvector-count g))))
  (gvector-add! r x)
  (for ([v (in-gvector g)])
    (gvector-add! r v))
  r)
(define (gv-append a b)
  (define r (make-gvector #:capacity (max 1 (+ (gvector-count a) (gvector-count b)))))
  (for ([v (in-gvector a)])
    (gvector-add! r v))
  (for ([v (in-gvector b)])
    (gvector-add! r v))
  r)
;; the empty gvector cannot be shared, since each variant mutates its own
(define-syntax-rule (gv-e)
  (gv-empty))
(define (nqueens/gvector n)
  (define (one-to n)
    (let loop ([i n]
               [l (gv-empty)])
      (if (= i 0)
          l
          (loop (- i 1) (gv-cons i l)))))
  (define (ok? row dist placed)
    (let loop ([i 0]
               [dist dist])
      (cond
        [(>= i (gvector-count placed)) #t]
        [(let ([p (gvector-ref placed i)]) (or (= p (+ row dist)) (= p (- row dist)))) #f]
        [else (loop (add1 i) (add1 dist))])))
  (define (try-it x y z)
    (if (gv-empty? x)
        (if (gv-empty? y) 1 0)
        (+ (if (ok? (gv-first x) 1 z)
               (try-it (gv-append (gv-rest x) y) (gv-empty) (gv-cons (gv-first x) z))
               0)
           (try-it (gv-rest x) (gv-cons (gv-first x) y) z))))
  (try-it (one-to n) (gv-empty) (gv-empty)))

;; mutable-treelist: same, copying at every step
(define (mtl-empty)
  (make-mutable-treelist 0))
(define (mtl-rest t)
  (define r (mutable-treelist-copy t))
  (mutable-treelist-drop! r 1)
  r)
(define (mtl-cons x t)
  (define r (mutable-treelist-copy t))
  (mutable-treelist-cons! r x)
  r)
(define (mtl-append a b)
  (define r (mutable-treelist-copy a))
  (mutable-treelist-append! r b)
  r)
(define (nqueens/mutable-treelist n)
  (define (one-to n)
    (let loop ([i n]
               [l (mtl-empty)])
      (if (= i 0)
          l
          (loop (- i 1) (mtl-cons i l)))))
  (define (ok? row dist placed)
    (let loop ([i 0]
               [dist dist])
      (cond
        [(>= i (mutable-treelist-length placed)) #t]
        [(let ([p (mutable-treelist-ref placed i)]) (or (= p (+ row dist)) (= p (- row dist)))) #f]
        [else (loop (add1 i) (add1 dist))])))
  (define (try-it x y z)
    (if (eqv? 0 (mutable-treelist-length x))
        (if (eqv? 0 (mutable-treelist-length y)) 1 0)
        (+
         (if (ok? (mutable-treelist-first x) 1 z)
             (try-it (mtl-append (mtl-rest x) y) (mtl-empty) (mtl-cons (mutable-treelist-first x) z))
             0)
         (try-it (mtl-rest x) (mtl-cons (mutable-treelist-first x) y) z))))
  (try-it (one-to n) (mtl-empty) (mtl-empty)))

;; eseq: an ephemeral sequence, copied the same way.  eseq-copy shares its
;; chunks and separates them lazily, and eseq-append! takes a persistent
;; second argument without consuming it, so neither copy is a deep one.
(define (es-rest e)
  (define r (eseq-copy e))
  (eseq-pop-front! r)
  r)
(define (es-cons x e)
  (define r (eseq-copy e))
  (eseq-push-front! r x)
  r)
(define (es-append a b)
  (define r (eseq-copy a))
  (sek-for-each b (lambda (x) (eseq-push-back! r x)))
  r)
(define-nqueens nqueens/eseq ((make-eseq) eseq-empty? eseq-first es-rest es-cons es-append))

;; ------------------------------------------- the same problem, one stack deep

;; How one would actually write it with a mutable sequence: walk the search
;; tree, pushing a row when a queen is placed and popping it on the way back
;; out.  Only push, pop and reading the k-th element from the top are used,
;; and only one sequence is alive at a time.
(define-syntax-rule (define-nqueens/stack name (create push! pop! top-ref size))
  (define (name n)
    (define s (create))
    (define (safe? row)
      (let loop ([k 0] [dist 1])
        (cond
          [(>= k (size s)) #t]
          [(let ([p (top-ref s k)])
             (or (= p row) (= p (+ row dist)) (= p (- row dist))))
           #f]
          [else (loop (+ k 1) (+ dist 1))])))
    (let place ()
      (if (= (size s) n)
          1
          (let loop ([row 1] [acc 0])
            (cond
              [(> row n) acc]
              [(safe? row)
               (push! s row)
               (let ([sub (place)])
                 (pop! s)
                 (loop (+ row 1) (+ acc sub)))]
              [else (loop (+ row 1) acc)]))))))

(define-nqueens/stack nqueens/stack/gvector
  ((lambda () (make-gvector))
   gvector-add!
   gvector-remove-last!
   (lambda (g k) (gvector-ref g (- (gvector-count g) 1 k)))
   gvector-count))

(define-nqueens/stack nqueens/stack/mutable-treelist
  ((lambda () (make-mutable-treelist 0))
   mutable-treelist-add!
   (lambda (t) (mutable-treelist-drop-right! t 1))
   (lambda (t k) (mutable-treelist-ref t (- (mutable-treelist-length t) 1 k)))
   mutable-treelist-length))

(define-nqueens/stack nqueens/stack/eseq
  ((lambda () (make-eseq))
   eseq-push-back!
   eseq-pop-back!
   (lambda (e k) (eseq-ref e (- (eseq-length e) 1 k)))
   eseq-length))

;; A list is a stack already: the k-th from the top is the k-th pair.
;; A box holding a persistent sequence is a stack too, and is what you would
;; reach for if the search had to keep snapshots of the placed rows.
(define-nqueens/stack nqueens/stack/box-of-pseq
  ((lambda () (box empty-pseq))
   (lambda (b x) (set-box! b (pseq-push-back (unbox b) x)))
   (lambda (b) (set-box! b (let-values ([(x r) (pseq-pop-back (unbox b))]) r)))
   (lambda (b k) (let ([s (unbox b)]) (pseq-ref s (- (pseq-length s) 1 k))))
   (lambda (b) (pseq-length (unbox b)))))

(define-nqueens/stack nqueens/stack/box-of-treelist
  ((lambda () (box empty-treelist))
   (lambda (b x) (set-box! b (treelist-add (unbox b) x)))
   (lambda (b) (set-box! b (treelist-drop-right (unbox b) 1)))
   (lambda (b k) (let ([t (unbox b)]) (treelist-ref t (- (treelist-length t) 1 k))))
   (lambda (b) (treelist-length (unbox b)))))

(define-nqueens/stack nqueens/stack/box-of-list
  ((lambda () (box (cons '() 0)))
   (lambda (b x) (set-box! b (cons (cons x (car (unbox b))) (add1 (cdr (unbox b))))))
   (lambda (b) (set-box! b (cons (cdr (car (unbox b))) (sub1 (cdr (unbox b))))))
   (lambda (b k) (list-ref (car (unbox b)) k))
   (lambda (b) (cdr (unbox b)))))

(struct vstack (vec [n #:mutable]))
(define-nqueens/stack nqueens/stack/vector
  ((lambda () (vstack (make-vector 64 0) 0))
   (lambda (s x)
     (vector-set! (vstack-vec s) (vstack-n s) x)
     (set-vstack-n! s (add1 (vstack-n s))))
   (lambda (s) (set-vstack-n! s (sub1 (vstack-n s))))
   (lambda (s k) (vector-ref (vstack-vec s) (- (vstack-n s) 1 k)))
   vstack-n))

(define (bench name thunk reps expected)
  (collect-garbage)
  (collect-garbage)
  (define start (current-inexact-monotonic-milliseconds))
  (define v
    (let loop ([k reps]
               [v 0])
      (if (zero? k)
          v
          (loop (- k 1) (thunk)))))
  (define ms (- (current-inexact-monotonic-milliseconds) start))
  (unless (= v expected)
    (error 'nqueens "~a produced ~a, expected ~a" name v expected))
  (printf "~a~a ms\n"
          (let ([s (format "~a" name)])
            (string-append s (make-string (max 1 (- 26 (string-length s))) #\space)))
          (let ([t (number->string (inexact->exact (round ms)))])
            (string-append (make-string (max 1 (- 6 (string-length t))) #\space) t)))
  ms)

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (define json-file
    (let loop ([as args])
      (cond [(null? as) #f]
            [(and (equal? (car as) "--json") (pair? (cdr as))) (cadr as)]
            [else (loop (cdr as))])))
  (define reps
    (or (for/or ([a (in-list args)]
                 #:unless (regexp-match? #rx"^--" a))
          (string->number a))
        10000))
  (printf "nqueens 8, ~a repetitions -- Racket ~a\n" reps (version))

  ;; One column, which the report draws as a plain bar chart.  The title must
  ;; not itself contain a comma: the recorder splits off the units at the last.
  (define (group title rows)
    (printf "\n~a\n" title)
    (record-table!
     (format "~a, ms for ~a repetitions" title reps)
     (list "ms")
     (for/list ([r (in-list rows)])
       (cons (car r) (list (bench (car r) (cdr r) reps 92))))))

  (parameterize ([current-scenario 'nqueens])
    (group "the classic program with the pair operations swapped out"
           (list (cons 'pairs (lambda () (nqueens/pairs 8)))
                 (cons 'treelist (lambda () (nqueens/treelist 8)))
                 (cons 'pseq (lambda () (nqueens/pseq 8)))
                 (cons 'eseq (lambda () (nqueens/eseq 8)))
                 (cons 'gvector (lambda () (nqueens/gvector 8)))
                 (cons 'mutable-treelist (lambda () (nqueens/mutable-treelist 8)))
                 (cons 'vector (lambda () (nqueens/vector 8)))))
    (group "the same problem as a backtracking search over one mutable stack"
           (list (cons 'vector (lambda () (nqueens/stack/vector 8)))
                 (cons 'gvector (lambda () (nqueens/stack/gvector 8)))
                 (cons 'mutable-treelist (lambda () (nqueens/stack/mutable-treelist 8)))
                 (cons 'eseq (lambda () (nqueens/stack/eseq 8)))
                 (cons 'box-of-list (lambda () (nqueens/stack/box-of-list 8)))
                 (cons 'box-of-pseq (lambda () (nqueens/stack/box-of-pseq 8)))
                 (cons 'box-of-treelist (lambda () (nqueens/stack/box-of-treelist 8))))))
  (when json-file (dump-json! json-file)))
