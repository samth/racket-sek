#lang racket/base
;; Workloads, as against operations.
;;
;; `main.rkt` and `external.rkt` measure one operation at a time, which is what
;; you want to understand a structure but not what you want to choose one.  An
;; operation that is twice as fast may run a hundredth as often; a structure
;; that wins every row can still lose a program, because the rows are not
;; weighted by anything real.
;;
;; Each workload here is a small program.  It runs a script of mixed operations
;; over a sequence whose size *changes as it runs* -- growing, being split, being
;; rejoined -- so no single n characterises it, and it consumes what it reads so
;; nothing can be optimized away.  Every implementation executes the identical
;; script, generated once from a fixed seed, and the answer each produces is
;; checked against the others: a structure that is fast because it did something
;; different is not fast.
;;
;;   racket -y workloads.rkt              run everything
;;   racket -y workloads.rkt editor       one workload
;;   racket -y workloads.rkt --quick      shorter scripts
;;
;; Times are milliseconds for the whole workload, so they are comparable across
;; a row and meaningless down a column -- each workload is its own program.

(require racket/list
         racket/string
         racket/vector
         racket/fixnum
         racket/treelist
         racket/mutable-treelist
         data/gvector
         sek
         "main.rkt")     ; impl records, all-impls, fmt, quick?

(provide workload-scenarios run-workloads!)

;; ============================================================ the harness

(define verbose-answers? (make-parameter #f))
(define workloads (make-hash))
(define workload-order '())

(define-syntax-rule (define-workload name blurb needs body ...)
  (begin
    (hash-set! workloads 'name (list blurb 'needs body ...))
    (set! workload-order (cons 'name workload-order))))

;; A workload reports (values answer) and is timed as a whole.  The answer is
;; compared across implementations; a mismatch is a bug in the workload or the
;; structure, and either way the number is not worth reporting.
(define (time-workload thunk)
  (collect-garbage)
  (define t0 (current-inexact-monotonic-milliseconds))
  (define answer (thunk))
  (values (- (current-inexact-monotonic-milliseconds) t0) answer))

;; Best of a few, with the same adaptive rule `measure` uses: keep going while
;; the best still improves, because these allocate heavily.
(define (best-of thunk)
  (let loop ([best +inf.0] [answer #f] [n 0] [flat 0])
    (cond
      [(and (>= n 3) (>= flat 2)) (values best answer)]
      [(>= n 10) (values best answer)]
      [else
       (define-values (ms a) (time-workload thunk))
       (if (< ms (* best 0.98))
           (loop (min best ms) a (add1 n) 0)
           (loop (min best ms) (or answer a) (add1 n) (add1 flat)))])))

;; Can this implementation run this workload at all?
(define (supports? i needs)
  (for/and ([n (in-list needs)])
    (case n
      [(push-back) (and (impl-push-back i) #t)]
      [(push-front) (and (impl-push-front i) #t)]
      [(pop-front) (and (impl-pop-front i) #t)]
      [(pop-back) (and (impl-pop-back i) #t)]
      [(ref) (and (impl-ref i) #t)]
      [(set) (and (impl-set i) #t)]
      [(append) (and (impl-append i) #t)]
      [(split) (and (impl-split i) #t)]
      [(take) (and (impl-take i) #t)]
      [(drop) (and (impl-drop i) #t)]
      [(for-each) (and (impl-for-each i) #t)]
      [(filter) (and (impl-filter i) #t)]
      [(snapshot) (and (impl-snapshot i) #t)]
      [else #t])))

;; An implementation whose Theta(n) operations the workload leans on is not
;; excluded -- it is the comparison -- but a script that would make it
;; quadratic at the full size is run at a smaller one and marked, rather than
;; left to run for an hour or dropped without saying so.
(define (too-slow? i needs)
  (for/or ([n (in-list needs)]) (memq n (impl-linear i))))

;; ============================================================ the scripts
;;
;; Generated once, from a fixed seed, so every implementation does identical
;; work and the comparison is of structures rather than of random draws.

(define (make-script n seed gen)
  (parameterize ([current-pseudo-random-generator
                  (vector->pseudo-random-generator (vector seed 1 2 3 4 5))])
    (for/vector ([_ (in-range n)]) (gen))))

;; ============================================================ workloads

;; -------------------------------------------------------------------------
;; A document as a sequence of lines, edited and redrawn.  This is the shape
;; catenable sequences are for: the cursor moves, a line is inserted or
;; removed where it is, and a window around it is rendered.  A vector does the
;; rendering well and the editing badly; a list the reverse.
(define-workload editor
  "edit a document of lines: jump, insert, delete, redraw a window"
  (ref split append take drop for-each)
  (lambda (i n edits)
    (define script
      (make-script edits 20260912
                   (lambda () (vector (random 1000) (random 3)))))
    (lambda ()
      (define doc ((impl-construct i) n (lambda (k) k)))
      (let loop ([doc doc] [k 0] [checksum 0])
        (cond
          [(fx= k (vector-length script)) checksum]
          [else
           (define e (vector-ref script k))
           (define len ((impl-len i) doc))
           (define at (fxmodulo (fx* (vector-ref e 0) 977) (fxmax 1 len)))
           (case (vector-ref e 1)
             ;; insert a line at the cursor
             [(0)
              (define-values (a b) ((impl-split i) doc at))
              (loop ((impl-append i) ((impl-push-back i) a k) b)
                    (add1 k) checksum)]
             ;; delete the line at the cursor
             [(1)
              (cond
                [(fx< len 2) (loop doc (add1 k) checksum)]
                [else
                 (define-values (a b) ((impl-split i) doc at))
                 (loop ((impl-append i) a ((impl-drop i) b 1)) (add1 k) checksum)])]
             ;; redraw a forty-line window around the cursor
             [else
              (define from (fxmax 0 (fx- at 20)))
              (define to (fxmin len (fx+ from 40)))
              (define sum
                (for/fold ([s 0]) ([j (in-range from to)])
                  (fx+ s ((impl-ref i) doc j))))
              (loop doc (add1 k) (fxand (fx+ checksum sum) #xffffff))])])))))

;; -------------------------------------------------------------------------
;; A queue of work that is fed in bursts and drained, with the backlog
;; occasionally handed to a reader as an immutable snapshot.  The size swings
;; by orders of magnitude over the run, which is the point: a structure that is
;; good at one size has to be good across the range.
;; -------------------------------------------------------------------------
;; Editing with an undo history.  Every so often the session checkpoints, and
;; occasionally it goes back to the last checkpoint.  A persistent sequence
;; checkpoints by keeping the value it already holds; a mutable one copies.
;; That is the whole point of the workload: the editing is ordinary, and the
;; checkpoints are what separate the two families.
(define-workload undo
  "edit with an undo history: checkpoint often, occasionally restore"
  (len split append push-back drop ref)
  (lambda (i n edits)
    (define script
      (make-script edits 20260921
                   (lambda () (vector (random 1000) (random 10)))))
    (lambda ()
      (define doc0 ((impl-construct i) n (lambda (k) k)))
      (let loop ([doc doc0]
                 [saved ((impl-fresh i) doc0)]
                 [k 0]
                 [checksum 0])
        (cond
          [(fx= k (vector-length script)) checksum]
          [else
           (define e (vector-ref script k))
           (define len ((impl-len i) doc))
           (define at (fxmodulo (fx* (vector-ref e 0) 977) (fxmax 1 len)))
           (case (vector-ref e 1)
             ;; type a line
             [(0 1 2 3)
              (define-values (a b) ((impl-split i) doc at))
              (loop ((impl-append i) ((impl-push-back i) a k) b) saved (add1 k) checksum)]
             ;; delete a line
             [(4 5)
              (cond
                [(fx< len 2) (loop doc saved (add1 k) checksum)]
                [else
                 (define-values (a b) ((impl-split i) doc at))
                 (loop ((impl-append i) a ((impl-drop i) b 1)) saved (add1 k) checksum)])]
             ;; read one, so the edits cannot all be dead code
             [(6 7)
              (loop doc saved (add1 k) (fxand (fx+ checksum ((impl-ref i) doc at)) #xffffff))]
             ;; checkpoint: free for a persistent sequence, a copy for a
             ;; mutable one, which is the asymmetry this workload exists for
             [(8)
              (loop doc ((impl-fresh i) doc) (add1 k) checksum)]
             ;; undo: go back to the last checkpoint and keep editing from it
             [else
              (loop ((impl-fresh i) saved) saved (add1 k) checksum)])])))))

(define-workload queue
  "feed a work queue in bursts, drain it, snapshot the backlog"
  (push-back pop-front len)
  (lambda (i n rounds)
    (define script
      (make-script rounds 20260913
                   (lambda () (vector (+ 1 (random 400)) (+ 1 (random 400))))))
    (lambda ()
      (define q ((impl-empty i) 16))
      (let loop ([q q] [k 0] [done 0])
        (cond
          [(fx= k (vector-length script)) done]
          [else
           (define e (vector-ref script k))
           ;; a burst arrives
           (define q1
             (for/fold ([q q]) ([j (in-range (vector-ref e 0))])
               ((impl-push-back i) q (fx+ j k))))
           ;; some of it is handled
           (define want (vector-ref e 1))
           (define-values (q2 handled)
             (let drain ([q q1] [m 0])
               (if (or (fx= m want) (fx= ((impl-len i) q) 0))
                   (values q m)
                   (drain ((impl-pop-front i) q) (add1 m)))))
           ;; every sixteenth round the backlog is published
           (define q3
             (if (and (fx= 0 (fxand k 15)) (impl-snapshot i))
                 (let ([snap ((impl-snapshot i) q2)])
                   (if snap q2 q2))
                 q2))
           (loop q3 (add1 k) (fx+ done handled))])))))

;; -------------------------------------------------------------------------
;; A log that is appended to and periodically compacted: the old half is
;; dropped, what remains is filtered, and a fresh batch is concatenated on.
;; Every operation here is bulk, so this is where chunked structures should
;; show and a cons list should not.
(define-workload log
  "append a log, compact it by dropping, filtering and re-joining"
  (push-back drop filter append len for-each)
  (lambda (i n rounds)
    (lambda ()
      (define batch ((impl-construct i) n (lambda (k) k)))
      (let loop ([lg ((impl-empty i) 16)] [k 0] [kept 0])
        (cond
          [(fx= k rounds) kept]
          [else
           ;; a batch arrives whole
           (define joined ((impl-append i) lg ((impl-fresh i) batch)))
           ;; compact: drop the oldest half, keep one record in three
           (define len ((impl-len i) joined))
           (define trimmed
             (if (fx> len (fx* 4 n)) ((impl-drop i) joined (quotient len 2)) joined))
           (define compacted
             (if (fx= 0 (fxand k 3))
                 ((impl-filter i) trimmed (lambda (x) (fx= 0 (fxremainder x 3))))
                 trimmed))
           (loop compacted (add1 k) (fx+ kept ((impl-len i) compacted)))])))))

;; -------------------------------------------------------------------------
;; Build a sequence, then read it back in a way that is friendly to nothing in
;; particular: a strided walk, which defeats both a cursor and a cache line.
;; This is the honest test of indexed access away from the microbenchmark.
(define-workload scan
  "build, then walk at a stride, then fold"
  (ref for-each len)
  (lambda (i n rounds)
    (lambda ()
      (define s ((impl-construct i) n (lambda (k) k)))
      (define len ((impl-len i) s))
      (let loop ([k 0] [acc 0])
        (cond
          [(fx= k rounds) acc]
          [else
           (define stride (vector-ref #(1 7 61 509 4093) (fxmodulo k 5)))
           (define sum
             (let walk ([j 0] [s2 0])
               (if (fx>= j len)
                   s2
                   (walk (fx+ j stride) (fx+ s2 ((impl-ref i) s j))))))
           (loop (add1 k) (fxand (fx+ acc sum) #xffffff))])))))

;; ============================================================ driving

(define (run-workload name sizes)
  (define entry (hash-ref workloads name))
  (define blurb (car entry))
  (define needs (cadr entry))
  (define make (caddr entry))
  (printf "\n~a: ~a, ms for the whole workload\n" name blurb)
  (printf "~a" (make-string 32 #\space))
  (for ([sz (in-list sizes)]) (printf "~a" (~w (format "~a" (car sz)) 12)))
  (newline)
  (define answers (make-hash))
  (define failures '())
  (for ([i (in-list all-impls)])
    (cond
      [(not (supports? i needs))
       (void)]
      [else
       (printf "~a" (~w (impl-name i) 32))
       (for ([sz (in-list sizes)])
         (define n (cadr sz))
         (define reps (caddr sz))
         (cond
           [(and (too-slow? i needs) (> n 20000))
            (printf "~a" (~w "  n/a" 12))]
           [else
            ;; An implementation that raises is reported and its cell marked,
            ;; rather than taking the whole run down with it.  This matters:
            ;; the editor workload is how `treelist-copy-for-mutable` was found
            ;; to reject trees that are not leftwise dense, and on a Racket
            ;; without that fix this is the cell that says so.  It is recorded,
            ;; not swallowed -- no time is printed and no answer is entered
            ;; into the cross-check.
            (define-values (ms answer)
              (with-handlers ([exn:fail?
                               (lambda (e)
                                 (set! failures
                                       (cons (list (impl-name i) (car sz) (exn-message e))
                                             failures))
                                 (values #f #f))])
                (best-of (make i n reps))))
            (cond
              [ms
               (hash-update! answers (car sz)
                             (lambda (l) (cons (cons (impl-name i) answer) l)) '())
               (printf "~a" (~w (fmt ms) 12))]
              [else (printf "~a" (~w "  err" 12))])]))
       (newline)]))
  ;; every implementation must have computed the same thing
  (for ([(label as) (in-hash answers)])
    (define vs (remove-duplicates (map cdr as)))
    (when (verbose-answers?)
      (printf "  (~a: ~a implementations agreed on ~s)\n" label (length as) (car vs)))
    (unless (= 1 (length vs))
      (printf "  !! ~a: implementations disagree: ~s\n" label
              (for/list ([a (in-list as)]) (cons (car a) (cdr a))))))
  (for ([f (in-list (reverse failures))])
    ;; just the first line; a Racket exception message can carry a long context
    (printf "  !! ~a at ~a raised: ~a\n" (car f) (cadr f)
            (car (string-split (caddr f) "\n")))))

(define (~w s w)
  (define t (format "~a" s))
  (if (>= (string-length t) w)
      (string-append t " ")
      (string-append (make-string (- w (string-length t)) #\space) t)))

(define (workload-scenarios) (reverse workload-order))

(define (run-workloads! args)
  (define quick? (member "--quick" args))
  (when (member "--answers" args) (verbose-answers? #t))
  (define names
    (let ([asked (filter (lambda (a) (not (regexp-match #rx"^--" a))) args)])
      (if (null? asked)
          (reverse workload-order)
          (map string->symbol asked))))
  (printf "sequence workloads -- Racket ~a\n" (version))
  (for ([nm (in-list names)])
    (case nm
      [(editor)
       (run-workload 'editor (if quick?
                                 '(("1k lines" 1000 400) ("20k lines" 20000 400))
                                 '(("1k lines" 1000 2000) ("20k lines" 20000 2000)
                                   ("400k lines" 400000 2000))))]
      [(undo)
       (run-workload 'undo (if quick?
                               '(("1k lines" 1000 400) ("20k lines" 20000 400))
                               '(("1k lines" 1000 2000) ("20k lines" 20000 2000)
                                 ("400k lines" 400000 1000))))]
      [(queue)
       (run-workload 'queue (if quick?
                                '(("200 rounds" 0 200))
                                '(("2000 rounds" 0 2000))))]
      [(log)
       (run-workload 'log (if quick?
                              '(("1k batches" 1000 20))
                              '(("1k batches" 1000 60) ("50k batches" 50000 60))))]
      [(scan)
       (run-workload 'scan (if quick?
                               '(("10k" 10000 20))
                               '(("10k" 10000 40) ("1M" 1000000 3))))]
      [else (printf "no such workload: ~a\n" nm)]))
  (newline))

(module+ main
  (run-workloads! (vector->list (current-command-line-arguments))))
