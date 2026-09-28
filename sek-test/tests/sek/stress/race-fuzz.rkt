#lang racket/base
;; Race threads on shared sequences, then check what they leave behind.
;;
;;   racket race-fuzz.rkt MODE [--threads N] [--trials T] [--seed S]
;;                             [--max-size M] [--leaf K0] [--node K1] [--threshold T]
;;
;; Prints one line per kind of event, with its count.  A run in which every
;; line but `trials` is absent found nothing.
;;
;; Supported uses -- these must find nothing:
;;   pseq      N threads push, pop and set on the same persistent sequences,
;;             starting together, and keep using their results; each result
;;             must hold what its own operations put there, and sequences
;;             derived before the race must not change.
;;   readers   N threads read one ephemeral sequence at once by every route --
;;             in-sek, folds, iterators that jump, segments, `for`, snapshots,
;;             copies, eseq-ref -- and then one thread updates it.
;;   handoff   one thread updates an ephemeral sequence and hands snapshots of
;;             it, over a channel, to N - 1 threads, which check them, extend
;;             them persistently and edit them into ephemeral sequences of
;;             their own, while the first thread goes on updating.
;;
;; Unsupported uses -- these may return wrong answers or raise exn:fail:sek,
;; but must never corrupt memory:
;;   writer    one thread updates an ephemeral sequence while N - 1 read it
;;   writers   N threads update one ephemeral sequence
;;   iterator  N threads move and read through one iterator
;;
;; Run these against a build of sek-lib compiled with SEK_CHECKED=1 (see
;; sek/unchecked) to have every unchecked operation check its precondition: an
;; event mentioning "precondition violated" is then a memory-safety failure
;; caught before it happened.  A build compiled with SEK_JITTER=1 widens the
;; windows in which races happen.  Elements are fixnums: 0..999999 in the
;; initial sequence and tagged by thread above that, so anything else that
;; comes out -- the empty-slot marker, a chunk -- is reported as foreign.
(require racket/list
         racket/async-channel
         sek)

(define mode 'pseq)
(define nthreads 2)
(define trials 40)
(define seed 1)
(define max-size 30000)

;; Racing threads note events too, so the tally has a lock.
(define tally (make-hash))
(define tally-lock (make-semaphore 1))
(define (note! k)
  (call-with-semaphore tally-lock (lambda () (hash-update! tally k add1 0))))

(define (first-line e)
  (define m (if (exn? e) (exn-message e) (format "non-exception raised: ~e" e)))
  (define l (car (regexp-split #rx"\n" m)))
  (regexp-replace* #rx"[0-9]+" (if (> (string-length l) 100) (substring l 0 100) l) "N"))

(define (note-exn! where e)
  (define line (first-line e))
  ;; the tally keeps one line; a memory-safety event deserves the stack too
  (when (and (exn? e) (regexp-match? #rx"precondition|invalid memory" line))
    (eprintf "~a raised: ~a\n~a\n" where (exn-message e)
             (for/list ([f (in-list (continuation-mark-set->context (exn-continuation-marks e)))]
                        [_ (in-range 16)])
               (format "  ~a ~a" (or (car f) "?")
                       (let ([s (cdr f)])
                         (if s (format "~a:~a" (srcloc-source s) (srcloc-line s)) ""))))))
  (note! (string->symbol (format "~a raised: ~a" where line))))

;; Run a check that returns #f when all is well, or a symbol naming the fault.
(define (check! where thunk)
  (with-handlers ([(lambda (e) #t) (lambda (e) (note-exn! where e) 'raised)])
    (define r (thunk))
    (when r (note! r))
    r))

(define (foreign? lst) (for/or ([x (in-list lst)]) (not (fixnum? x))))

(define (random-size)
  (case (random 4)
    [(0) (random (min 40 max-size))]
    [(1) (random (min 300 max-size))]
    [(2) (random (min 3000 max-size))]
    [else (random max-size)]))

;; Run the thunks in parallel threads that start together, and return their
;; results in order; a thread that raises gives #f, and its exception is noted.
;; The thunks share nothing but the structures under test: every script is
;; made beforehand, since `random` is not safe to call from parallel threads.
(define (race where thunks)
  (define n (length thunks))
  (define arrived (make-vector n #f))
  (define results (make-vector n #f))
  (define errors (make-vector n #f))
  (define (runner i thunk)
    (lambda ()
      (vector-set! arrived i #t)
      (let spin () (unless (for/and ([a (in-vector arrived)]) a) (spin)))
      (with-handlers ([(lambda (e) #t) (lambda (e) (vector-set! errors i e))])
        (vector-set! results i (thunk)))))
  (define ts (for/list ([thunk (in-list thunks)] [i (in-naturals)])
               (thread (runner i thunk) #:pool 'own)))
  ;; A thread still running after `hang-seconds` is interrupted with a break,
  ;; which it raises where it is, so the runner's handler records where that
  ;; was; one hang then costs a trial rather than the run.
  (define deadline (+ (current-inexact-milliseconds) (* 1000 hang-seconds)))
  (for ([t (in-list ts)])
    (unless (sync/timeout (max 0 (/ (- deadline (current-inexact-milliseconds)) 1000))
                          (thread-dead-evt t))
      (break-thread t)
      (unless (sync/timeout 10 (thread-dead-evt t))
        (note! (string->symbol (format "~a hung, and ignored a break" where)))
        (kill-thread t))))
  (for ([e (in-vector errors)] #:when e)
    (if (exn:break? e)
        (note! (string->symbol
                (format "~a hung in: ~a" where
                        (for/list ([f (in-list (continuation-mark-set->context
                                                (exn-continuation-marks e)))]
                                   [_ (in-range 8)])
                          (format "~a" (or (car f) "?"))))))
        (note-exn! where e)))
  (vector->list results))

(define hang-seconds 60)

;; ------------------------------------------------------------ scripts

;; A script of persistent operations, made against a model length so that no
;; pop is of an empty sequence.  `tag` makes the pushed values distinct.
(define (make-script n0 len tag)
  (let loop ([i 0] [n n0] [acc '()])
    (if (= i len)
        (reverse acc)
        (case (if (> n 0) (random 7) (random 2))
          [(0 2) (loop (add1 i) (add1 n) (cons (list 'front (+ tag i)) acc))]
          [(1 3) (loop (add1 i) (add1 n) (cons (list 'back (+ tag i)) acc))]
          [(4) (loop (add1 i) n (cons (list 'set (random n) (+ tag i)) acc))]
          [else (loop (add1 i) (sub1 n)
                      (cons (list (if (zero? (random 2)) 'pop-front 'pop-back)) acc))]))))

(define (run-pseq-script s script)
  (for/fold ([s s]) ([op (in-list script)])
    (case (car op)
      [(front) (pseq-push-front s (cadr op))]
      [(back) (pseq-push-back s (cadr op))]
      [(set) (pseq-set s (cadr op) (caddr op))]
      [(pop-front) (let-values ([(x r) (pseq-pop-front s)]) r)]
      [(pop-back) (let-values ([(x r) (pseq-pop-back s)]) r)])))

(define (run-eseq-script! e script)
  (for ([op (in-list script)])
    (case (car op)
      [(front) (eseq-push-front! e (cadr op))]
      [(back) (eseq-push-back! e (cadr op))]
      [(set) (eseq-set! e (cadr op) (caddr op))]
      [(pop-front) (eseq-pop-front! e)]
      [(pop-back) (eseq-pop-back! e)])))

(define (run-list-script l script)
  (for/fold ([l l]) ([op (in-list script)])
    (case (car op)
      [(front) (cons (cadr op) l)]
      [(back) (append l (list (cadr op)))]
      [(set) (list-set l (cadr op) (caddr op))]
      [(pop-front) (cdr l)]
      [(pop-back) (drop-right l 1)])))

(define (build-pseq-by-pushes n)
  (for/fold ([s empty-pseq]) ([i (in-range n)])
    (if (zero? (random 3)) (pseq-push-front s i) (pseq-push-back s i))))

(define (build-eseq n)
  (define e (make-eseq))
  (for ([i (in-range n)])
    (if (zero? (random 3)) (eseq-push-front! e i) (eseq-push-back! e i)))
  e)

;; What a persistent result must be; checks and returns its list.
(define (examine-pseq where s expected)
  (define lst #f)
  (check! where (lambda () (sek-validate-pseq s) #f))
  (check! where (lambda () (set! lst (pseq->list s)) #f))
  (when lst
    (check! where (lambda () (and (foreign? lst) 'foreign-value)))
    (check! where (lambda () (and expected (not (equal? lst expected)) 'wrong-contents)))
    (check! where (lambda () (and (not (= (pseq-length s) (length lst))) 'length-disagrees)))
    (define v (list->vector lst))
    (check! where
            (lambda ()
              (and (for/or ([_ (in-range 30)])
                     (and (> (vector-length v) 0)
                          (let ([i (random (vector-length v))])
                            (not (eqv? (pseq-ref s i) (vector-ref v i))))))
                   'ref-disagrees))))
  lst)

;; ------------------------------------------------------------ modes

(define (pseq-trial)
  (define n (random-size))
  (define base (build-pseq-by-pushes n))
  (define base-list (pseq->list base))
  (define bystanders
    (for/list ([j (in-range (random 3))])
      (define b (run-pseq-script base (make-script n (random 30) (* 90000000 (add1 j)))))
      (cons b (pseq->list b))))
  (define scripts (for/list ([t (in-range nthreads)])
                    (make-script n (add1 (random 300)) (* 1000000 (add1 t)))))
  (define results (race "race" (for/list ([sc (in-list scripts)])
                                 (lambda () (run-pseq-script base sc)))))
  (define (bystanders-intact? where)
    (check! where (lambda ()
                    (and (not (and (equal? (pseq->list base) base-list)
                                   (for/and ([b (in-list bystanders)])
                                     (equal? (pseq->list (car b)) (cdr b)))))
                         'bystander-changed))))
  (bystanders-intact? "after race")
  (define lists (for/list ([r (in-list results)] [sc (in-list scripts)])
                  (and r (examine-pseq "result" r (run-list-script base-list sc)))))
  ;; keep using each result from one thread, and edit it into an ephemeral
  ;; sequence as well
  (for ([r (in-list results)] [l (in-list lists)] #:when (and r l))
    (define script (make-script (length l) 200 50000000))
    (check! "continuing" (lambda ()
                           (examine-pseq "continuing" (run-pseq-script r script)
                                         (run-list-script l script))
                           #f))
    (check! "editing" (lambda ()
                        (define e (pseq-edit r))
                        (run-eseq-script! e script)
                        (sek-validate-eseq e)
                        (and (not (equal? (eseq->list e) (run-list-script l script)))
                             'wrong-after-edit)))
    (check! "continuing" (lambda () (and (not (equal? (pseq->list r) l)) 'value-changed-after-race))))
  (bystanders-intact? "after continuing"))

;; Every way of reading an ephemeral sequence, each returning its elements as a
;; list (or, for eseq-ref, a list of samples to compare).
(define (reads e idx)
  (list
   (lambda () (for/list ([x (in-sek e)]) x))
   (lambda () (reverse (sek-fold-left e (lambda (acc x) (cons x acc)) '())))
   (lambda () (sek-fold-right e cons '()))
   (lambda () (for/list ([x e]) x))
   (lambda () (pseq->list (eseq-snapshot e)))
   (lambda () (eseq->list (eseq-copy e)))
   (lambda () (eseq->list (eseq-copy e #:mode 'share)))
   (lambda () (let ([acc '()])
                (sek-segments-for-each e (lambda (sg) (set! acc (append (reverse (segment->list sg)) acc))))
                (reverse acc)))
   (lambda () (let ([it (sek-iterator e 'backward)])
                (let loop ([acc '()])
                  (if (sek-iter-finished? it) acc
                      (loop (cons (sek-iter-get-and-move! it 'backward) acc))))))
   (lambda () (let ([it (sek-iterator e)] [n (eseq-length e)])
                ;; jump around, then report the samples in index order
                (sort (for/list ([i (in-list idx)] #:when (< i n))
                        (sek-iter-reach! it i)
                        (cons i (sek-iter-get it)))
                      < #:key car)))
   (lambda () (let ([n (eseq-length e)])
                (sort (for/list ([i (in-list idx)] #:when (< i n)) (cons i (eseq-ref e i)))
                      < #:key car)))))

(define (expected-reads l idx)
  (define v (list->vector l))
  (define samples (sort (for/list ([i (in-list idx)] #:when (< i (vector-length v)))
                          (cons i (vector-ref v i)))
                        < #:key car))
  (append (make-list 9 l) (list samples samples)))

(define (readers-trial)
  (define n (random-size))
  (define e (build-eseq n))
  (run-eseq-script! e (make-script n (random 400) 3000000)) ; pops too, so inner chunks vary
  (define before (eseq->list e))
  (define idx (remove-duplicates (for/list ([_ (in-range 40)]) (random (max 1 (length before))))))
  (define want (expected-reads before idx))
  (define results
    (race "race" (for/list ([t (in-range nthreads)])
                   (lambda () (for/list ([r (in-list (reads e idx))])
                                (with-handlers ([(lambda (x) #t) (lambda (x) x)]) (r)))))))
  (for ([r (in-list results)] #:when r)
    (for ([got (in-list r)] [w (in-list want)])
      (cond
        [(exn? got) (note-exn! "reader" got)]
        [(not (equal? got w)) (note! 'reader-saw-wrong-contents)])))
  (check! "after" (lambda () (sek-validate-eseq e) #f))
  (check! "after" (lambda () (and (not (equal? (eseq->list e) before)) 'sequence-changed)))
  ;; then update it from one thread: the first update adopts whatever fold the
  ;; readers' snapshots left
  (define snap (eseq-snapshot e))
  (define script (make-script (length before) 300 4000000))
  (check! "updating after"
          (lambda ()
            (run-eseq-script! e script)
            (sek-validate-eseq e)
            (and (not (equal? (eseq->list e) (run-list-script before script)))
                 'wrong-after-updates)))
  (check! "updating after" (lambda () (and (not (equal? (pseq->list snap) before)) 'snapshot-changed))))

(define (handoff-trial)
  (define n (random-size))
  (define e (build-eseq n))
  (define rounds (add1 (random 20)))
  (define start-list (eseq->list e))
  ;; the owner's scripts, made against the model lengths it will have
  (define owner-scripts
    (let loop ([r 0] [len (length start-list)] [acc '()])
      (if (= r rounds)
          (reverse acc)
          (let ([sc (make-script len (add1 (random 100)) (* 1000000 (add1 r)))])
            (loop (add1 r) (length (run-list-script (make-list len 0) sc)) (cons sc acc))))))
  ;; each receiver's persistent scripts are made when it knows the length of
  ;; the snapshot it received, so it seeds its own generator: `random` with the
  ;; shared one is not safe from parallel threads
  (define receiver-seeds (for/list ([t (in-range (sub1 nthreads))]) (random 1000000)))
  ;; asynchronous, so that a receiver that fails cannot block the owner; an
  ;; owner that fails sends 'failed, so that no receiver waits forever
  (define chans (for/list ([t (in-range (sub1 nthreads))]) (make-async-channel)))
  (define (owner)
    (with-handlers ([(lambda (x) #t)
                     (lambda (x)
                       (for ([ch (in-list chans)]) (async-channel-put ch 'failed))
                       (raise x))])
      (for/fold ([l start-list]) ([sc (in-list owner-scripts)])
        (run-eseq-script! e sc)
        (define l* (run-list-script l sc))
        (define s (eseq-snapshot e))
        (for ([ch (in-list chans)]) (async-channel-put ch (cons s l*)))
        l*)))
  (define (receiver t)
    (define ch (list-ref chans t))
    (current-pseudo-random-generator (make-pseudo-random-generator))
    (random-seed (list-ref receiver-seeds t))
    (let/ec stop
     (for ([_ (in-range rounds)])
      (define got (async-channel-get ch))
      (when (eq? got 'failed) (stop (void)))
      (define s (car got))
      (define l (cdr got))
      ;; the snapshot, while the owner goes on updating
      (examine-pseq "receiver" s l)
      ;; extend it persistently, twice from the same point, and edit it
      (define script (make-script (length l) 50 (+ 60000000 (* 100000 t))))
      (examine-pseq "receiver" (run-pseq-script s script) (run-list-script l script))
      (examine-pseq "receiver" (run-pseq-script s script) (run-list-script l script))
      (define mine (pseq-edit s))
      (run-eseq-script! mine script)
      (check! "receiver" (lambda () (and (not (equal? (eseq->list mine) (run-list-script l script)))
                                         'wrong-after-edit)))
      (check! "receiver" (lambda () (and (not (equal? (pseq->list s) l)) 'snapshot-changed))))))
  (define results (race "race" (cons owner (for/list ([t (in-range (sub1 nthreads))])
                                             (lambda () (receiver t))))))
  (define final (car results))
  (when final
    (check! "after" (lambda () (sek-validate-eseq e) #f))
    (check! "after" (lambda () (and (not (equal? (eseq->list e) final)) 'wrong-contents)))))

;; Unsupported: one writer and N - 1 readers.  Only memory safety is at stake.
(define (writer-trial)
  (define n (random-size))
  (define e (build-eseq n))
  (define writes (make-script n (add1 (random 2000)) 5000000))
  (define idx (for/list ([_ (in-range 100)]) (random 1000000)))
  (define (reader)
    (for ([_ (in-range 5)])
      (for ([r (in-list (reads e idx))])
        (with-handlers ([(lambda (x) #t) (lambda (x) (note-exn! "reader" x))])
          (define got (r))
          (when (and (list? got) (foreign? (filter (lambda (x) (not (pair? x))) got)))
            (note! 'reader-saw-foreign-value))))))
  (race "race" (cons (lambda () (run-eseq-script! e writes))
                     (for/list ([t (in-range (sub1 nthreads))]) reader)))
  (check! "after" (lambda () (sek-validate-eseq e) #f)))

;; Unsupported: N writers.  Only memory safety is at stake.
(define (writers-trial)
  (define n (random-size))
  (define e (build-eseq n))
  (define scripts (for/list ([t (in-range nthreads)])
                    (make-script (max n 1000) (add1 (random 2000)) (* 1000000 (add1 t)))))
  (race "race" (for/list ([sc (in-list scripts)])
                 (lambda () (run-eseq-script! e sc))))
  (check! "after" (lambda () (sek-validate-eseq e) #f))
  (check! "after" (lambda ()
                    (define l (eseq->list e))
                    (and (foreign? l) 'foreign-value)))
  (check! "continuing" (lambda ()
                         (for ([i (in-range 300)]) (eseq-push-back! e i) (eseq-pop-front! e))
                         #f)))

;; Unsupported: N threads share one iterator, over a sequence nobody updates.
;; Only memory safety is at stake.
(define (make-iterator-script n len)
  (define (dir) (if (zero? (random 2)) 'forward 'backward))
  (for/list ([_ (in-range len)])
    (case (random 7)
      [(0) (list 'move (dir))]
      [(1) (list 'get)]
      [(2) (list 'get-and-move (dir))]
      [(3) (list 'reach (sub1 (random (+ n 2))))]
      [(4) (list 'segment (dir))]
      [(5) (list 'segment-and-jump (dir))]
      [else (list 'reset (dir))])))

(define (run-iterator-script! it sc)
  (for ([op (in-list sc)])
    ;; moving off a sentinel and the like raise, as they would in one thread
    (with-handlers ([(lambda (x)
                       (and (exn:fail? x)
                            (not (regexp-match? #rx"precondition|invalid memory|overflowed"
                                                (exn-message x)))))
                     void])
      (define (seen x) (unless (or (not x) (fixnum? x)) (note! 'foreign-value)))
      (case (car op)
        [(move) (sek-iter-move! it (cadr op))]
        [(get) (seen (sek-iter-get* it))]
        [(get-and-move) (seen (sek-iter-get-and-move*! it (cadr op)))]
        [(reach) (sek-iter-reach! it (cadr op))]
        [(segment) (let ([s (sek-iter-segment* it (cadr op))]) (when s (for-each seen (segment->list s))))]
        [(segment-and-jump)
         (let ([s (sek-iter-segment-and-jump*! it (cadr op))]) (when s (for-each seen (segment->list s))))]
        [(reset) (sek-iter-reset! it (cadr op))]))))

(define (iterator-trial)
  (define n (random-size))
  (define s (if (zero? (random 2)) (build-pseq-by-pushes n) (build-eseq n)))
  (define l (sek->list s))
  (define it (sek-iterator s))
  (define scripts (for/list ([t (in-range nthreads)])
                    (make-iterator-script n (add1 (random 3000)))))
  (race "race" (for/list ([sc (in-list scripts)])
                 (lambda () (run-iterator-script! it sc))))
  ;; a reset puts the iterator back together
  (check! "after reset"
          (lambda ()
            (sek-iter-reset! it)
            (and (not (equal? (for/list ([_ (in-range (length l))]) (sek-iter-get-and-move! it)) l))
                 'iterator-wrong-after-reset))))

;; ------------------------------------------------------------ main

(module+ main
  (run (vector->list (current-command-line-arguments))))

(define (run args)
  (define leaf #f)
  (define node #f)
  (define threshold #f)
  (let loop ([args args])
    (unless (null? args)
      (define (value) (string->number (cadr args)))
      (case (car args)
        [("--threads") (set! nthreads (value)) (loop (cddr args))]
        [("--trials") (set! trials (value)) (loop (cddr args))]
        [("--seed") (set! seed (value)) (loop (cddr args))]
        [("--max-size") (set! max-size (value)) (loop (cddr args))]
        [("--hang-seconds") (set! hang-seconds (value)) (loop (cddr args))]
        [("--leaf") (set! leaf (value)) (loop (cddr args))]
        [("--node") (set! node (value)) (loop (cddr args))]
        [("--threshold") (set! threshold (value)) (loop (cddr args))]
        [else (set! mode (string->symbol (car args))) (loop (cdr args))])))
  (when (or leaf node threshold)
    (sek-configure! #:leaf-capacity (or leaf 128) #:node-capacity (or node 16)
                    #:short-threshold (or threshold (min 32 (or leaf 128)))))
  (random-seed seed)
  (define trial
    (case mode
      [(pseq) pseq-trial]
      [(readers) readers-trial]
      [(handoff) handoff-trial]
      [(writer) writer-trial]
      [(writers) writers-trial]
      [(iterator) iterator-trial]
      [else (error 'race-fuzz "unknown mode ~a" mode)]))
  (for ([_ (in-range trials)])
    (note! 'trials)
    (trial))
  (for ([k (sort (hash-keys tally) string<? #:key (lambda (k) (format "~a" k)))])
    (printf "~a\t~a\n" (hash-ref tally k) k)))
