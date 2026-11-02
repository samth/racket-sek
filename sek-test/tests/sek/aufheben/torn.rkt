#lang racket/base
;; Torn states: fault injection for the memory-safety guards, driven by
;; aufheben (https://github.com/samth/aufheben).
;;
;; The argument in THREADS.md is that a race can leave a field of the
;; structure holding a wrong value, but never a value of the wrong kind, and
;; that the guards keep every unchecked operation's precondition true whatever
;; those values are.  Races reach such states rarely; this test makes them
;; directly.  A state machine interleaves operations on ephemeral sequences,
;; their snapshots and iterators with *corruptions*, each of which stores a
;; wrong value of the right kind in one field: a chunk's head, size or weight,
;; a support's extent or a slot above depth 0, or a field of the sequence
;; itself.  It also tears iterators' cursors, field by field, as a second
;; thread using the same iterator could.  Afterwards an operation may give a
;; wrong answer or raise, but it must not violate the precondition of an
;; unchecked operation, and it must finish.
;;
;; The values stay in the ranges a race can produce: every value a field ever
;; holds is one that some operation wrote there, so a head lies in [0, K), a
;; size in [-2, K] (racing pops can take one below zero), a slot at depth d
;; holds a chunk of depth d - 1, and so on.
;;
;; It needs the checked build, which reports a violated precondition instead
;; of reading out of bounds, and refuses to run on any other:
;;   SEK_CHECKED=1 raco make ... ; racket torn.rkt [--leaf K] [--node K]
;;     [--threshold T] [--test-cases N] [--steps N] [--seed S]

(require racket/list
         ;; the full iterator and segment interface, which `sek` does not
         ;; export
         (except-in sek sek-iterator)
         sek/private/iterator
         sek/private/segment
         (submod sek config)
         (submod sek validate)
         (only-in sek/private/ptree lvl? lvl-front lvl-middle lvl-back)
         (only-in sek/private/chunk fresh-id! chunk-capacity chunk-weight)
         (submod sek/private/chunk testing)
         (submod sek/private/ephemeral testing)
         (submod sek/private/iterator testing)
         (submod sek/private/unchecked testing)
         aufheben
         aufheben/runner)

(provide run)

;; ------------------------------------------------------------ running ops

(define violation-rx #rx"precondition violated|unchecked fixnum operation overflowed")

(define (violation? x)
  (and (exn? x) (regexp-match? violation-rx (exn-message x))))

(define op-seconds 10)

;; Run `thunk` in its own thread, so that one that never returns can be
;; stopped.  Returns its value, or #f if it raised anything but a violation.
(define (run-op name thunk)
  (define result #f)
  (define t
    (thread (lambda ()
              (set! result
                    (with-handlers ([(lambda (x) #t) (lambda (x) (box x))])
                      (list (thunk)))))))
  (unless (sync/timeout op-seconds t)
    (kill-thread t)
    (error 'torn "~a did not finish in ~a seconds" name op-seconds))
  (cond
    [(pair? result) (car result)]
    [(violation? (unbox result)) (raise (unbox result))]
    [(exn:break? (unbox result)) (raise (unbox result))]
    [else
     (aufheben-event (format "~a raised" name))
     #f]))

;; ----------------------------------------------------------- the structure

;; The chunks and trees reachable from an esek, each with its depth, which are
;; what the fields that hold them can be set to.
(define (walk-esek e on-chunk on-tree)
  (define (walk-chunk c d)
    (unless (eqv? (chunk-capacity c) 0)
      (on-chunk c d)
      (when (> d 0)
        (for ([x (in-vector (support-data (chunk-support c)))])
          (when (chunk? x) (walk-chunk x (sub1 d)))))))
  (define (walk-tree t d)
    (when (lvl? t)
      (on-tree t d)
      (walk-chunk (lvl-front t) d)
      (walk-tree (lvl-middle t) (add1 d))
      (walk-chunk (lvl-back t) d)))
  (for ([c (list (esq-front e) (esq-ifront e) (esq-iback e) (esq-back e))])
    (walk-chunk c 0))
  (for ([t (list (esq-middle e) (esq-folded e))])
    (walk-tree t 1)))

;; A cursor and the cursors on its path, one level deeper each.
(define (cursor-chain c)
  (define p (cur-path c))
  (cons c (if (cur? p) (cursor-chain p) '())))

;; A target is a field that one corruption sets:
;;   (esq e field)          a field of the esek
;;   (chunk c depth field)  head, size or weight
;;   (extent c)             the extent of c's support
;;   (slot c depth)         a slot of c's support, when depth > 0
(define (targets e)
  (define ts '())
  (define (add! t) (set! ts (cons t ts)))
  (for ([f '(front ifront iback back middle folded version id)])
    (add! (list 'esq e f)))
  (define seen (make-hasheq))
  (walk-esek e
             (lambda (c d)
               (unless (hash-ref seen c #f)
                 (hash-set! seen c #t)
                 (for ([f '(head size weight)]) (add! (list 'chunk c d f)))
                 (add! (list 'extent c))
                 (when (> d 0) (add! (list 'slot c d)))))
             void)
  (list->vector (reverse ts)))

;; ------------------------------------------------------------ the machine

(define (machine)
  (define es (make-variables))
  (define ps (make-variables))
  (define is (make-variables))
  (define all-e '())
  (define counter 0)
  (define (fresh!) (set! counter (add1 counter)) counter)
  (define (add-e! e)
    (when (esek? e)
      (set! all-e (cons e all-e))
      (variables-add! es e)))
  (define (add-p! p) (when (psek? p) (variables-add! ps p)))
  (define all-i '())
  (define (add-i! i)
    (when (sek-iter? i)
      (set! all-i (cons i all-i))
      (variables-add! is i)))
  (define (draw gen name) (aufheben-draw gen name))
  (define (draw-int lo hi name) (draw (gen:integer-in lo hi) name))
  (define (draw-of xs name) (draw (gen:sampled-from xs) name))
  (define (draw-s) (if (draw gen:boolean "persistent?") (variables-draw ps) (variables-draw es)))
  (define (draw-index) (draw-int -1 120 "index"))

  ;; ---- corruption
  (define (chunks-and-trees)
    (define chunks (make-hasheqv))
    (define trees (make-hasheqv))
    (for ([e (in-list all-e)])
      (walk-esek e
                 (lambda (c d) (hash-update! chunks d (lambda (l) (cons c l)) '()))
                 (lambda (t d) (hash-update! trees d (lambda (l) (cons t l)) '()))))
    (values chunks trees))
  (define (pick xs name) (list-ref xs (draw-int 0 (sub1 (length xs)) name)))
  (define (corrupt!)
    (define e (variables-draw es))
    (define ts (targets e))
    (define t (vector-ref ts (draw-int 0 (sub1 (vector-length ts)) "target")))
    (define-values (chunks trees) (chunks-and-trees))
    (define (a-chunk d) (pick (cons none (hash-ref chunks d '())) "chunk"))
    (define (a-tree) (pick (cons #f (hash-ref trees 1 '())) "tree"))
    (aufheben-note (format "corrupt ~a" (if (eq? (car t) 'esq) (caddr t) (car t))))
    (case (car t)
      [(esq)
       (case (caddr t)
         [(front) (set-esq-front! e (a-chunk 0))]
         [(ifront) (set-esq-ifront! e (a-chunk 0))]
         [(iback) (set-esq-iback! e (a-chunk 0))]
         [(back) (set-esq-back! e (a-chunk 0))]
         [(middle) (set-esq-middle! e (a-tree))]
         [(folded) (set-esq-folded! e (a-tree))]
         [(version) (set-esq-version! e (+ (esq-version e) (draw-int -3 3 "delta")))]
         [(id)
          (set-esq-id! e (case (draw-of '(fresh chunk none) "id")
                           [(fresh) (fresh-id!)]
                           [(chunk) (chunk-id (a-chunk 0))]
                           [(none) #f]))])]
      [(chunk)
       (define c (cadr t))
       (define k (chunk-capacity c))
       (case (cadddr t)
         [(head) (set-chunk-head! c (draw-int 0 (sub1 k) "head"))]
         [(size) (set-chunk-size! c (draw-int -2 k "size"))]
         [(weight) (set-chunk-weight! c (+ (chunk-weight c) (draw-int -40 40 "delta")))])]
      [(extent)
       (define c (cadr t))
       (define k (chunk-capacity c))
       (set-support-extent! (chunk-support c)
                            (extent (draw-int 0 (sub1 k) "head") (draw-int 0 k "size")))]
      [(slot)
       (define c (cadr t))
       (define v (support-data (chunk-support c)))
       (vector-set! v (draw-int 0 (sub1 (vector-length v)) "slot") (a-chunk (sub1 (caddr t))))]))

  ;; Tear one field of an iterator's cursor, or of a cursor on its path, as a
  ;; second thread using the same iterator could.  A cursor at depth d holds
  ;; chunks and supports of depth d and a middle tree and path cursor of depth
  ;; d + 1; its indices are small fixnums.
  (define (tear-iterator!)
    (define it (variables-draw is))
    (define c (pick (cursor-chain (siter-cursor it)) "cursor"))
    (define d (cur-depth c))
    (define-values (chunks trees) (chunks-and-trees))
    (define (a-chunk-or-#f)
      (if (draw gen:boolean "#f?") #f (pick (cons none (hash-ref chunks d '())) "chunk")))
    (define (an-index name) (draw-int -1 140 name))
    (define field
      (draw-of '(path chunk support icur ishd istl ishd-sch w wbase wt front ifront middle iback back)
               "field"))
    (aufheben-note (format "tear the ~a of a cursor at depth ~a" field d))
    (case field
      [(path)
       (define deeper
         (for*/list ([i (in-list all-i)]
                     [x (in-list (cursor-chain (siter-cursor i)))]
                     #:when (eqv? (cur-depth x) (add1 d)))
           x))
       (set-cur-path! c (pick (append '(sf sb front ifront iback back) deeper) "path"))]
      [(chunk) (set-cur-chunk! c (a-chunk-or-#f))]
      [(support)
       (set-cur-support! c (if (draw gen:boolean "empty?")
                               (vector)
                               (support-data (chunk-support (pick (cons none (hash-ref chunks d '()))
                                                                  "chunk")))))]
      [(icur) (set-cur-icur! c (an-index "icur"))]
      [(ishd) (set-cur-ishd! c (an-index "ishd"))]
      [(istl) (set-cur-istl! c (an-index "istl"))]
      [(ishd-sch) (set-cur-ishd-sch! c (an-index "ishd-sch"))]
      [(w) (set-cur-w! c (an-index "w"))]
      [(wbase) (set-cur-wbase! c (an-index "wbase"))]
      [(wt) (set-cur-wt! c (draw-int 0 200 "wt"))]
      [(front) (set-cur-front! c (a-chunk-or-#f))]
      [(ifront) (set-cur-ifront! c (a-chunk-or-#f))]
      [(iback) (set-cur-iback! c (a-chunk-or-#f))]
      [(back) (set-cur-back! c (a-chunk-or-#f))]
      [(middle) (set-cur-middle! c (pick (cons #f (hash-ref trees (add1 d) '())) "tree"))]))

  ;; ---- operations
  (define-syntax op
    (syntax-rules ()
      [(_ name #:weight w body ...) (rule 'name (lambda () body ...) #:weight w)]
      [(_ name body ...) (rule 'name (lambda () body ...))]))
  (define rules
    (list
     (rule 'corrupt #:weight 4 corrupt!)
     (rule 'tear-iterator #:weight 3 tear-iterator!)
     (op make
         (define xs (for/list ([_ (in-range (draw-int 0 80 "n"))]) (fresh!)))
         (add-e! (list->esek xs)))
     (op psek-edit
         (define p (variables-draw ps))
         (add-e! (run-op "psek-edit" (lambda () (psek-edit p)))))
     (op push!
         (define e (variables-draw es))
         (define back? (draw gen:boolean "back?"))
         (define xs (for/list ([_ (in-range (draw-int 1 30 "k"))]) (fresh!)))
         (run-op "push" (lambda ()
                          (for ([x (in-list xs)])
                            (if back? (esek-push-back! e x) (esek-push-front! e x))))))
     (op pop!
         (define e (variables-draw es))
         (define back? (draw gen:boolean "back?"))
         (define k (draw-int 1 30 "k"))
         (run-op "pop" (lambda ()
                         (for ([_ (in-range k)])
                           (if back? (esek-pop-back! e) (esek-pop-front! e))))))
     (op esek-set!
         (define e (variables-draw es))
         (define i (draw-index))
         (define v (fresh!))
         (run-op "esek-set!" (lambda () (esek-set! e i v))))
     (op read #:weight 3
         (define s (draw-s))
         (define s2 (draw-s))
         (define i (draw-index))
         (case (draw-of '(ref list vector fold for in-sek length first last validate equal) "read")
           [(ref) (run-op "sek-ref" (lambda () (sek-ref s i)))]
           [(list) (run-op "sek->list" (lambda () (sek->list s)))]
           [(vector) (run-op "sek->vector" (lambda () (sek->vector s)))]
           [(fold) (run-op "sek-fold-left" (lambda () (sek-fold-left s + 0)))
                   (run-op "sek-fold-right" (lambda () (sek-fold-right s cons '())))]
           [(for) (run-op "for" (lambda () (for/list ([x s]) x)))]
           [(in-sek) (run-op "in-sek" (lambda () (for/sum ([x (in-sek s)]) 1)))]
           [(length) (run-op "sek-length" (lambda () (sek-length s)))]
           [(first) (run-op "sek-first" (lambda () (sek-first s)))]
           [(last) (run-op "sek-last" (lambda () (sek-last s)))]
           [(validate) (run-op "validate" (lambda () (if (esek? s) (sek-validate-esek s) (sek-validate-psek s))))]
           [(equal) (run-op "equal?" (lambda () (equal? s s2)))]))
     (op snapshot #:weight 2
         (define e (variables-draw es))
         (add-p! (run-op "esek-snapshot" (lambda () (esek-snapshot e)))))
     (op copy
         (define e (variables-draw es))
         (define mode (draw-of '(copy share) "mode"))
         (add-e! (run-op "esek-copy" (lambda () (esek-copy e #:mode mode)))))
     (op restructure
         (define a (variables-draw es))
         (define b (variables-draw es))
         (define p (variables-draw ps))
         (define i (draw-index))
         (define front? (draw gen:boolean "front?"))
         (define side (if front? 'front 'back))
         (case (draw-of '(append-e append-p concat split carve take drop clear assign
                          snapshot-and-clear)
                        "how")
           [(append-e) (run-op "esek-append!" (lambda () (esek-append! a b side)))]
           [(append-p) (run-op "esek-append!" (lambda () (esek-append! a p side)))]
           [(concat) (add-e! (run-op "esek-concat!" (lambda () (esek-concat! a b))))]
           [(split)
            (define r (run-op "esek-split!" (lambda () (call-with-values (lambda () (esek-split! a i)) list))))
            (when r (for-each add-e! r))]
           [(carve) (add-e! (run-op "esek-carve!" (lambda () (esek-carve! a i side))))]
           [(take) (run-op "esek-take!" (lambda () (esek-take! a i side)))]
           [(drop) (run-op "esek-drop!" (lambda () (esek-drop! a i side)))]
           [(clear) (run-op "esek-clear!" (lambda () (esek-clear! a)))]
           [(assign) (run-op "esek-assign!" (lambda () (esek-assign! a b)))]
           [(snapshot-and-clear)
            (add-p! (run-op "esek-snapshot-and-clear!" (lambda () (esek-snapshot-and-clear! a))))]))
     (op persistent
         (define p (variables-draw ps))
         (define q (variables-draw ps))
         (define i (draw-index))
         (define v (fresh!))
         (define (r name thunk) (add-p! (run-op name thunk)))
         (case (draw-of '(push-back push-front pop-back pop-front set append split take drop) "how")
           [(push-back) (r "psek-push-back" (lambda () (psek-push-back p v)))]
           [(push-front) (r "psek-push-front" (lambda () (psek-push-front p v)))]
           [(pop-back) (r "psek-pop-back" (lambda () (let-values ([(x r) (psek-pop-back p)]) r)))]
           [(pop-front) (r "psek-pop-front" (lambda () (let-values ([(x r) (psek-pop-front p)]) r)))]
           [(set) (r "psek-set" (lambda () (psek-set p i v)))]
           [(append) (r "psek-append" (lambda () (psek-append p q)))]
           [(split) (r "psek-split" (lambda () (let-values ([(a b) (psek-split p i)]) b)))]
           [(take) (r "psek-take" (lambda () (psek-take p i)))]
           [(drop) (r "psek-drop" (lambda () (psek-drop p i)))]))
     (op derive
         (define s (draw-s))
         (define i (draw-index))
         (define r
           (case (draw-of '(reverse sub map filter) "how")
             [(reverse) (run-op "sek-reverse" (lambda () (sek-reverse s)))]
             [(sub) (run-op "sek-sub" (lambda () (sek-sub s (max 0 i) 3)))]
             [(map) (run-op "sek-map" (lambda () (sek-map s values)))]
             [(filter) (run-op "sek-filter" (lambda () (sek-filter s even?)))]))
         (if (esek? r) (add-e! r) (add-p! r)))
     (op iterator #:weight 2
         (define s (draw-s))
         (define how (draw-of '(forward backward front back) "how"))
         (add-i! (run-op "sek-iterator"
                         (lambda ()
                           (case how
                             [(forward backward) (sek-iterator s how)]
                             [else (sek-iterator-at-sentinel s how)])))))
     (op iterate #:weight 6
         (define it (variables-draw is))
         (define dir (draw-of '(forward backward) "dir"))
         (define i (draw-index))
         (define n (draw-int 0 20 "n"))
         (define v (fresh!))
         (case (draw-of '(move get get* get-and-move* walk jump reach segment segment-and-jump
                          segment-walk set set-and-move writable-segment reset check)
                        "how")
           [(move) (run-op "sek-iter-move!" (lambda () (sek-iter-move! it dir)))]
           [(get) (run-op "sek-iter-get" (lambda () (sek-iter-get it)))]
           [(get*) (run-op "sek-iter-get*" (lambda () (sek-iter-get* it)))]
           [(get-and-move*) (run-op "sek-iter-get-and-move*!" (lambda () (sek-iter-get-and-move*! it dir)))]
           [(walk)
            (run-op "walk" (lambda ()
                             (let loop ([k 0])
                               (when (and (< k 1000) (sek-iter-get-and-move*! it dir))
                                 (loop (add1 k))))))]
           [(jump) (run-op "sek-iter-jump!" (lambda () (sek-iter-jump! it dir n)))]
           [(reach) (run-op "sek-iter-reach!" (lambda () (sek-iter-reach! it i)))]
           [(segment) (run-op "sek-iter-segment" (lambda () (segment->list (sek-iter-segment it dir))))]
           [(segment-and-jump)
            (run-op "sek-iter-segment-and-jump!" (lambda () (segment->list (sek-iter-segment-and-jump! it dir))))]
           [(segment-walk)
            (run-op "segment walk"
                    (lambda ()
                      (let loop ([k 0])
                        (define s (and (< k 1000) (sek-iter-segment-and-jump*! it dir)))
                        (when s
                          (for ([x (in-segment s)]) x)
                          (segment-for-each s void dir)
                          (loop (add1 k))))))]
           [(set) (run-op "sek-iter-set!" (lambda () (sek-iter-set! it v)))]
           [(set-and-move) (run-op "sek-iter-set-and-move!" (lambda () (sek-iter-set-and-move! it v dir)))]
           [(writable-segment)
            (run-op "sek-iter-writable-segment"
                    (lambda ()
                      (define s (sek-iter-writable-segment it dir))
                      (unless (segment-empty? s)
                        (segment-set! s 0 v))
                      (segment->list s)))]
           [(reset) (run-op "sek-iter-reset!" (lambda () (sek-iter-reset! it dir)))]
           [(check) (run-op "sek-iter-check" (lambda () (sek-iter-check it)))]))
     (op copy-iterator
         (define it (variables-draw is))
         (add-i! (run-op "sek-iter-copy" (lambda () (sek-iter-copy it)))))))

  ;; start with one sequence and its snapshot, so that few steps are rejected
  (define e0 (list->esek (for/list ([_ (in-range (draw-int 0 80 "n"))]) (fresh!))))
  (add-e! e0)
  (add-p! (esek-snapshot e0))
  (run-state-machine rules #:steps (current-steps)))

(define current-steps (make-parameter 80))

;; ---------------------------------------------------------------------- run

(define (run args)
  (unless checked-build?
    (error 'torn "needs the checked build of sek: compile it with SEK_CHECKED=1"))
  (define leaf #f)
  (define node #f)
  (define threshold #f)
  (define test-cases 200)
  (define seed #f)
  (let loop ([args args])
    (unless (null? args)
      (define (value) (string->number (cadr args)))
      (case (car args)
        [("--leaf") (set! leaf (value))]
        [("--node") (set! node (value))]
        [("--threshold") (set! threshold (value))]
        [("--test-cases") (set! test-cases (value))]
        [("--steps") (current-steps (value))]
        [("--seed") (set! seed (value))]
        [else (error 'torn "unknown option ~a" (car args))])
      (loop (cddr args))))
  (when (or leaf node threshold)
    (sek-configure! #:leaf-capacity (or leaf 128) #:node-capacity (or node 16)
                    #:short-threshold (or threshold (min 32 (or leaf 128)))))
  (define settings
    (if seed
        (make-aufheben-settings #:test-cases test-cases #:seed seed #:database #f)
        (make-aufheben-settings #:test-cases test-cases #:database #f)))
  (define r (run-aufheben-test machine #:settings settings))
  (printf "torn: ~a test cases, ~a valid\n" (hash-ref r "test-cases") (hash-ref r "valid-test-cases"))
  (unless (hash-ref r "passed")
    (eprintf "torn: FAILED: ~a\n" (hash-ref r "error-message" "")))
  (hash-ref r "passed"))

(module+ main
  (unless (run (vector->list (current-command-line-arguments)))
    (exit 1)))
