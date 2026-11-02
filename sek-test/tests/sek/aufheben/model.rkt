#lang racket/base
;; Model-based tests of the sequential API, driven by aufheben
;; (https://github.com/samth/aufheben), a front end for the Hegel
;; property-based testing engine.
;;
;; A state machine applies operations the engine chooses to a pool of
;; ephemeral sequences, persistent sequences and iterators, and checks every
;; result against a list model; a failure shrinks to a short series of steps.
;; For each ephemeral sequence the model counts updates, so it predicts
;; exactly which iterators are still valid.  In particular it holds sek to
;; the rule that reads -- `esek-snapshot`, `esek-copy`, traversals -- leave
;; every iterator valid, although a snapshot changes the sequence's
;; representation, and that the next update then invalidates them.
;;
;; aufheben is not a dependency of sek-test (see info.rkt).  Run with
;;   racket model.rkt [--leaf K] [--node K] [--threshold T]
;;                    [--test-cases N] [--steps N] [--seed S]

(require racket/list
         sek
         aufheben
         aufheben/runner)

(provide run)

;; ---------------------------------------------------------------- the model

;; An ephemeral sequence, its contents, and the number of updates it has seen.
(struct E (seq [model #:mutable] [version #:mutable]))
;; A persistent sequence and its contents.
(struct P (seq model))
;; An iterator, the sequence it was made from, its position, and the version
;; of that sequence it was made valid at.
(struct I (it owner [index #:mutable] [born #:mutable]))

(define (model-of s)
  (if (E? s) (E-model s) (P-model s)))
(define (seq-of s)
  (if (E? s) (E-seq s) (P-seq s)))

(define (valid? i)
  (define o (I-owner i))
  (or (P? o) (= (I-born i) (E-version o))))

;; Fresh elements, so that an element in the wrong place shows.
(define counter 0)
(define (fresh!)
  (set! counter (add1 counter))
  counter)
(define (fresh-list! n)
  (for/list ([_ (in-range n)]) (fresh!)))

;; ------------------------------------------------------------------ helpers

(define (fail! fmt . args)
  (error 'model (apply format fmt args)))

(define (same! what got want)
  (unless (equal? got want)
    (fail! "~a: got ~e, want ~e" what got want)))

;; Run `thunk`, which must raise an exn:fail whose message matches `rx`.
(define (raises! what rx thunk)
  (define r
    (with-handlers ([exn:fail? (lambda (x) x)])
      (thunk)
      #f))
  (cond
    [(not r) (fail! "~a: expected an exception matching ~s" what rx)]
    [(not (regexp-match? rx (exn-message r)))
     (fail! "~a: expected an exception matching ~s, got ~s" what rx (exn-message r))]))

(define invalidated #rx"invalidated")

(define (draw gen name) (aufheben-draw gen name))
(define (draw-int lo hi name) (draw (gen:integer-in lo hi) name))
(define (draw-of xs name) (draw (gen:sampled-from xs) name))

(define (slice l start end) (take (drop l start) (- end start)))

;; ------------------------------------------------------------------ machine

(define (machine #:max-new [max-new 40])
  (define es (make-variables))
  (define ps (make-variables))
  (define is (make-variables))
  (define all-e '())
  (define all-p '())
  (define all-i '())
  ;; An update invalidates every iterator on `e`.  One that leaves the
  ;; contents alone may or may not, so the model takes sek's word for it.
  (define (update! e model)
    (define unchanged? (equal? model (E-model e)))
    (define old (E-version e))
    (set-E-model! e model)
    (set-E-version! e (add1 old))
    (when unchanged?
      (for ([i (in-list all-i)]
            #:when (and (eq? (I-owner i) e) (= (I-born i) old)))
        (when (sek-iter-valid? (I-it i))
          (set-I-born! i (E-version e))))))
  (define (add-e! seq model)
    (define e (E seq model 0))
    (set! all-e (cons e all-e))
    (variables-add! es e)
    e)
  (define (add-p! seq model)
    (same! "a new persistent sequence" (psek->list seq) model)
    (define p (P seq model))
    (set! all-p (cons p all-p))
    (variables-add! ps p)
    p)
  (define (add-i! it owner index)
    (define i (I it owner index (if (E? owner) (E-version owner) 0)))
    (set! all-i (cons i all-i))
    (variables-add! is i)
    (same! "a new iterator's index" (sek-iter-index it) index)
    i)
  (define (draw-e) (variables-draw es))
  (define (draw-p) (variables-draw ps))
  (define (draw-s)
    (if (draw gen:boolean "persistent?") (draw-p) (draw-e)))
  (define (draw-two-e)
    (define a (draw-e))
    (define b (draw-e))
    (aufheben-assume (not (eq? a b)))
    (values a b))
  (define (draw-side) (draw-of '(front back) "side"))
  (define (draw-dir) (draw-of '(forward backward) "dir"))
  (define (draw-new-list)
    (fresh-list! (draw-int 0 max-new "n")))

  ;; ---- making sequences
  (define making
    (list
     (rule 'list->esek (lambda () (define xs (draw-new-list)) (add-e! (list->esek xs) xs)))
     (rule 'list->psek (lambda () (define xs (draw-new-list)) (add-p! (list->psek xs) xs)))
     (rule 'psek-edit
           (lambda ()
             (define p (draw-p))
             (add-e! (psek-edit (P-seq p)) (P-model p))))
     (rule 'esek-snapshot #:weight 3
           (lambda ()
             (define e (draw-e))
             (add-p! (esek-snapshot (E-seq e)) (E-model e))))
     (rule 'esek-snapshot-and-clear!
           (lambda ()
             (define e (draw-e))
             (define p (esek-snapshot-and-clear! (E-seq e)))
             (add-p! p (E-model e))
             (update! e '())))
     (rule 'esek-copy #:weight 2
           (lambda ()
             (define e (draw-e))
             (define mode (draw-of '(copy share) "mode"))
             (add-e! (esek-copy (E-seq e) #:mode mode) (E-model e))))))

  ;; ---- updating ephemeral sequences
  (define updating
    (list
     (rule 'push! #:weight 4
           (lambda ()
             (define e (draw-e))
             (define side (draw-side))
             (define xs (fresh-list! (draw-int 1 (max 1 (quotient max-new 2)) "k")))
             (for ([x (in-list xs)])
               (define alias? (draw gen:boolean "alias?"))
               (if (eq? side 'back)
                   ((if alias? esek-add! esek-push-back!) (E-seq e) x)
                   ((if alias? esek-cons! esek-push-front!) (E-seq e) x)))
             (update! e (if (eq? side 'back)
                            (append (E-model e) xs)
                            (append (reverse xs) (E-model e))))))
     (rule 'pop! #:weight 3
           (lambda ()
             (define e (draw-e))
             (define n (length (E-model e)))
             (aufheben-assume (> n 0))
             (define side (draw-side))
             (define k (draw-int 1 n "k"))
             (define got
               (for/list ([_ (in-range k)])
                 (if (eq? side 'back) (esek-pop-back! (E-seq e)) (esek-pop-front! (E-seq e)))))
             (define m (E-model e))
             (cond
               [(eq? side 'back)
                (same! "popped" got (reverse (take-right m k)))
                (update! e (drop-right m k))]
               [else
                (same! "popped" got (take m k))
                (update! e (drop m k))])))
     (rule 'esek-set! #:weight 2
           (lambda ()
             (define e (draw-e))
             (define n (length (E-model e)))
             (aufheben-assume (> n 0))
             (define i (draw-int 0 (sub1 n) "i"))
             (define v (fresh!))
             (esek-set! (E-seq e) i v)
             (update! e (list-set (E-model e) i v))))
     (rule 'esek-append!
           (lambda ()
             (define e (draw-e))
             (define other (draw-s))
             (aufheben-assume (not (eq? e other)))
             (define side (draw-side))
             (esek-append! (E-seq e) (seq-of other) side)
             (define om (model-of other))
             (update! e (if (eq? side 'back) (append (E-model e) om) (append om (E-model e))))
             (when (E? other) (update! other '()))))
     (rule 'esek-concat!
           (lambda ()
             (define-values (a b) (draw-two-e))
             (define r (esek-concat! (E-seq a) (E-seq b)))
             (add-e! r (append (E-model a) (E-model b)))
             (update! a '())
             (update! b '())))
     (rule 'esek-split!
           (lambda ()
             (define e (draw-e))
             (define m (E-model e))
             (define i (draw-int 0 (length m) "i"))
             (define-values (a b) (esek-split! (E-seq e) i))
             (add-e! a (take m i))
             (add-e! b (drop m i))
             (update! e '())))
     (rule 'esek-carve!
           (lambda ()
             (define e (draw-e))
             (define m (E-model e))
             (define i (draw-int 0 (length m) "i"))
             (define side (draw-side))
             (define r (esek-carve! (E-seq e) i side))
             (cond
               [(eq? side 'back) (add-e! r (drop m i)) (update! e (take m i))]
               [else (add-e! r (take m i)) (update! e (drop m i))])))
     (rule 'esek-take!/drop!
           (lambda ()
             (define e (draw-e))
             (define m (E-model e))
             (define i (draw-int 0 (length m) "i"))
             (define side (draw-side))
             (define take? (draw gen:boolean "take?"))
             ((if take? esek-take! esek-drop!) (E-seq e) i side)
             (update! e (if (eq? take? (eq? side 'front)) (take m i) (drop m i)))))
     (rule 'esek-clear! (lambda () (define e (draw-e)) (esek-clear! (E-seq e)) (update! e '())))
     (rule 'esek-assign!
           (lambda ()
             (define-values (a b) (draw-two-e))
             (esek-assign! (E-seq a) (E-seq b))
             (update! a (E-model b))
             (update! b '())))
     (rule 'esek-fill!
           (lambda ()
             (define e (draw-e))
             (define n (length (E-model e)))
             (define start (draw-int 0 n "start"))
             (define end (draw-int start n "end"))
             (define v (fresh!))
             (esek-fill! (E-seq e) v start end)
             (define m (E-model e))
             (update! e (append (take m start) (make-list (- end start) v) (drop m end)))))
     (rule 'esek-copy!
           (lambda ()
             (define dst (draw-e))
             (define src (draw-s))
             (define dm (E-model dst))
             (define sm (model-of src))
             (define len (draw-int 0 (min (length sm) (length dm)) "len"))
             (define src-start (draw-int 0 (- (length sm) len) "src-start"))
             (define src-end (+ src-start len))
             (define dst-start (draw-int 0 (- (length dm) len) "dst-start"))
             (esek-copy! (E-seq dst) dst-start (seq-of src) src-start src-end)
             (update! dst (append (take dm dst-start)
                                  (slice sm src-start src-end)
                                  (drop dm (+ dst-start len))))))))

  ;; ---- reading either flavor, and persistent operations
  (define reading
    (list
     (rule 'ref #:weight 2
           (lambda ()
             (define s (draw-s))
             (define m (model-of s))
             (aufheben-assume (pair? m))
             (define i (draw-int 0 (sub1 (length m)) "i"))
             (same! "sek-ref" (sek-ref (seq-of s) i) (list-ref m i))
             (same! "sek-first" (sek-first (seq-of s)) (first m))
             (same! "sek-last" (sek-last (seq-of s)) (last m))
             (when (E? s)
               (same! "esek-ref" (esek-ref (E-seq s) i) (list-ref m i)))))
     (rule 'traverse #:weight 2
           (lambda ()
             (define s (draw-s))
             (define m (model-of s))
             (define q (seq-of s))
             (same! "in-sek" (for/list ([x (in-sek q)]) x) m)
             (same! "sek-fold-right" (sek-fold-right q cons '()) m)
             (same! "sek->vector" (sek->vector q) (list->vector m))
             (same! "sek-length" (sek-length q) (length m))))
     (rule 'derive
           (lambda ()
             ;; operations that build a new sequence of the same flavor
             (define s (draw-s))
             (define m (model-of s))
             (define q (seq-of s))
             (define i (draw-int 0 (length m) "i"))
             (define j (draw-int i (length m) "j"))
             (define-values (r rm)
               (case (draw-of '(reverse sub take drop map) "derive")
                 [(reverse) (values (sek-reverse q) (reverse m))]
                 [(sub) (values (sek-sub q i (- j i)) (slice m i j))]
                 [(take) (values (sek-take q i) (take m i))]
                 [(drop) (values (sek-drop q i) (drop m i))]
                 [(map) (values (sek-map q add1) (map add1 m))]))
             (if (esek? r) (add-e! r rm) (add-p! r rm))))
     (rule 'psek-push
           (lambda ()
             (define p (draw-p))
             (define v (fresh!))
             (if (eq? (draw-side) 'back)
                 (add-p! (psek-push-back (P-seq p) v) (append (P-model p) (list v)))
                 (add-p! (psek-push-front (P-seq p) v) (cons v (P-model p))))))
     (rule 'psek-pop
           (lambda ()
             (define p (draw-p))
             (define m (P-model p))
             (aufheben-assume (pair? m))
             (cond
               [(eq? (draw-side) 'back)
                (define-values (x r) (psek-pop-back (P-seq p)))
                (same! "psek-pop-back" x (last m))
                (add-p! r (drop-right m 1))]
               [else
                (define-values (x r) (psek-pop-front (P-seq p)))
                (same! "psek-pop-front" x (first m))
                (add-p! r (rest m))])))
     (rule 'psek-set
           (lambda ()
             (define p (draw-p))
             (define m (P-model p))
             (aufheben-assume (pair? m))
             (define i (draw-int 0 (sub1 (length m)) "i"))
             (define v (fresh!))
             (add-p! (psek-set (P-seq p) i v) (list-set m i v))))
     (rule 'psek-append
           (lambda ()
             (define a (draw-p))
             (define b (draw-p))
             (add-p! (psek-append (P-seq a) (P-seq b)) (append (P-model a) (P-model b)))))
     (rule 'psek-split
           (lambda ()
             (define p (draw-p))
             (define m (P-model p))
             (define i (draw-int 0 (length m) "i"))
             (define-values (a b) (psek-split (P-seq p) i))
             (add-p! a (take m i))
             (add-p! b (drop m i))))))

  ;; ---- iterators
  (define (draw-i) (variables-draw is))
  (define (len-of i) (length (model-of (I-owner i))))
  ;; If the model says `i` is valid, run `then`, which checks the operation
  ;; against the model; otherwise the operation itself, `op`, must raise.
  (define (when-valid i what op then)
    (if (valid? i)
        (then)
        (raises! what invalidated op)))
  (define (at-sentinel? i)
    (define k (I-index i))
    (or (= k -1) (= k (len-of i))))
  (define (elem i) (list-ref (model-of (I-owner i)) (I-index i)))
  (define iterating
    (list
     (rule 'sek-iterator #:weight 3
           (lambda ()
             (define s (draw-s))
             (define n (length (model-of s)))
             (case (draw-of '(forward backward front back) "kind")
               [(forward) (add-i! (sek-iterator (seq-of s) 'forward) s 0)]
               [(backward) (add-i! (sek-iterator (seq-of s) 'backward) s (sub1 n))]
               [(front) (add-i! (sek-iterator-at-sentinel (seq-of s) 'front) s -1)]
               [(back) (add-i! (sek-iterator-at-sentinel (seq-of s) 'back) s n)])))
     (rule 'sek-iter-copy
           (lambda ()
             (define i (draw-i))
             (define (op) (sek-iter-copy (I-it i)))
             (when-valid i "sek-iter-copy" op
                         (lambda ()
                           (define j (add-i! (op) (I-owner i) (I-index i)))
                           (set-I-born! j (I-born i))))))
     (rule 'sek-iter-reset!
           (lambda ()
             (define i (draw-i))
             (define dir (draw-of '(forward backward sentinel) "dir"))
             (sek-iter-reset! (I-it i) dir)
             (define o (I-owner i))
             (when (E? o) (set-I-born! i (E-version o)))
             (set-I-index! i (case dir [(forward) 0] [(backward) (sub1 (len-of i))] [else -1]))
             (same! "index after reset" (sek-iter-index (I-it i)) (I-index i))))
     (rule 'sek-iter-move! #:weight 4
           (lambda ()
             (define i (draw-i))
             (define it (I-it i))
             (define dir (draw-dir))
             (define k (I-index i))
             (define off-end? (if (eq? dir 'forward) (= k (len-of i)) (= k -1)))
             (define step (if (eq? dir 'forward) 1 -1))
             (case (draw-of '(move get-and-move get-and-move*) "how")
               [(move)
                (define (op) (sek-iter-move! it dir))
                (when-valid i "sek-iter-move!" op
                            (lambda ()
                              (cond
                                [off-end? (raises! "moving off the end" #rx"" op)]
                                [else (op) (set-I-index! i (+ k step))])))]
               [(get-and-move)
                (define (op) (sek-iter-get-and-move! it dir))
                (when-valid i "sek-iter-get-and-move!" op
                            (lambda ()
                              (cond
                                [(at-sentinel? i) (raises! "get-and-move at a sentinel" #rx"" op)]
                                [else
                                 (same! "sek-iter-get-and-move!" (op) (elem i))
                                 (set-I-index! i (+ k step))])))]
               [(get-and-move*)
                (define (op) (sek-iter-get-and-move*! it dir))
                (when-valid i "sek-iter-get-and-move*!" op
                            (lambda ()
                              ;; at a sentinel it returns #f and stays put
                              (cond
                                [(at-sentinel? i) (same! "sek-iter-get-and-move*!" (op) #f)]
                                [else
                                 (same! "sek-iter-get-and-move*!" (op) (elem i))
                                 (set-I-index! i (+ k step))])))])))
     (rule 'sek-iter-get #:weight 3
           (lambda ()
             (define i (draw-i))
             (define it (I-it i))
             (define op
               (case (draw-of '(get get* finished? index) "which")
                 [(get) (lambda () (sek-iter-get it))]
                 [(get*) (lambda () (sek-iter-get* it))]
                 [(finished?) (lambda () (sek-iter-finished? it))]
                 [(index) (lambda () (sek-iter-index it))]))
             (when-valid i "reading through an iterator" op
                         (lambda ()
                           (same! "sek-iter-finished?" (sek-iter-finished? it) (at-sentinel? i))
                           (same! "sek-iter-length" (sek-iter-length it) (len-of i))
                           (cond
                             [(at-sentinel? i)
                              (raises! "sek-iter-get at a sentinel" #rx"sentinel" (lambda () (sek-iter-get it)))
                              (same! "sek-iter-get*" (sek-iter-get* it) #f)]
                             [else
                              (same! "sek-iter-get" (sek-iter-get it) (elem i))
                              (same! "sek-iter-get*" (sek-iter-get* it) (elem i))])))))
     (rule 'sek-iter-jump!/reach!
           (lambda ()
             (define i (draw-i))
             (define it (I-it i))
             (define target (draw-int -1 (len-of i) "target"))
             (define k (I-index i))
             (define reach? (draw gen:boolean "reach?"))
             (define (op)
               (if reach?
                   (sek-iter-reach! it target)
                   (sek-iter-jump! it (if (>= target k) 'forward 'backward) (abs (- target k)))))
             (when-valid i "sek-iter-jump!" op
                         (lambda () (op) (set-I-index! i target)))))
     (rule 'sek-iter-segment #:weight 2
           (lambda ()
             (define i (draw-i))
             (define it (I-it i))
             (define dir (draw-dir))
             (define jump? (draw gen:boolean "jump?"))
             (define (op) ((if jump? sek-iter-segment-and-jump! sek-iter-segment) it dir))
             (when-valid i "sek-iter-segment" op
                         (lambda ()
                           (cond
                             [(at-sentinel? i)
                              (raises! "sek-iter-segment at a sentinel" #rx"" op)
                              (same! "sek-iter-segment*" (sek-iter-segment* it dir) #f)]
                             [else
                              (define seg (op))
                              (define len (segment-length seg))
                              (define m (model-of (I-owner i)))
                              (define k (I-index i))
                              (when (< len 1) (fail! "an empty segment off a sentinel"))
                              (if (eq? dir 'forward)
                                  (same! "forward segment" (segment->list seg) (slice m k (min (length m) (+ k len))))
                                  (same! "backward segment" (segment->list seg) (slice m (max 0 (- k len -1)) (add1 k))))
                              (when jump?
                                (set-I-index! i (if (eq? dir 'forward) (+ k len) (- k len))))])))))
     (rule 'sek-iter-set! #:weight 2
           (lambda ()
             (define i (draw-i))
             (define o (I-owner i))
             (define v (fresh!))
             (define move? (draw gen:boolean "and-move?"))
             (define dir (draw-dir))
             (define (op) (if move?
                              (sek-iter-set-and-move! (I-it i) v dir)
                              (sek-iter-set! (I-it i) v)))
             (cond
               [(P? o) (raises! "sek-iter-set! on a persistent sequence" #rx"persistent" op)]
               [else
                (when-valid i "sek-iter-set!" op
                            (lambda ()
                              (cond
                                [(at-sentinel? i) (raises! "sek-iter-set! at a sentinel" #rx"sentinel" op)]
                                [else
                                 (op)
                                 (define k (I-index i))
                                 (update! o (list-set (E-model o) k v))
                                 (set-I-born! i (E-version o))
                                 (when move?
                                   (set-I-index! i (+ k (if (eq? dir 'forward) 1 -1))))])))])))
     (rule 'sek-iter-writable-segment
           (lambda ()
             (define i (draw-i))
             (define o (I-owner i))
             (define dir (draw-dir))
             (define (op) (sek-iter-writable-segment (I-it i) dir))
             (cond
               [(P? o) (raises! "a writable segment of a persistent sequence" #rx"persistent" op)]
               [else
                (when-valid i "sek-iter-writable-segment" op
                            (lambda ()
                              (cond
                                [(at-sentinel? i) (raises! "a writable segment at a sentinel" #rx"sentinel" op)]
                                [else
                                 (define seg (op))
                                 (define len (segment-length seg))
                                 (define k (I-index i))
                                 (define first-index (if (eq? dir 'forward) k (- k len -1)))
                                 (define j (draw-int 0 (sub1 len) "j"))
                                 (define v (fresh!))
                                 (segment-set! seg j v)
                                 (update! o (list-set (E-model o) (+ first-index j) v))
                                 (set-I-born! i (E-version o))
                                 (same! "sek-iter-get after writing through a segment"
                                        (sek-iter-get (I-it i)) (list-ref (E-model o) k))])))])))))

  ;; ---- invariants
  (define invariants
    (list
     (invariant 'contents
                (lambda ()
                  (for ([e (in-list all-e)])
                    (sek-validate-esek (E-seq e))
                    (same! "esek contents" (esek->list (E-seq e)) (E-model e))
                    (same! "esek-length" (esek-length (E-seq e)) (length (E-model e))))
                  (for ([p (in-list all-p)])
                    (sek-validate-psek (P-seq p))
                    (same! "psek contents" (psek->list (P-seq p)) (P-model p)))))
     (invariant 'iterators #:always? #t
                (lambda ()
                  (for ([i (in-list all-i)])
                    (define it (I-it i))
                    (same! "sek-iter-valid?" (sek-iter-valid? it) (valid? i))
                    (when (valid? i)
                      (sek-iter-check it)
                      (same! "sek-iter-index" (sek-iter-index it) (I-index i))))))))

  ;; start with one sequence and its snapshot, so that few steps are rejected
  (define xs0 (draw-new-list))
  (define e0 (add-e! (list->esek xs0) xs0))
  (add-p! (esek-snapshot (E-seq e0)) xs0)
  (run-state-machine (append making updating reading iterating)
                     #:invariants invariants
                     #:steps (current-steps)))

(define current-steps (make-parameter 60))

;; ---------------------------------------------------------------------- run

(define (run args)
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
        [else (error 'model "unknown option ~a" (car args))])
      (loop (cddr args))))
  (when (or leaf node threshold)
    (sek-configure! #:leaf-capacity (or leaf 128) #:node-capacity (or node 16)
                    #:short-threshold (or threshold (min 32 (or leaf 128)))))
  (define settings
    (if seed
        (make-aufheben-settings #:test-cases test-cases #:seed seed #:database #f)
        (make-aufheben-settings #:test-cases test-cases #:database #f)))
  (define r (run-aufheben-test machine #:settings settings))
  (printf "model: ~a test cases, ~a valid\n" (hash-ref r "test-cases") (hash-ref r "valid-test-cases"))
  (unless (hash-ref r "passed")
    (eprintf "model: FAILED: ~a\n" (hash-ref r "error-message" "")))
  (hash-ref r "passed"))

(module+ main
  (unless (run (vector->list (current-command-line-arguments)))
    (exit 1)))
