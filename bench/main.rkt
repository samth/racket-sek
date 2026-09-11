#lang racket/base
;; A benchmark suite for sek, modelled on the one that accompanies the OCaml
;; library (its stack, reach, iteration, traversal, construction, fill,
;; flatten and split benchmarks) and on Figures 17 and 18 of the paper.
;;
;;   racket -y bench/main.rkt              run everything
;;   racket -y bench/main.rkt stack queue  run named scenarios
;;   racket -y bench/main.rkt --quick      fewer operations
;;
;; Every measurement is reported in nanoseconds per operation, so numbers are
;; comparable down a column.
;;
;; Every structure is measured on every operation it can perform at all, even
;; where that costs it a walk of the whole sequence -- a cons list does have a
;; back, it is just Theta(n) away.  Such operations are named in the impl's
;; `linear` field, which the scenarios use to keep the total work bounded: a
;; loop that would be quadratic is replaced by a bounded burst against a
;; sequence already built to length n, which estimates the same per-operation
;; cost.  The two paths agree to within a few percent on structures that can
;; afford both; `racket -y main.rkt burst-check` prints that comparison.
;;
;; A dash is left only where the operation does not exist: a gvector has no
;; snapshot, a persistent sequence has no in-place fill.

(require (for-syntax racket/base)
         racket/list
         racket/string
         racket/vector
         racket/treelist
         racket/mutable-treelist
         racket/fixnum
         racket/performance-hint
         racket/unsafe/ops
         data/gvector
         json
         "../sek/main.rkt")

;; bench/external.rkt reuses the contenders and the harness below.
(provide (all-defined-out))

;; ---------------------------------------------------------------- contenders

;; One record per data structure.  Mutable structures return themselves from
;; the update operations, so the same loop drives both flavours.
(struct impl (name kind
                   empty        ; nat -> s          (nat is a size hint)
                   build        ; list -> s
                   construct    ; nat proc -> s     (build n elements from scratch)
                   push-back    ; s x -> s
                   push-front   ; s x -> s
                   pop-back     ; s -> s
                   pop-front    ; s -> s
                   ref          ; s i -> v
                   set          ; s i v -> s
                   len          ; s -> nat
                   append       ; s1 s2 -> s
                   split        ; s i -> (values s1 s2)
                   take         ; s i -> first i elements (may consume s)
                   drop         ; s i -> all but the first i (may consume s)
                   for-each     ; s proc -> void
                   filter       ; s pred -> s
                   map          ; s proc -> s
                   snapshot     ; s -> immutable version, or #f
                   fresh        ; s -> s, independently mutable (identity if persistent)
                   linear)      ; symbols naming the Theta(n) operations
  #:transparent)

(define (mk name kind
            #:empty [empty #f] #:build [build #f] #:construct [construct #f]
            #:push-back [pb #f] #:push-front [pf #f]
            #:pop-back [qb #f] #:pop-front [qf #f]
            #:ref [rf #f] #:set [st #f] #:len [ln #f]
            #:append [ap #f] #:split [sp #f]
            #:take [tk #f] #:drop [dp #f] #:for-each [fe #f]
            #:filter [fl #f] #:map [mp #f]
            #:snapshot [sn #f] #:fresh [fr values] #:linear [lin '()])
  (impl name kind empty build construct pb pf qb qf rf st ln ap sp tk dp fe fl mp sn fr lin))

;; Does this structure walk the sequence to do `op`?
(define (linear? i op) (and (memq op (impl-linear i)) #t))

;; A Theta(n) operation gets a smaller operation count, so that its row costs
;; about what everyone else's does.  `measure` reports per operation, so the
;; number stays comparable down the column.
(define (cap i op n k)
  (if (linear? i op) (max 1 (min k (quotient 2000000 (max n 1)))) k))

;; A destructive operation needs an instance of its own.  For a persistent
;; structure this is free; for an ephemeral one the copy is charged to the
;; measurement, which is the honest way round -- it is what using a mutable
;; structure in a persistent way actually costs.  `eseq-copy` is O(1), so for
;; sek it is nearly free; `mutable-treelist-copy` is about 0.5 ns per element
;; and a gvector's is about 1.4.
(define (fresh-of i s) ((impl-fresh i) s))

(define eseq-impl
  (mk "eseq" 'ephemeral
      #:empty (lambda (_) (make-eseq))
      #:build list->eseq
      #:construct build-eseq
      #:push-back (lambda (s x) (eseq-push-back! s x) s)
      #:push-front (lambda (s x) (eseq-push-front! s x) s)
      #:pop-back (lambda (s) (eseq-pop-back! s) s)
      #:pop-front (lambda (s) (eseq-pop-front! s) s)
      #:ref eseq-ref
      #:set (lambda (s i v) (eseq-set! s i v) s)
      #:len eseq-length
      #:append (lambda (a b) (eseq-append! a b) a)
      #:split (lambda (s i) (eseq-split! s i))
      ;; eseq-take! keeps one side in place, which is the ephemeral spelling of
      ;; immer's take_mut; eseq-split! would build both halves
      #:take (lambda (s i) (eseq-take! s i 'front) s)
      #:drop (lambda (s i) (eseq-take! s i 'back) s)
      #:for-each (lambda (s f) (sek-for-each s f))
      #:filter sek-filter
      #:map sek-map
      #:snapshot eseq-snapshot
      #:fresh eseq-copy))

(define pseq-impl
  (mk "pseq" 'persistent
      #:empty (lambda (_) empty-pseq)
      #:build list->pseq
      #:construct build-pseq
      #:push-back pseq-push-back
      #:push-front pseq-push-front
      #:pop-back (lambda (s) (let-values ([(x s*) (pseq-pop-back s)]) s*))
      #:pop-front (lambda (s) (let-values ([(x s*) (pseq-pop-front s)]) s*))
      #:ref pseq-ref
      #:set pseq-set
      #:len pseq-length
      #:append pseq-append
      #:split pseq-split
      #:take sek-take
      #:drop sek-drop
      #:for-each (lambda (s f) (sek-for-each s f))
      #:filter sek-filter
      #:map sek-map))

(define treelist-impl
  (mk "treelist" 'persistent
      #:empty (lambda (_) empty-treelist)
      #:build list->treelist
      #:construct (lambda (n f) (for/fold ([t empty-treelist]) ([i (in-range n)]) (treelist-add t (f i))))
      #:push-back treelist-add
      #:push-front treelist-cons
      #:pop-back (lambda (s) (treelist-drop-right s 1))
      #:pop-front (lambda (s) (treelist-drop s 1))
      #:ref treelist-ref
      #:set treelist-set
      #:len treelist-length
      #:append treelist-append
      #:split (lambda (s i) (values (treelist-take s i) (treelist-drop s i)))
      #:take treelist-take
      #:drop treelist-drop
      #:for-each treelist-for-each
      #:filter (lambda (t p) (treelist-filter p t))
      #:map treelist-map))

;; Shortening a mutable treelist at the front and then copying or snapshotting
;; it used to raise `vector-length: contract violation`; that was a bug in
;; `treelist-copy-for-mutable`, which assumed every node was a bare vector and
;; met a node carrying a size vector.  Fixed in racket/collects/racket/
;; treelist.rkt, with a regression test in racket-test-core's treelist.rktl,
;; and needs a Racket newer than 9.3.0.2 to run the split-parts, take-drop and
;; slice rows below.
(define mtreelist-impl
  (mk "mutable-treelist" 'ephemeral
      #:empty (lambda (_) (make-mutable-treelist 0))
      #:build list->mutable-treelist
      #:construct (lambda (n f)
                    (define t (make-mutable-treelist 0))
                    (for ([i (in-range n)]) (mutable-treelist-add! t (f i)))
                    t)
      #:push-back (lambda (s x) (mutable-treelist-add! s x) s)
      #:push-front (lambda (s x) (mutable-treelist-cons! s x) s)
      #:pop-back (lambda (s) (mutable-treelist-drop-right! s 1) s)
      #:pop-front (lambda (s) (mutable-treelist-drop! s 1) s)
      #:ref mutable-treelist-ref
      #:set (lambda (s i v) (mutable-treelist-set! s i v) s)
      #:len mutable-treelist-length
      #:append (lambda (a b) (mutable-treelist-append! a b) a)
      ;; take!/drop! are destructive, so the prefix has to be copied off first
      #:split (lambda (s i)
                (define a (mutable-treelist-copy s))
                (mutable-treelist-take! a i)
                (define b (mutable-treelist-copy s))
                (mutable-treelist-drop! b i)
                (values a b))
      #:take (lambda (s i) (mutable-treelist-take! s i) s)
      #:drop (lambda (s i) (mutable-treelist-drop! s i) s)
      #:for-each mutable-treelist-for-each
      #:filter (lambda (t p)
                 (for/mutable-treelist ([x (in-mutable-treelist t)] #:when (p x)) x))
      #:map (lambda (t f)
              (for/mutable-treelist ([x (in-mutable-treelist t)]) (f x)))
      #:snapshot mutable-treelist-snapshot
      #:fresh mutable-treelist-copy
      #:linear '(split)))

(define gvector-impl
  (mk "gvector" 'ephemeral
      #:empty (lambda (_) (make-gvector))
      #:build (lambda (xs) (define g (make-gvector)) (for ([x (in-list xs)]) (gvector-add! g x)) g)
      #:construct (lambda (n f)
                    (define g (make-gvector))
                    (for ([i (in-range n)]) (gvector-add! g (f i)))
                    g)
      #:push-back (lambda (s x) (gvector-add! s x) s)
      ;; gvector-insert!/remove! at 0 shift the whole backing vector -- Theta(n),
      ;; but a vector-copy! rather than an element-at-a-time loop
      #:push-front (lambda (s x) (gvector-insert! s 0 x) s)
      #:pop-back (lambda (s) (gvector-remove-last! s) s)
      #:pop-front (lambda (s) (gvector-remove! s 0) s)
      #:ref gvector-ref
      #:set (lambda (s i v) (gvector-set! s i v) s)
      #:len gvector-count
      #:append (lambda (a b) (gvector-append! a b) a)
      #:split (lambda (s i)
                (define v (gvector->vector s))
                (values (vector->gvector (vector-copy v 0 i))
                        (vector->gvector (vector-copy v i))))
      #:take (lambda (s i) (vector->gvector (vector-copy (gvector->vector s) 0 i)))
      #:drop (lambda (s i) (vector->gvector (vector-copy (gvector->vector s) i)))
      #:for-each (lambda (s f) (for ([x (in-gvector s)]) (f x)))
      #:filter (lambda (g p) (for/gvector ([x (in-gvector g)] #:when (p x)) x))
      #:map (lambda (g f) (for/gvector ([x (in-gvector g)]) (f x)))
      #:fresh (lambda (s) (vector->gvector (gvector->vector s)))
      #:linear '(push-front pop-front split)))

;; A cons list does have a back and does have an index; both are Theta(n) away,
;; which is a number worth printing rather than a dash.  `list-set` is
;; racket/list's, and rebuilds the prefix.
(define list-impl
  (mk "list" 'persistent
      #:empty (lambda (_) '())
      #:build (lambda (xs) xs)
      #:construct build-list
      #:push-back (lambda (s x) (append s (list x)))
      #:push-front (lambda (s x) (cons x s))
      #:pop-back (lambda (s) (reverse (cdr (reverse s))))
      #:pop-front cdr
      #:ref list-ref
      #:set list-set
      #:len length
      #:append append
      #:split (lambda (s i) (split-at s i))
      #:take take
      #:drop drop
      #:for-each (lambda (s f) (for-each f s))
      #:filter (lambda (l p) (filter p l))
      #:map (lambda (l f) (map f l))
      #:linear '(push-back pop-back ref set len append split)))

;; A mutable box holding an immutable list: the idiomatic Racket stack.
(define boxlist-impl
  (mk "box of list" 'ephemeral
      #:empty (lambda (_) (box '()))
      #:build (lambda (xs) (box xs))
      #:construct (lambda (n f) (box (build-list n f)))
      #:push-back (lambda (s x) (set-box! s (append (unbox s) (list x))) s)
      #:push-front (lambda (s x) (set-box! s (cons x (unbox s))) s)
      #:pop-back (lambda (s) (set-box! s (reverse (cdr (reverse (unbox s))))) s)
      #:pop-front (lambda (s) (set-box! s (cdr (unbox s))) s)
      #:ref (lambda (s i) (list-ref (unbox s) i))
      #:set (lambda (s i v) (set-box! s (list-set (unbox s) i v)) s)
      #:len (lambda (s) (length (unbox s)))
      #:append (lambda (a b) (set-box! a (append (unbox a) (unbox b))) a)
      #:split (lambda (s i)
                (define-values (a b) (split-at (unbox s) i))
                (values (box a) (box b)))
      #:take (lambda (s i) (set-box! s (take (unbox s) i)) s)
      #:drop (lambda (s i) (set-box! s (drop (unbox s) i)) s)
      #:for-each (lambda (s f) (for-each f (unbox s)))
      #:filter (lambda (b p) (box (filter p (unbox b))))
      #:map (lambda (b f) (box (map f (unbox b))))
      #:fresh (lambda (s) (box (unbox s)))
      #:linear '(push-back pop-back ref set len append split)))

;; The floor: a hand-rolled growable array with nothing else on it.  A gvector
;; is the same shape -- a vector and a count, doubling on demand -- plus the
;; things that make it a library data structure: an argument check on every
;; operation, a range check on `ref`, impersonator support, a shrink pass on
;; every removal, and a dict/equal+hash/serialization surface.  This has none
;; of it, and the gap between the two rows is what that costs.
;;
;; To be that floor honestly it has to be written the way gvector's hot paths
;; are written: unsafe vector operations, and inlined rather than called across
;; a module boundary.  With safe operations it loses to a gvector on
;; construction, which would make the row measure this code rather than the
;; overhead it is supposed to isolate.
;;
;; Memory-safety invariant, the same one gvector maintains and for the same
;; reason.  At every observable point:
;;
;;     (<= (arr-n a) (vector-length (arr-vec a)))
;;
;; Reads here are `unsafe-vector*-ref`, so without this a thread that saw a
;; grown `n` beside an ungrown `vec` would read past the end of the vector and
;; corrupt the heap.  It is kept by three rules:
;;
;;   * a writer stores into the vector and only then raises `n`, so a reader
;;     that sees the old `n` with the new `vec` is still within bounds;
;;   * `arr-ensure!` installs the larger vector with `unsafe-struct*-cas!` and
;;     retries if another thread got there first, so two concurrent growers
;;     cannot leave a small vector installed beside a large `n`;
;;   * a writer captures `n` once and asks `arr-ensure!` for room for *that*
;;     `n`, never for a re-read one.  `n` is written as an absolute value
;;     computed from a stale read, so a concurrent writer can lower it -- which
;;     only loses an update, and is what "not atomic" means here -- but a
;;     growth path that re-read `n` could see the lowered value, decide no
;;     growth was needed, and then store at the `n` it had captured, past the
;;     end.  bench/array-tests.rkt caught exactly that under futures.
;;
;; `arr-ensure!` therefore does not read `n` at all: it copies the whole old
;; backing vector, whose length it already has.  Everywhere else that reads
;; both fields reads `n` first and `vec` second, so that a stale `n` is bounded
;; by a `vec` at least as new -- both only grow, so
;; n(t1) <= n(t2) <= length(vec(t2)) for t1 <= t2.
;;
;; Removals lower `n` before anything else and never shrink `vec`, so they
;; only make the invariant slacker.
;;
;; Like gvector, this buys memory safety and not atomicity: two threads pushing
;; at once can still lose an update or overwrite each other's slot.  The
;; `sync-cost` scenario prices that distinction, including what a lock costs.
(struct arr ([vec #:mutable] [n #:mutable]) #:authentic #:sealed)

;; The growth path is deliberately a separate, un-inlined function: the whole
;; point of the row is that the common case is a bounds test, a store and an
;; increment, and inlining a `make-vector` beside them buries it.
;; Make room for `need` elements.  Note that `n` is never consulted: the caller
;; has already captured the `n` it intends to write at, and re-reading it here
;; is the bug described above.
(define (arr-ensure! a need)
  (let retry ()
    (define v (arr-vec a))
    (define cap (unsafe-vector*-length v))
    (when (unsafe-fx> need cap)
      (define bigger (make-vector (unsafe-fxmax need (unsafe-fx* 2 cap)) 0))
      (vector-copy! bigger 0 v 0 cap)
      ;; install the larger vector before anyone raises n; if another thread
      ;; installed one first, look again -- theirs may already be big enough
      (unless (unsafe-struct*-cas! a 0 v bigger)
        (retry)))))

(begin-encourage-inline

  (define (arr-empty cap) (arr (make-vector (unsafe-fxmax 1 cap) 0) 0))

  (define (arr-room! a extra)
    (define need (unsafe-fx+ (arr-n a) extra))
    (unless (unsafe-fx<= need (unsafe-vector*-length (arr-vec a)))
      (arr-ensure! a need)))

  (define (arr-push-back! a x)
    (define n (arr-n a))
    (define v (arr-vec a))
    (cond
      [(unsafe-fx< n (unsafe-vector*-length v))
       (unsafe-vector*-set! v n x)
       (set-arr-n! a (unsafe-fx+ n 1))]
      [else
       (arr-ensure! a (unsafe-fx+ n 1))
       (unsafe-vector*-set! (arr-vec a) n x)
       (set-arr-n! a (unsafe-fx+ n 1))]))

  (define (arr-ref a i) (unsafe-vector*-ref (arr-vec a) i))
  (define (arr-set! a i x) (unsafe-vector*-set! (arr-vec a) i x)))

(define (arr-of xs)
  (define a (arr-empty (unsafe-fxmax 1 (length xs))))
  (for ([x (in-list xs)]) (arr-push-back! a x))
  a)
(define (arr-push-front! a x)
  (define n (arr-n a))
  (arr-ensure! a (unsafe-fx+ n 1))
  (define v (arr-vec a))
  (vector-copy! v 1 v 0 n)
  (unsafe-vector*-set! v 0 x)
  (set-arr-n! a (unsafe-fx+ 1 n)))
(define (arr-pop-front! a)
  ;; lower n first: a reader that still sees the old n reads a live slot
  (define n (arr-n a))
  (define v (arr-vec a))
  (set-arr-n! a (unsafe-fx- n 1))
  (vector-copy! v 0 v 1 n))
(define (arr-copy a [from 0] [to (arr-n a)])
  (define b (arr (make-vector (unsafe-fxmax 1 (- to from)) 0) (- to from)))
  (vector-copy! (arr-vec b) 0 (arr-vec a) from to)
  b)

;; Traversal.  Reading `vec` and `n` once and then walking the backing vector
;; with `in-vector`'s own bounds is what makes this as cheap as a vector sweep;
;; an index loop calling `arr-ref` re-reads both struct fields per element.
;; `in-arr` is the same loop as a sequence form, expanded in place by `for`,
;; and falls back to a generic sequence when used as a first-class value.
(define (in-arr/proc a)
  (define n (arr-n a))
  (in-vector (arr-vec a) 0 n))

(define-sequence-syntax in-arr
  (lambda () #'in-arr/proc)
  (lambda (stx)
    (syntax-case stx ()
      [[(x) (_ a-expr)]
       #'[(x)
          (:do-in ([(vec n) (let* ([a a-expr] [n (arr-n a)]) (values (arr-vec a) n))])
                  #t
                  ([i 0])
                  (unsafe-fx< i n)
                  ([(x) (unsafe-vector*-ref vec i)])
                  #t
                  #t
                  ((unsafe-fx+ i 1)))]]
      [_ #f])))

(define (arr-for-each a f)
  (define n (arr-n a))
  (define v (arr-vec a))
  (for ([x (in-vector v 0 n)]) (f x)))

(define array-impl
  (mk "array" 'ephemeral
      #:empty (lambda (n) (arr-empty (min (max n 8) 1024)))
      #:build arr-of
      #:construct (lambda (n f)
                    (define a (arr-empty n))
                    (for ([i (in-range n)]) (arr-push-back! a (f i)))
                    a)
      #:push-back (lambda (s x) (arr-push-back! s x) s)
      #:push-front (lambda (s x) (arr-push-front! s x) s)
      #:pop-back (lambda (s) (set-arr-n! s (unsafe-fx- (arr-n s) 1)) s)
      #:pop-front (lambda (s) (arr-pop-front! s) s)
      #:ref arr-ref
      #:set (lambda (s i v) (arr-set! s i v) s)
      #:len arr-n
      #:append (lambda (a b)
                 (define bn (arr-n b))
                 (define an (arr-n a))
                 (arr-ensure! a (unsafe-fx+ an bn))
                 (vector-copy! (arr-vec a) an (arr-vec b) 0 bn)
                 (set-arr-n! a (unsafe-fx+ an bn))
                 a)
      #:split (lambda (s i) (values (arr-copy s 0 i) (arr-copy s i)))
      ;; shortening at the back is just a smaller count; at the front it moves
      #:take (lambda (s i) (set-arr-n! s i) s)
      #:drop (lambda (s i) (arr-copy s i))
      #:for-each arr-for-each
      #:filter (lambda (a p)
                 (define out (arr-empty (arr-n a)))
                 (for ([x (in-arr a)]) (when (p x) (arr-push-back! out x)))
                 out)
      #:map (lambda (a f)
              (define out (arr-empty (arr-n a)))
              (for ([x (in-arr a)]) (arr-push-back! out (f x)))
              out)
      #:fresh arr-copy
      #:linear '(push-front pop-front append split)))

(define all-impls
  (list eseq-impl pseq-impl treelist-impl mtreelist-impl gvector-impl
        list-impl boxlist-impl array-impl))

;; ------------------------------------------------------------------ harness

(define quick? (make-parameter #f))
(define (scale n) (if (quick?) (max 1 (quotient n 8)) n))

;; The two size ladders the scenarios use.  Quick mode drops the largest step,
;; which is what dominates both the running time and the memory pressure.
(define (sizes-s) (if (quick?) '(10 1000 100000) '(10 1000 100000 1000000)))
(define (sizes-m) (if (quick?) '(100 10000 100000) '(100 10000 1000000)))
(define (big-n) (if (quick?) 200000 1000000))

;; Run thunk until at least `target` milliseconds have elapsed, and return the
;; nanoseconds per operation, where one call to thunk performs `ops`.
;; Microbenchmarks on a shared machine are noisy in one direction only, so
;; report the best of a few trials rather than a single timing.
(define trials (make-parameter 3))

(define (measure ops thunk #:target [target 150.0] #:max-reps [max-reps +inf.0])
  (define (once reps)
    (collect-garbage)
    (define start (current-inexact-monotonic-milliseconds))
    (for ([_ (in-range reps)]) (thunk))
    (- (current-inexact-monotonic-milliseconds) start))
  ;; find a repetition count that runs long enough to time reliably
  (define reps
    (let loop ([reps 1])
      (define elapsed (once reps))
      (if (and (< elapsed target) (< reps 10000000) (< reps max-reps))
          (loop (* reps (max 2 (inexact->exact (ceiling (/ target (max elapsed 0.05)))))))
          reps)))
  (define best
    (for/fold ([best +inf.0]) ([_ (in-range (trials))])
      (min best (once reps))))
  (/ (* best 1e6) (* reps ops)))

(define (fmt x)
  (cond [(not x) "-"]
        [(>= x 1000) (real->decimal-string x 0)]
        [(>= x 100) (real->decimal-string x 1)]
        [else (real->decimal-string x 2)]))

(define (pad s w)
  (define str (format "~a" s))
  (string-append str (make-string (max 1 (- w (string-length str))) #\space)))

(define (rpad s w)
  (define str (format "~a" s))
  (string-append (make-string (max 1 (- w (string-length str))) #\space) str))

;; Every table a run produces, newest first, for `--json`.  A table's title is
;; "<label>, <units>"; splitting on the last comma keeps the axis label out of
;; the chart heading.
(define recorded '())
(define current-scenario (make-parameter 'unknown))

(define (record-table! title sizes rows)
  (define m (regexp-match #rx"^(.*), ([^,]*)$" title))
  (set! recorded
        (cons (hasheq 'scenario (symbol->string (current-scenario))
                      'title (if m (cadr m) title)
                      'units (if m (caddr m) "")
                      'sizes sizes
                      'rows (for/list ([row (in-list rows)])
                              (hasheq 'name (string-trim (format "~a" (car row)))
                                      'values (for/list ([v (in-list (cdr row))])
                                                (if v (exact->inexact v) (json-null))))))
              recorded)))

;; Print one table: rows are implementations, columns are sizes.
(define (table title sizes rows)
  (record-table! title sizes rows)
  (printf "\n~a\n" title)
  (printf "~a" (pad "" 24))
  (for ([n (in-list sizes)]) (printf "~a" (rpad n 12)))
  (newline)
  (for ([row (in-list rows)])
    (printf "~a" (pad (car row) 24))
    (for ([v (in-list (cdr row))]) (printf "~a" (rpad (fmt v) 12)))
    (newline)))

;; Append this run's tables to FILE as one JSON object per line, so that the
;; one-scenario-per-process runner in run.sh accumulates a single data file.
(define (dump-json! file)
  (call-with-output-file file #:exists 'append
    (lambda (o)
      (for ([t (in-list (reverse recorded))])
        (write-json t o)
        (newline o)))))

;; --------------------------------------------------------------- scenarios

(define scenarios (make-hash))
(define scenario-order '())
(define-syntax-rule (define-scenario name doc body ...)
  (begin (hash-set! scenarios 'name (lambda () body ...))
         (set! scenario-order (cons (cons 'name doc) scenario-order))))

(define (build-of i n)
  ((impl-build i) (build-list n values)))

;; Figures 17 and 18: repeat "n pushes then n pops" until `total` pushes have
;; happened.  The peak length is n, so this measures how the cost of a stack
;; operation varies with the length of the stack.
;;
;; Where either operation walks the sequence, that loop is quadratic and will
;; not finish: a cons list grown to 10^5 elements one `append` at a time costs
;; about a minute a round.  Those rows take the burst path instead -- build to
;; length n once, then alternate a bounded number of pushes and pops, which
;; holds the length at n and estimates the same per-operation cost.
(define burst 200)

(define (op-of i sym)
  (case sym
    [(push-back) (impl-push-back i)]
    [(push-front) (impl-push-front i)]
    [(pop-back) (impl-pop-back i)]
    [(pop-front) (impl-pop-front i)]))

(define (ends-scenario title push-sym pop-sym sizes total)
  (table
   title sizes
   (for/list ([i (in-list all-impls)])
     (cons (impl-name i)
           (for/list ([n (in-list sizes)])
             (define pushf (op-of i push-sym))
             (define popf (op-of i pop-sym))
             (and pushf popf
                  (if (or (linear? i push-sym) (linear? i pop-sym))
                      (burst-cost i n pushf popf)
                      (loop-cost i n pushf popf total))))))))

;; The two ways to charge a push/pop pair against a sequence of length n.
(define (loop-cost i n pushf popf total)
  (define rounds (max 1 (quotient total n)))
  (measure (* 2 rounds n)
           (lambda ()
             (let loop ([r 0] [s ((impl-empty i) n)])
               (unless (= r rounds)
                 (define s1 (for/fold ([s s]) ([k (in-range n)]) (pushf s k)))
                 (define s2 (for/fold ([s s1]) ([k (in-range n)]) (popf s)))
                 (loop (add1 r) s2))))))

;; m pushes and then m pops, which leaves the length back at n and so needs no
;; copy between repetitions.  Alternating a push with a pop would be quicker to
;; write but is a pathological pattern for any chunked structure -- it sits on
;; a chunk boundary and allocates a chunk per pair -- and measures that rather
;; than the operation; pushing and popping in runs, as the full loop does,
;; crosses a boundary once every K operations just as the full loop does.
(define (burst-cost i n pushf popf)
  (define m (max 1 (min burst (quotient 2000000 (max n 1)))))
  (define base (build-of i n))
  (measure (* 2 m)
           #:max-reps 20000
           (lambda ()
             (define up (for/fold ([s base]) ([k (in-range m)]) (pushf s k)))
             (for/fold ([s up]) ([_ (in-range m)]) (popf s)))))

(define-scenario stack
  "push and pop at the back (the paper's Figures 17 and 18)"
  (ends-scenario "stack: push-back / pop-back, ns per operation"
                 'push-back 'pop-back
                 (sizes-s) (scale 2000000)))

(define-scenario front-stack
  "push and pop at the front"
  (ends-scenario "front stack: push-front / pop-front, ns per operation"
                 'push-front 'pop-front
                 (sizes-s) (scale 2000000)))

(define-scenario queue
  "push at the back, pop at the front"
  (ends-scenario "queue: push-back / pop-front, ns per operation"
                 'push-back 'pop-front
                 (sizes-s) (scale 2000000)))

;; What concurrency safety costs a growable array, one discipline at a time.
;;
;;   unsynchronised   store, then raise the count, with no ordering discipline
;;                    and no CAS: two growers can leave a small vector beside a
;;                    large count, and an unsafe read then goes out of bounds
;;   memory-safe      what `arr` and gvector both do: store before raising the
;;                    count, install a larger vector with `unsafe-struct*-cas!`.
;;                    Concurrent use cannot corrupt the heap; it can still lose
;;                    an update, because a slot is not reserved
;;   locked           a semaphore around the whole operation, which is what it
;;                    takes for two threads pushing at once to both be recorded
;;
;; The last row is the honest price of the word "thread-safe", as opposed to
;; the memory safety the other two rows are about.
(struct usarr ([vec #:mutable] [n #:mutable]) #:authentic #:sealed)
(define (usarr-push! a x)
  (define n (usarr-n a))
  (define v (usarr-vec a))
  (cond
    [(unsafe-fx< n (unsafe-vector*-length v))
     (unsafe-vector*-set! v n x)
     (set-usarr-n! a (unsafe-fx+ n 1))]
    [else
     (define bigger (make-vector (unsafe-fx* 2 (unsafe-vector*-length v)) 0))
     (vector-copy! bigger 0 v 0 n)
     (set-usarr-vec! a bigger)
     (unsafe-vector*-set! bigger n x)
     (set-usarr-n! a (unsafe-fx+ n 1))]))

(define-scenario sync-cost
  "what the memory-safety invariant and a lock cost a growable array"
  (define sizes (sizes-s))
  (define total (scale 2000000))
  (define (build-cost push empty)
    (for/list ([n (in-list sizes)])
      (define rounds (max 1 (quotient total n)))
      (measure (* rounds n)
               (lambda ()
                 (for ([_ (in-range rounds)])
                   (define a (empty))
                   (for ([k (in-range n)]) (push a k)))))))
  (table
   "sync-cost: push-back onto a growable array, ns per push"
   sizes
   (list
    (cons "unsynchronised"
          (build-cost usarr-push! (lambda () (usarr (make-vector 8 0) 0))))
    (cons "memory-safe (array)"
          (build-cost arr-push-back! (lambda () (arr-empty 8))))
    (cons "locked (semaphore)"
          (build-cost (lambda (a x)
                        (define sema (car a))
                        (semaphore-wait sema)
                        (arr-push-back! (cdr a) x)
                        (semaphore-post sema))
                      (lambda () (cons (make-semaphore 1) (arr-empty 8)))))
    (cons "gvector"
          (build-cost gvector-add! (lambda () (make-gvector))))))
  ;; And the reading side, where the invariant costs nothing at all: it is
  ;; maintained by writers, so a reader is still one unsafe vector reference.
  (define k (scale 1000000))
  (table
   "sync-cost: ref on a growable array of 100000, ns per read"
   '("ref")
   (let ([n 100000])
     (list
      (cons "memory-safe (array)"
            (let ([a ((impl-build array-impl) (build-list n values))])
              (list (measure k (lambda ()
                                 (for/fold ([acc 0]) ([j (in-range k)])
                                   (+ acc (arr-ref a (unsafe-fxand j 65535)))))))))
      (cons "locked (semaphore)"
            (let ([a ((impl-build array-impl) (build-list n values))]
                  [sema (make-semaphore 1)])
              (list (measure k (lambda ()
                                 (for/fold ([acc 0]) ([j (in-range k)])
                                   (semaphore-wait sema)
                                   (begin0 (+ acc (arr-ref a (unsafe-fxand j 65535)))
                                           (semaphore-post sema))))))))
      (cons "gvector"
            (let ([g ((impl-build gvector-impl) (build-list n values))])
              (list (measure k (lambda ()
                                 (for/fold ([acc 0]) ([j (in-range k)])
                                   (+ acc (gvector-ref g (unsafe-fxand j 65535)))))))))))))

;; Both ways of charging a push/pop pair, for the structures that can afford
;; either, so that the burst rows above can be read alongside the loop rows.
(define-scenario burst-check
  "the loop and the burst measured against each other"
  (define sizes (sizes-s))
  (define total (scale 2000000))
  (table
   "burst-check: push-back / pop-back, ns per operation"
   sizes
   (append*
    (for/list ([i (in-list all-impls)]
               #:when (and (impl-push-back i) (impl-pop-back i)
                           (not (linear? i 'push-back))
                           (not (linear? i 'pop-back))))
      (list
       (cons (format "~a loop" (impl-name i))
             (for/list ([n (in-list sizes)])
               (loop-cost i n (impl-push-back i) (impl-pop-back i) total)))
       (cons (format "~a burst" (impl-name i))
             (for/list ([n (in-list sizes)])
               (burst-cost i n (impl-push-back i) (impl-pop-back i)))))))))

(define-scenario traversal
  "visit every element in order"
  (define sizes (sizes-m))
  (table
   "traversal: for-each over the whole sequence, ns per element"
   sizes
   (append
    (for/list ([i (in-list all-impls)])
      (cons (impl-name i)
            (for/list ([n (in-list sizes)])
              (define fe (impl-for-each i))
              (and fe
                   (let ([s (build-of i n)] [acc 0])
                     (measure n (lambda () (fe s (lambda (x) (set! acc (+ acc x)))))))))))
    ;; the two ways sek can be swept, to show what segments buy
    (list
     (cons "  pseq via fold"
           (for/list ([n (in-list sizes)])
             (define s (list->pseq (build-list n values)))
             (measure n (lambda () (sek-fold-left s + 0)))))
     (cons "  pseq via iter"
           (for/list ([n (in-list sizes)])
             (define s (list->pseq (build-list n values)))
             (measure n
                      (lambda ()
                        (define it (sek-iterator s 'forward))
                        (let loop ([acc 0])
                          (if (sek-iter-finished? it)
                              acc
                              (loop (+ acc (sek-iter-get-and-move! it 'forward)))))))))
     (cons "  vector"
           (for/list ([n (in-list sizes)])
             (define v (build-vector n values))
             (measure n (lambda () (for/fold ([acc 0]) ([x (in-vector v)]) (+ acc x))))))))))

(define-scenario random-access
  "read at uniformly random indices"
  (define sizes (sizes-m))
  (define k 100000)
  (table
   "random access: ref at random indices, ns per read"
   sizes
   (append
    (for/list ([i (in-list all-impls)])
      (cons (impl-name i)
            (for/list ([n (in-list sizes)])
              (define rf (impl-ref i))
              (and rf
                   (let* ([reads (cap i 'ref n (scale k))]
                          [s (build-of i n)]
                          [ix (build-vector reads (lambda (_) (random n)))])
                     (measure (vector-length ix)
                              (lambda ()
                                (for/fold ([acc 0]) ([j (in-vector ix)])
                                  (+ acc (rf s j))))))))))
    (list
     (cons "  vector"
           (for/list ([n (in-list sizes)])
             (define v (build-vector n values))
             (define ix (build-vector (scale k) (lambda (_) (random n))))
             (measure (vector-length ix)
                      (lambda ()
                        (for/fold ([acc 0]) ([j (in-vector ix)]) (+ acc (vector-ref v j)))))))))))

(define-scenario hops
  "read at a fixed stride, the reference's `reach` benchmark"
  (define n (big-n))
  (define k (scale 100000))
  (define strides '(1 8 64 4096))
  (define (destinations stride)
    (define v (make-vector k 0))
    (let loop ([i 0] [pos (random n)])
      (unless (= i k)
        (vector-set! v i pos)
        (loop (add1 i) (modulo (+ pos stride) n))))
    v)
  (define ixs (for/list ([d (in-list strides)]) (destinations d)))
  (table
   (format "hops over ~a elements: ref at a fixed stride, ns per read" n)
   (map (lambda (d) (format "+~a" d)) strides)
   (append
    (for/list ([i (in-list all-impls)])
      (cons (impl-name i)
            (for/list ([ix (in-list ixs)])
              (define rf (impl-ref i))
              (and rf
                   (let ([s (build-of i n)]
                         [reads (cap i 'ref n k)])
                     (measure reads
                              (lambda ()
                                (for/fold ([acc 0]) ([j (in-vector ix)] [_ (in-range reads)])
                                  (+ acc (rf s j))))))))))
    (list
     (cons "  vector"
           (for/list ([ix (in-list ixs)])
             (define v (build-vector n values))
             (measure k (lambda () (for/fold ([acc 0]) ([j (in-vector ix)]) (+ acc (vector-ref v j)))))))
     ;; an iterator remembers where it is, so a short hop stays in one chunk
     (cons "pseq iterator"
           (for/list ([ix (in-list ixs)])
             (define s (list->pseq (build-list n values)))
             (define it (sek-iterator s 'forward))
             (measure k
                      (lambda ()
                        (for/fold ([acc 0]) ([j (in-vector ix)])
                          (sek-iter-reach! it j)
                          (+ acc (sek-iter-get it)))))))))))

(define-scenario update
  "write at uniformly random indices"
  (define sizes (sizes-m))
  (define k (scale 100000))
  (table
   "update: set at random indices, ns per write"
   sizes
   (append
    (for/list ([i (in-list all-impls)])
      (cons (impl-name i)
            (for/list ([n (in-list sizes)])
              (define st (impl-set i))
              (and st
                   (let* ([writes (cap i 'set n k)]
                          [s0 (build-of i n)]
                          [ix (build-vector writes (lambda (_) (random n)))])
                     ;; write a value that differs from what is already there:
                     ;; writing a constant means that after the first pass every
                     ;; slot already holds it, and a structure with an identity
                     ;; fast path (sek has one, and so does the reference)
                     ;; measures the fast path rather than the write
                     (measure writes
                              (lambda ()
                                (for/fold ([s s0]) ([j (in-vector ix)] [c (in-naturals)])
                                  (st s j c)))))))))
    (list
     (cons "  vector"
           (for/list ([n (in-list sizes)])
             (define v (build-vector n values))
             (define ix (build-vector k (lambda (_) (random n))))
             (measure k
                      (lambda ()
                        (for ([j (in-vector ix)]) (vector-set! v j 0))))))))))

(define-scenario construction
  "build a sequence of n elements"
  (define sizes (sizes-m))
  (table
   "construction: n elements from scratch, ns per element"
   sizes
   (append
    (for/list ([i (in-list all-impls)])
      (cons (impl-name i)
            (for/list ([n (in-list sizes)])
              (define c (impl-construct i))
              (and c (measure n (lambda () (c n values)) #:max-reps 6)))))
    (list
     (cons "  vector"
           (for/list ([n (in-list sizes)])
             (measure n (lambda () (build-vector n values)))))))))

(define-scenario concat
  "concatenate two sequences repeatedly"
  (define sizes (sizes-m))
  (table
   "concat: append two sequences of n/2 elements, ns per concatenation"
   sizes
   (append
    ;; A destructive append consumes its left argument, so the ephemeral rows
    ;; pay for a fresh copy of it -- which is what appending to a mutable
    ;; sequence and keeping the original actually costs.
    (for/list ([i (in-list all-impls)])
      (cons (impl-name i)
            (for/list ([n (in-list sizes)])
              (define ap (impl-append i))
              (define a (build-of i (quotient n 2)))
              (define b (build-of i (quotient n 2)))
              (and ap (measure 1 (lambda () (ap (fresh-of i a) b)) #:max-reps 2000)))))
    (list
     (cons "  vector"
           (for/list ([n (in-list sizes)])
             (define a (build-vector (quotient n 2) values))
             (measure 1 (lambda () (vector-append a a)) #:max-reps 2000)))))))

(define-scenario split
  "split a sequence at a random index"
  (define sizes (sizes-m))
  (table
   "split: split at a random index, ns per split"
   sizes
   (append
    ;; eseq-split! consumes the sequence, so the ephemeral rows split a fresh
    ;; copy -- O(1) for an eseq, a walk for the others.
    (for/list ([i (in-list all-impls)])
      (cons (impl-name i)
            (for/list ([n (in-list sizes)])
              (define sp (impl-split i))
              (define s (build-of i n))
              (define splits (cap i 'split n (if (> n 50000) 100 1000)))
              (define ix (build-vector splits (lambda (_) (random n))))
              (and sp
                   (measure (vector-length ix)
                            (lambda ()
                              (for ([j (in-vector ix)])
                                (call-with-values (lambda () (sp (fresh-of i s) j)) void))))))))
    (list
     (cons "  vector"
           (for/list ([n (in-list sizes)])
             (define v (build-vector n values))
             (define ix (build-vector (if (> n 50000) 100 1000) (lambda (_) (random n))))
             (measure (vector-length ix)
                      (lambda ()
                        (for ([j (in-vector ix)])
                          (vector-copy v 0 j)
                          (vector-copy v j))))))))))

(define-scenario snapshot
  "taking a persistent snapshot of a sequence being built in place"
  (define sizes (sizes-m))
  ;; What one snapshot costs, after a change, as the sequence grows.  This is
  ;; the operation the paper's ownership identifiers make free: the answer
  ;; should not depend on n.
  (table
   "snapshot: one change plus one snapshot of an n-element sequence, ns"
   sizes
   (list
    (cons "eseq"
          (for/list ([n (in-list sizes)])
            (define e (list->eseq (build-list n values)))
            (define keep #f)
            (measure 1 (lambda ()
                         (eseq-set! e 0 1)
                         (set! keep (eseq-snapshot e))))))
    (cons "mutable-treelist"
          (for/list ([n (in-list sizes)])
            (define t (list->mutable-treelist (build-list n values)))
            (define keep #f)
            (measure 1 #:max-reps 2000
                     (lambda ()
                       (mutable-treelist-set! t 0 1)
                       (set! keep (mutable-treelist-snapshot t))))))))
  ;; And the scenario that motivates the whole design: build a sequence in
  ;; place while keeping a persistent snapshot every m pushes.
  (define n (scale 100000))
  (define periods '(1 10 1000 100000))
  ;; A mutable-treelist snapshot copies, so keeping n/m of them moves about
  ;; n^2/2m elements at roughly half a nanosecond each.  Run it wherever that
  ;; is under a couple of seconds, which at these sizes is everywhere.
  (define (affordable? m) (<= (/ (* n n 0.5e-9) (* 2 m)) 2.0))
  (table
   (format "snapshot: ~a pushes, keeping a snapshot every m of them, ns per push" n)
   (map (lambda (m) (format "m=~a" m)) periods)
   (list
    (cons "eseq"
          (for/list ([m (in-list periods)])
            (measure n #:max-reps 8
                     (lambda ()
                       (define e (make-eseq))
                       (define keep '())
                       (for ([k (in-range n)])
                         (eseq-push-back! e k)
                         (when (zero? (modulo k m))
                           (set! keep (cons (eseq-snapshot e) keep))))
                       keep))))
    (cons "mutable-treelist"
          (for/list ([m (in-list periods)])
            ;; its snapshots are copies, so keeping many of them is quadratic
            (and (affordable? m)
                 (measure n #:max-reps 8
                          (lambda ()
                            (define e (make-mutable-treelist 0))
                            (define keep '())
                            (for ([k (in-range n)])
                              (mutable-treelist-add! e k)
                              (when (zero? (modulo k m))
                                (set! keep (cons (mutable-treelist-snapshot e) keep))))
                            keep)))))
    (cons "pseq (persistent)"
          (for/list ([m (in-list periods)])
            (measure n #:max-reps 8
                     (lambda ()
                       (for/fold ([s empty-pseq]) ([k (in-range n)])
                         (pseq-push-back s k))))))
    (cons "treelist (persistent)"
          (for/list ([m (in-list periods)])
            (measure n #:max-reps 8
                     (lambda ()
                       (for/fold ([s empty-treelist]) ([k (in-range n)])
                         (treelist-add s k)))))))))

(define-scenario fill
  "overwrite a range of elements"
  (define n (big-n))
  (define sizes (if (quick?) '(10 1000) '(10 1000 100000)))
  (table
   (format "fill: overwrite k consecutive elements of ~a, ns per element" n)
   sizes
   (list
    (cons "eseq (sek-fill!)"
          (for/list ([k (in-list sizes)])
            (define e (list->eseq (build-list n values)))
            (measure k (lambda () (sek-fill! e 0 k 0)))))
    (cons "eseq (set! loop)"
          (for/list ([k (in-list sizes)])
            (define e (list->eseq (build-list n values)))
            (measure k (lambda () (for ([j (in-range k)]) (eseq-set! e j 0))))))
    (cons "mutable-treelist"
          (for/list ([k (in-list sizes)])
            (define t (list->mutable-treelist (build-list n values)))
            (measure k (lambda () (for ([j (in-range k)]) (mutable-treelist-set! t j 0))))))
    (cons "gvector"
          (for/list ([k (in-list sizes)])
            (define g ((impl-build gvector-impl) (build-list n values)))
            (measure k (lambda () (for ([j (in-range k)]) (gvector-set! g j 0))))))
    (cons "array"
          (for/list ([k (in-list sizes)])
            (define a ((impl-build array-impl) (build-list n values)))
            (define st (impl-set array-impl))
            (measure k (lambda () (for ([j (in-range k)]) (st a j 0))))))
    (cons "vector"
          (for/list ([k (in-list sizes)])
            (define v (build-vector n values))
            (measure k (lambda () (for ([j (in-range k)]) (vector-set! v j 0))))))
    ;; A persistent sequence has no in-place fill; the same range written one
    ;; persistent set at a time is what it costs instead.
    (cons "pseq (persistent)"
          (for/list ([k (in-list sizes)])
            (define s (build-pseq n values))
            (measure k (lambda () (for/fold ([s s]) ([j (in-range k)]) (pseq-set s j 0))))))
    (cons "treelist (persistent)"
          (for/list ([k (in-list sizes)])
            (define t (sequence->treelist (in-range n)))
            (measure k (lambda () (for/fold ([t t]) ([j (in-range k)]) (treelist-set t j 0))))))
    (cons "list (persistent)"
          (for/list ([k (in-list sizes)])
            (define l (build-list n values))
            (measure k (lambda () (for/fold ([l l]) ([j (in-range k)]) (list-set l j 0)))))))))

;; ------------------------------------------------------------------- driver

(define-scenario capacities
  "how the tunable chunk capacities trade one operation against another"
  (define n (big-n))
  (define k (scale 50000))
  (define configs (list (cons 128 16) (cons 64 16) (cons 32 16)
                        (cons 16 16) (cons 8 8) (cons 256 32)))
  (define (with-config c thunk)
    (sek-configure! #:leaf-capacity (car c) #:node-capacity (cdr c)
                    #:short-threshold (min 32 (car c)))
    (begin0 (thunk)
            (sek-configure! #:leaf-capacity 128 #:node-capacity 16
                            #:short-threshold 32)))
  (define ix (build-vector k (lambda (_) (random n))))
  (table
   (format "capacities over ~a elements: leaf/node capacity, ns per operation" n)
   (map (lambda (c) (format "~a/~a" (car c) (cdr c))) configs)
   (list
    (cons "pseq-ref"
          (for/list ([c (in-list configs)])
            (with-config c
              (lambda ()
                (define s (build-pseq n values))
                (measure k (lambda ()
                             (for/fold ([acc 0]) ([j (in-vector ix)])
                               (+ acc (pseq-ref s j)))))))))
    (cons "pseq-set"
          (for/list ([c (in-list configs)])
            (with-config c
              (lambda ()
                (define s0 (build-pseq n values))
                (measure k (lambda ()
                             (for/fold ([s s0]) ([j (in-vector ix)])
                               (pseq-set s j 0))))))))
    (cons "eseq push/pop"
          (for/list ([c (in-list configs)])
            (with-config c
              (lambda ()
                (measure (* 2 n)
                         (lambda ()
                           (define e (make-eseq))
                           (for ([j (in-range n)]) (eseq-push-back! e j))
                           (for ([_ (in-range n)]) (eseq-pop-back! e))))))))
    (cons "traversal"
          (for/list ([c (in-list configs)])
            (with-config c
              (lambda ()
                (define s (build-pseq n values))
                (measure n (lambda () (sek-fold-left s + 0))))))))))

(define-scenario transient
  "the round trip: edit a persistent sequence, update it in place, snapshot"
  (define n (big-n))
  (define counts '(1 10 1000 100000))
  (define ix (build-vector 100000 (lambda (_) (random n))))
  (define p (build-pseq n values))
  (define tl (sequence->treelist (in-range n)))
  ;; Racket's treelist has the same pair of conversions: treelist-copy is its
  ;; edit, mutable-treelist-snapshot its snapshot.
  (table
   (format "transient: edit, m in-place updates, snapshot, over ~a elements, ns per update" n)
   (map (lambda (m) (format "m=~a" m)) counts)
   (list
    (cons "sek edit/snapshot"
          (for/list ([m (in-list counts)])
            (measure m #:max-reps 200
                     (lambda ()
                       (define e (pseq-edit p))
                       (for ([k (in-range m)])
                         (eseq-set! e (vector-ref ix (modulo k 100000)) 0))
                       (eseq-snapshot e)))))
    (cons "treelist copy/snapshot"
          (for/list ([m (in-list counts)])
            (measure m #:max-reps 200
                     (lambda ()
                       (define t (treelist-copy tl))
                       (for ([k (in-range m)])
                         (mutable-treelist-set! t (vector-ref ix (modulo k 100000)) 0))
                       (mutable-treelist-snapshot t)))))
    (cons "sek persistent set"
          (for/list ([m (in-list counts)])
            (measure m #:max-reps 200
                     (lambda ()
                       (for/fold ([s p]) ([k (in-range m)])
                         (pseq-set s (vector-ref ix (modulo k 100000)) 0))))))
    (cons "treelist persistent set"
          (for/list ([m (in-list counts)])
            (measure m #:max-reps 200
                     (lambda ()
                       (for/fold ([t tl]) ([k (in-range m)])
                         (treelist-set t (vector-ref ix (modulo k 100000)) 0)))))))))

(define-scenario filter
  "the paper's motivating example: filter a persistent sequence"
  (define sizes (sizes-m))
  (define (keep? x) (zero? (modulo x 3)))
  (table
   "filter: keep one element in three, ns per input element"
   sizes
   (append
    (for/list ([i (in-list all-impls)])
      (cons (impl-name i)
            (for/list ([n (in-list sizes)])
              (define fl (impl-filter i))
              (and fl (let ([s (build-of i n)])
                        (measure n (lambda () (fl s keep?))))))))
    (list
     (cons "  vector"
           (for/list ([n (in-list sizes)])
             (define v (build-vector n values))
             (measure n (lambda () (vector-filter keep? v)))))))))

;; The command line shared by main.rkt and external.rkt:
;;   [--quick] [--careful] [--json FILE] [scenario ...]
(define (run-scenarios! banner args scenarios scenario-order)
  (when (member "--quick" args) (quick? #t))
  (when (member "--careful" args) (trials 7))
  (define json-file
    (let loop ([as args])
      (cond [(null? as) #f]
            [(and (equal? (car as) "--json") (pair? (cdr as))) (cadr as)]
            [else (loop (cdr as))])))
  (define named
    (let loop ([as args])
      (cond [(null? as) '()]
            [(equal? (car as) "--json") (loop (if (pair? (cdr as)) (cddr as) '()))]
            [(regexp-match? #rx"^--" (car as)) (loop (cdr as))]
            [else (cons (car as) (loop (cdr as)))])))
  (define chosen
    (if (null? named)
        (map car (reverse scenario-order))
        (map string->symbol named)))
  (printf "~a -- Racket ~a~a\n" banner (version) (if (quick?) " (quick)" ""))
  (for ([name (in-list chosen)])
    (define run (hash-ref scenarios name #f))
    (cond
      [run
       (collect-garbage)
       (parameterize ([current-scenario name]) (run))]
      [else (printf "\nno such scenario: ~a\navailable: ~a\n"
                    name (map car (reverse scenario-order)))]))
  (when json-file (dump-json! json-file)))

(module+ main
  (run-scenarios! "sek benchmarks"
                  (vector->list (current-command-line-arguments))
                  scenarios scenario-order))
