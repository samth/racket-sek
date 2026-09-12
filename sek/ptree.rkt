#lang racket/base
;; The Sek tree -- Charguéraud & Pottier, ICFP 2026, §3.1 and §3.2.
;;
;; A sequence of items is either empty or a *level*: a front chunk, a middle
;; sequence, and a back chunk (Figure 9).  The middle sequence is itself a Sek
;; tree, one level deeper, whose items are chunks of the current level's items
;; (non-uniform recursion).  Depth 0 items are the sequence's own elements;
;; depth d+1 items are chunks of depth d items.
;;
;; Every level maintains three invariants:
;;   1. if the middle sequence is nonempty then front and back are nonempty;
;;   2. every chunk in a middle sequence is nonempty;
;;   3. two consecutive chunks of a middle sequence hold more than K items
;;      together (the density invariant, which bounds the depth).
;;
;; The empty tree is represented by #f.

(require racket/list
         racket/fixnum
         racket/performance-hint
         "config.rkt"
         "chunk.rkt")

;; Compiled in unsafe mode: this module is the inner loop of the whole
;; library, and its safe operations were costing a type check on every struct
;; field read.  Every index that reaches here has already been bounds-checked
;; by the public entry points, which do it with an explicit `unless`.
(#%declare #:unsafe)

(provide (struct-out lvl)
         pt-weight
         make-level
         pt-push-front
         pt-push-back
         pt-pop-front
         pt-pop-back
         pt-populate-sides
         pt-ref
         pt-set
         pt-own
         pt-split
         pt-take
         pt-drop
         pt-concat
         pt-for-each
         pt-items-for-each
         pt-of-list)

;; weight : total number of atomic elements below this level
(struct lvl (weight front middle back) #:authentic #:sealed)

(define (pt-weight t)
  (if t
      (lvl-weight t)
      0))

(define (make-level f m b)
  (lvl (+ (chunk-weight f) (pt-weight m) (chunk-weight b)) f m b))

;; ---------------------------------------------------------------- push / pop

;; push-front (§3.2).  x is an item of depth d whose weight is w.
(define (pt-push-front t x w d owner)
  (define k (capacity-at d))
  (cond
    [(not t) (lvl w (chunk-singleton x w k owner) #f (make-chunk k owner))]
    [else
     (define f (lvl-front t))
     (define m (lvl-middle t))
     (define b (lvl-back t))
     (define w* (+ (lvl-weight t) w))
     (cond
       [(not (chunk-full? f)) (lvl w* (chunk-push-front f x w owner) m b)]
       ;; f is full; if b is empty then so is m (invariant 1), and f can simply
       ;; become the new back chunk
       [(chunk-empty? b) (lvl w* (chunk-singleton x w k owner) m f)]
       [else
        (define m* (pt-push-front m f (chunk-weight f) (add1 d) owner))
        (lvl w* (chunk-singleton x w k owner) m* b)])]))

(define (pt-push-back t x w d owner)
  (define k (capacity-at d))
  (cond
    [(not t) (lvl w (make-chunk k owner) #f (chunk-singleton x w k owner))]
    [else
     (define f (lvl-front t))
     (define m (lvl-middle t))
     (define b (lvl-back t))
     (define w* (+ (lvl-weight t) w))
     (cond
       [(not (chunk-full? b)) (lvl w* f m (chunk-push-back b x w owner))]
       [(chunk-empty? f) (lvl w* b m (chunk-singleton x w k owner))]
       [else
        (define m* (pt-push-back m b (chunk-weight b) (add1 d) owner))
        (lvl w* f m* (chunk-singleton x w k owner))])]))

;; pop-front (§3.2) -- returns the item that was removed and the new tree.
(define (pt-pop-front t d owner)
  (define f (lvl-front t))
  (define m (lvl-middle t))
  (define b (lvl-back t))
  (define mw (measure-at d))
  (cond
    ;; f empty implies m empty (invariant 1), so the first item is in b
    [(chunk-empty? f)
     (define-values (x b*) (chunk-pop-front b mw owner))
     (values x
             (if (chunk-empty? b*)
                 #f
                 (lvl (- (lvl-weight t) (mw x)) f m b*)))]
    [else
     (define-values (x f*) (chunk-pop-front f mw owner))
     (define w* (- (lvl-weight t) (mw x)))
     (cond
       [(not (chunk-empty? f*)) (values x (lvl w* f* m b))]
       ;; the front chunk is now empty: refill it from the middle sequence,
       ;; which by invariant 2 yields a nonempty chunk
       [m
        (define-values (c m*) (pt-pop-front m (add1 d) owner))
        (values x (lvl w* c m* b))]
       [(chunk-empty? b) (values x #f)]
       [else (values x (lvl w* f* m b))])]))

(define (pt-pop-back t d owner)
  (define f (lvl-front t))
  (define m (lvl-middle t))
  (define b (lvl-back t))
  (define mw (measure-at d))
  (cond
    [(chunk-empty? b)
     (define-values (x f*) (chunk-pop-back f mw owner))
     (values x
             (if (chunk-empty? f*)
                 #f
                 (lvl (- (lvl-weight t) (mw x)) f* m b)))]
    [else
     (define-values (x b*) (chunk-pop-back b mw owner))
     (define w* (- (lvl-weight t) (mw x)))
     (cond
       [(not (chunk-empty? b*)) (values x (lvl w* f m b*))]
       [m
        (define-values (c m*) (pt-pop-back m (add1 d) owner))
        (values x (lvl w* f m* c))]
       [(chunk-empty? f) (values x #f)]
       [else (values x (lvl w* f m b*))])]))

;; populate-sides (§3.2): restore invariant 1 by moving one chunk out of the
;; middle sequence onto each side that is empty.
;; An ephemeral sequence may carry the shared zero-capacity stand-in on a side
;; it has never pushed to, which saves allocating a chunk that is never used.
;; A level cannot: pushing into its front chunk has to have somewhere to put
;; the element.  So a stand-in that survives into a level is replaced here --
;; and only here, so nothing is allocated when the side gets filled from the
;; middle sequence instead.
(begin-encourage-inline
  (define (real-chunk c d owner)
    (if (eqv? (chunk-capacity c) (capacity-at d)) c (make-chunk (capacity-at d) owner))))

(define (pt-populate-sides f m b d owner)
  (cond
    [(not m)
     (if (and (chunk-empty? f) (chunk-empty? b))
         #f
         (make-level (real-chunk f d owner) #f (real-chunk b d owner)))]
    [else
     (define-values (f* m*)
       (if (chunk-empty? f)
           (pt-pop-front m (add1 d) owner)
           (values f m)))
     (define-values (b* m**)
       (if (and (chunk-empty? b) m*)
           (pt-pop-back m* (add1 d) owner)
           (values b m*)))
     (if (and (chunk-empty? f*) (chunk-empty? b*) (not m**))
         #f
         (make-level (real-chunk f* d owner) m** (real-chunk b* d owner)))]))

;; ----------------------------------------------------------------- get / set

;; Weights, indices and depths are all fixnums -- an index is checked to be one
;; at the entry points, and a weight is a count of elements in a sequence whose
;; length is a fixnum.  Saying so matters here: with generic `<`, `+` and `-`
;; this compiled to five fixnum-tag guards and three overflow checks around
;; three comparisons and two subtractions, because nothing tells the compiler
;; what comes out of a struct field.
(define (pt-ref t i d)
  (define f (lvl-front t))
  (define m (lvl-middle t))
  (define wf (chunk-weight f))
  (define wfm (fx+ wf (pt-weight m)))
  (cond
    [(fx< i wf) (chunk-ref-atomic f i d)]
    [(fx< i wfm) (pt-ref m (fx- i wf) (fx+ d 1))]
    [else (chunk-ref-atomic (lvl-back t) (fx- i wfm) d)]))

(define (pt-set t i x d owner)
  (define f (lvl-front t))
  (define m (lvl-middle t))
  (define b (lvl-back t))
  (define w (lvl-weight t))
  (define wf (chunk-weight f))
  (define wm (pt-weight m))
  ;; If the chunk comes back unchanged, because the element was already there,
  ;; then so is the level and the spine above it need not be rebuilt either.
  (cond
    [(fx< i wf)
     (define f* (chunk-set-atomic f i x d owner))
     (if (eq? f f*) t (lvl w f* m b))]
    [(fx< i (fx+ wf wm))
     (define m* (pt-set m (fx- i wf) x (fx+ d 1) owner))
     (if (eq? m m*) t (lvl w f m* b))]
    [else
     (define b* (chunk-set-atomic b (fx- (fx- i wf) wm) x d owner))
     (if (eq? b b*) t (lvl w f m b*))]))

;; ---------------------------------------------------- first / last / update

;; Take ownership of the chunk that holds atomic index i, and of the levels
;; above it, so that a caller may write into that chunk's data vector directly.
;; The shape of the tree does not change; only the sharing does.
(define (pt-own t i d owner)
  (define f (lvl-front t))
  (define m (lvl-middle t))
  (define b (lvl-back t))
  (define w (lvl-weight t))
  (define wf (chunk-weight f))
  (define wm (pt-weight m))
  (cond
    [(< i wf) (lvl w (chunk-own-atomic f i d owner) m b)]
    [(< i (+ wf wm)) (lvl w f (pt-own m (- i wf) (add1 d) owner) b)]
    [else (lvl w f m (chunk-own-atomic b (- i wf wm) d owner))]))

(define (pt-first-item t)
  (define f (lvl-front t))
  (if (chunk-empty? f)
      (chunk-first (lvl-back t))
      (chunk-first f)))

(define (pt-last-item t)
  (define b (lvl-back t))
  (if (chunk-empty? b)
      (chunk-last (lvl-front t))
      (chunk-last b)))

;; update-front (§3.2): replace the first item without any recursive call, so
;; that merge can adjust the ends of a middle sequence in time O(K).
(define (pt-update-front t e d owner)
  (define mw (measure-at d))
  (define f (lvl-front t))
  (define m (lvl-middle t))
  (define b (lvl-back t))
  (define we (mw e))
  (cond
    [(not (chunk-empty? f))
     (define old (chunk-first f))
     (lvl (+ (lvl-weight t) (- we (mw old))) (chunk-set f 0 e (mw old) we owner) m b)]
    [else
     (define old (chunk-first b))
     (lvl (+ (lvl-weight t) (- we (mw old))) f m (chunk-set b 0 e (mw old) we owner))]))

(define (pt-update-back t e d owner)
  (define mw (measure-at d))
  (define f (lvl-front t))
  (define m (lvl-middle t))
  (define b (lvl-back t))
  (define we (mw e))
  (cond
    [(not (chunk-empty? b))
     (define old (chunk-last b))
     (lvl (+ (lvl-weight t) (- we (mw old)))
          f
          m
          (chunk-set b (sub1 (chunk-length b)) e (mw old) we owner))]
    [else
     (define old (chunk-last f))
     (lvl (+ (lvl-weight t) (- we (mw old)))
          (chunk-set f (sub1 (chunk-length f)) e (mw old) we owner)
          m
          b)]))

;; ---------------------------------------------------------------- splitting

;; Locating the split point inside a chunk also settles the weights of the two
;; pieces, with no rescan: `chunk-item-at` returns the item index q and the
;; offset j of the atomic index within that item, so everything before item q
;; weighs i - j, item q weighs (mw item), and everything after is what is left
;; of the chunk's own weight.  The reference computes them the same way --
;; `reach` hands back the prefix weight and `weight2` is a subtraction.
;; At depth 0 the items are elements, every one of weight 1, so the measure
;; needs no look at the item itself -- and `mw` would have ignored it anyway.
(define-syntax-rule (define-split-point (q j w1 wq w2) c i d mw)
  (begin
    (define-values (q j) (chunk-item-at c i d))
    (define w1 (- i j))
    (define wq (if (eqv? d 0) 1 (chunk-weight (chunk-ref c q))))
    (define w2 (- (chunk-weight c) w1 wq))))

;; The item at the split point, which the caller needs only when it is going to
;; push it back on.  At depth 0 nobody does: `pt-split`, `pt-take` and
;; `pt-drop` all ignore it, and the recursive calls that do use it are at
;; depth 1 and below.
(define-syntax-rule (split-item c q d)
  (if (eqv? d 0) #f (chunk-ref c q)))

;; The item at the split point belongs to the right-hand sequence, and the
;; reference puts it there by pushing it onto the front afterwards
;; (`PersistentSequence.split` does `SSeq.push Front s2 x`).  That copies a
;; whole chunk to prepend one element, and it turns out to be unnecessary: the
;; right-hand side's leading chunk is a view starting at item q+1 of the chunk
;; that was split, so item q is the slot immediately before it, and starting
;; the view at q instead puts the element where it belongs for nothing.
;;
;; This only works where the item at q is not itself subdivided -- that is, at
;; the outermost level, where items are elements.  `keep?` is true only in the
;; call from `pt-split` / `pt-drop`, never in the recursive ones.  It also
;; leaves the right-hand side's leading chunk non-empty, which spares
;; `pt-populate-sides` a pop from the middle sequence.
(define-syntax-rule (right-part keep? c q len w wq owner)
  (if keep?
      (chunk-sub c q (add1 len) (+ w wq) owner)
      (chunk-sub c (add1 q) len w owner)))

;; 3-way split (§3.2): returns the sequence before the item that holds atomic
;; index i, that item, the index of i within it, and the sequence after it.
;; When `keep?` is true the item is already included in the fourth result.
(define (pt-split3 t i d owner keep?)
  (define f (lvl-front t))
  (define m (lvl-middle t))
  (define b (lvl-back t))
  (define wf (chunk-weight f))
  (define wm (pt-weight m))
  (define k (capacity-at d))
  (define mw (measure-at d))
  (define (solo c)
    (if (chunk-empty? c)
        #f
        (make-level c #f (make-chunk k owner))))
  (cond
    [(< i wf)
     (define-split-point (q j w1 wq w2) f i d mw)
     (values (solo (chunk-sub f 0 q w1 owner))
             (split-item f q d)
             j
             (pt-populate-sides
              (right-part keep? f q (- (chunk-length f) q 1) w2 wq owner)
              m b d owner))]
    [(>= i (+ wf wm))
     (define-split-point (q j w1 wq w2) b (- i wf wm) d mw)
     (values (pt-populate-sides f m (chunk-sub b 0 q w1 owner) d owner)
             (split-item b q d)
             j
             (solo (right-part keep? b q (- (chunk-length b) q 1) w2 wq owner)))]
    [else
     (define-values (m1 c j0 m2) (pt-split3 m (- i wf) (add1 d) owner #f))
     (define-split-point (q j w1 wq w2) c j0 d mw)
     (values (pt-populate-sides f m1 (chunk-sub c 0 q w1 owner) d owner)
             (split-item c q d)
             j
             (pt-populate-sides
              (right-part keep? c q (- (chunk-length c) q 1) w2 wq owner)
              m2 b d owner))]))

;; Specialised versions of pt-split3 that build only the side that is wanted,
;; mirroring `take` and `drop` in ShareableSequence.  Each returns the one
;; sequence, plus the item at the split point and the offset within it, which
;; the caller needs to finish the job.  The saving is per level and compounds:
;; a 3-way split allocates a level on each side at every depth, and half of
;; that is thrown away when only one side was asked for.
(define (pt-take3 t i d owner)
  (define f (lvl-front t))
  (define m (lvl-middle t))
  (define b (lvl-back t))
  (define wf (chunk-weight f))
  (define wm (pt-weight m))
  (define k (capacity-at d))
  (define mw (measure-at d))
  (cond
    [(< i wf)
     (define-split-point (q j w1 wq w2) f i d mw)
     (values (if (eqv? q 0) #f (make-level (chunk-sub f 0 q w1 owner) #f (make-chunk k owner)))
             (split-item f q d)
             j)]
    [(>= i (+ wf wm))
     (define-split-point (q j w1 wq w2) b (- i wf wm) d mw)
     (values (pt-populate-sides f m (chunk-sub b 0 q w1 owner) d owner)
             (split-item b q d)
             j)]
    [else
     (define-values (m1 c j0) (pt-take3 m (- i wf) (add1 d) owner))
     (define-split-point (q j w1 wq w2) c j0 d mw)
     (values (pt-populate-sides f m1 (chunk-sub c 0 q w1 owner) d owner)
             (split-item c q d)
             j)]))

(define (pt-drop3 t i d owner keep?)
  (define f (lvl-front t))
  (define m (lvl-middle t))
  (define b (lvl-back t))
  (define wf (chunk-weight f))
  (define wm (pt-weight m))
  (define k (capacity-at d))
  (define mw (measure-at d))
  (cond
    [(< i wf)
     (define-split-point (q j w1 wq w2) f i d mw)
     (values (split-item f q d)
             j
             (pt-populate-sides
              (right-part keep? f q (- (chunk-length f) q 1) w2 wq owner)
              m b d owner))]
    [(>= i (+ wf wm))
     (define-split-point (q j w1 wq w2) b (- i wf wm) d mw)
     (define c* (right-part keep? b q (- (chunk-length b) q 1) w2 wq owner))
     (values (split-item b q d)
             j
             (if (chunk-empty? c*)
                 #f
                 (make-level c* #f (make-chunk k owner))))]
    [else
     (define-values (c j0 m2) (pt-drop3 m (- i wf) (add1 d) owner #f))
     (define-split-point (q j w1 wq w2) c j0 d mw)
     (values (split-item c q d)
             j
             (pt-populate-sides
              (right-part keep? c q (- (chunk-length c) q 1) w2 wq owner)
              m2 b d owner))]))

;; 2-way split at the top level, where every item weighs one.
(define (pt-split t i owner)
  (cond
    [(eqv? i 0) (values #f t)]
    [(eqv? i (pt-weight t)) (values t #f)]
    [else
     ;; keep? = #t, so t2 already holds the element at the split point
     (define-values (t1 e j t2) (pt-split3 t i 0 owner #t))
     (values t1 t2)]))

;; The first i elements, and everything from element i on.  The element at the
;; split point belongs to the second half, which is why `pt-take` can drop it
;; and `pt-drop` has to push it back on.
(define (pt-take t i owner)
  (cond
    [(eqv? i 0) #f]
    [(eqv? i (pt-weight t)) t]
    [else
     (define-values (t1 e j) (pt-take3 t i 0 owner))
     t1]))

(define (pt-drop t i owner)
  (cond
    [(eqv? i 0) t]
    [(eqv? i (pt-weight t)) #f]
    [else
     (define-values (e j t2) (pt-drop3 t i 0 owner #t))
     t2]))

;; ------------------------------------------------------------ concatenation

;; Fuse adjacent chunks, left to right, whenever two of them fit in one chunk.
;; Afterwards any two adjacent results hold more than k items together, which
;; is exactly the density invariant.
;;
;; `cs` holds at most four chunks, so dropping the empty ones with `filter` and
;; putting the result back in order with `reverse` costs more than the fusing
;; does -- together they were about a third of the time in `concat`.  This
;; skips empties as it goes and builds the answer in order.
(define (fuse-chunks cs k owner)
  (let skip ([cs cs])
    (cond
      [(null? cs) '()]
      [(chunk-empty? (car cs)) (skip (cdr cs))]
      [else
       (let scan ([acc (car cs)] [cs (cdr cs)])
         (cond
           [(null? cs) (list acc)]
           [(chunk-empty? (car cs)) (scan acc (cdr cs))]
           [(<= (+ (chunk-length acc) (chunk-length (car cs))) k)
            (scan (chunk-fuse acc (car cs) owner) (cdr cs))]
           [else (cons acc (scan (car cs) (cdr cs)))]))])))

;; The lists that merge threads between levels hold at most a handful of
;; chunks, and `last`, `drop-right` and `append` are the wrong tools for them:
;; they are contract-checked library functions that each walk the list again,
;; and profiling a concatenation at 10^4 found `last` and `drop-right` alone
;; accounting for 36% of `pt-merge`.  These do the same work in one pass and
;; without the checks.

;; Everything but the last element, and the last element.  `xs` is non-empty.
(define (split-last xs)
  (if (null? (cdr xs))
      (values '() (car xs))
      (let-values ([(front lst) (split-last (cdr xs))])
        (values (cons (car xs) front) lst))))

;; `xs` with `x` appended.
(define (snoc xs x)
  (if (null? xs)
      (list x)
      (cons (car xs) (snoc (cdr xs) x))))

(define (group-into-chunks xs k owner)
  (let loop ([xs xs]
             [acc '()])
    (cond
      [(null? xs) (reverse acc)]
      [else
       (define n (min k (length xs)))
       (loop (list-tail xs n) (cons (chunk-of-list (take xs n) k weight-measure owner) acc))])))

;; concat (§3.2)
(define (pt-concat t1 t2 d owner)
  (cond
    [(not t1) t2]
    [(not t2) t1]
    [else
     ;; If the front chunk of the left level is empty then so is its middle,
     ;; and the back chunk can take its place.  This is the reference's
     ;; `eject`, and like it this is a *swap*: the chunk being displaced is
     ;; already empty and already has this level's capacity, so it can stand in
     ;; on the other side.  Allocating a fresh empty chunk instead costs a
     ;; whole K-slot vector, which at depth 0 was most of what concatenating
     ;; two short sequences cost.
     (define-values (f1 b1)
       (if (chunk-empty? (lvl-front t1))
           (values (lvl-back t1) (lvl-front t1))
           (values (lvl-front t1) (lvl-back t1))))
     (define-values (f2 b2)
       (if (chunk-empty? (lvl-back t2))
           (values (lvl-back t2) (lvl-front t2))
           (values (lvl-front t2) (lvl-back t2))))
     (make-level f1 (pt-merge (lvl-middle t1) (list b1 f2) (lvl-middle t2) (add1 d) owner) b2)]))

;; merge (§3.2): build a middle sequence representing m1 ; L ; m2 that obeys
;; the density invariant.  m1 and m2 are trees at depth d, so their items are
;; chunks whose capacity is that of depth d-1 items.
(define (pt-merge m1 L m2 d owner)
  (define kf (capacity-at (sub1 d)))
  (cond
    ;; both middles empty: the fused list is the whole sequence
    [(and (not m1) (not m2))
     (for/fold ([t #f]) ([c (in-list (fuse-chunks L kf owner))])
       (pt-push-back t c (chunk-weight c) d owner))]
    ;; only m2 survives: fuse L with its first chunk and re-attach
    [(not m1)
     (define rs (fuse-chunks (snoc L (pt-first-item m2)) kf owner))
     (define-values (front lst) (split-last rs))
     (for/fold ([t (pt-update-front m2 lst d owner)])
               ([c (in-list (reverse front))])
       (pt-push-front t c (chunk-weight c) d owner))]
    [(not m2)
     (define rs (fuse-chunks (cons (pt-last-item m1) L) kf owner))
     (for/fold ([t (pt-update-back m1 (car rs) d owner)]) ([c (in-list (cdr rs))])
       (pt-push-back t c (chunk-weight c) d owner))]
    [else
     (define rs (fuse-chunks (cons (pt-last-item m1) (snoc L (pt-first-item m2))) kf owner))
     (cond
       [(null? (cdr rs))
        ;; everything fused into one chunk: keep it at the end of m1 and drop
        ;; the first chunk of m2
        (define m1* (pt-update-back m1 (car rs) d owner))
        (define-values (_ m2*) (pt-pop-front m2 d owner))
        (if m2*
            (merge-levels m1* '() m2* d owner)
            m1*)]
       [else
        (define-values (mid lst) (split-last (cdr rs)))
        (merge-levels (pt-update-back m1 (car rs) d owner)
                      mid
                      (pt-update-front m2 lst d owner)
                      d
                      owner)])]))

;; The non-terminal case of merge: peel one level off each side, stow the
;; leftover chunks of L in the adjoining chunks, and recurse one level down.
(define (merge-levels m1 L m2 d owner)
  (define k (capacity-at d))
  ;; the same swap as in pt-concat, rather than a fresh empty chunk
  (define-values (F1 B1)
    (if (chunk-empty? (lvl-front m1))
        (values (lvl-back m1) (lvl-front m1))
        (values (lvl-front m1) (lvl-back m1))))
  (define-values (F2 B2)
    (if (chunk-empty? (lvl-back m2))
        (values (lvl-back m2) (lvl-front m2))
        (values (lvl-front m2) (lvl-back m2))))
  ;; fill B1 from the left and F2 from the right, then box up what is left
  (define-values (B1* rest)
    (let loop ([c B1]
               [xs L])
      (if (or (null? xs) (chunk-full? c))
          (values c xs)
          (loop (chunk-push-back c (car xs) (chunk-weight (car xs)) owner) (cdr xs)))))
  (define-values (F2* rest*)
    (let loop ([c F2]
               [xs (reverse rest)])
      (if (or (null? xs) (chunk-full? c))
          (values c (reverse xs))
          (loop (chunk-push-front c (car xs) (chunk-weight (car xs)) owner) (cdr xs)))))
  ;; group-into-chunks never yields an empty chunk, so only the two ends need
  ;; testing, and the list can be built directly
  (define L*
    (let* ([tail (if (chunk-empty? F2*) '() (list F2*))]
           [tail (if (null? rest*)
                     tail
                     (append (group-into-chunks rest* k owner) tail))])
      (if (chunk-empty? B1*) tail (cons B1* tail))))
  (make-level F1 (pt-merge (lvl-middle m1) L* (lvl-middle m2) (add1 d) owner) B2))

;; ---------------------------------------------------------------- traversal

;; Expand an item of depth d into its atomic elements.
(define (item-for-each x d proc)
  (if (eqv? d 0)
      (proc x)
      (chunk-for-each x (sub1 d) proc)))

;; c holds items of depth d.
(define (chunk-for-each c d proc)
  (for ([i (in-range (chunk-length c))])
    (item-for-each (chunk-ref c i) d proc)))

(define (pt-for-each t d proc)
  (when t
    (chunk-for-each (lvl-front t) d proc)
    (pt-for-each (lvl-middle t) (add1 d) proc)
    (chunk-for-each (lvl-back t) d proc)))

;; Visit the items (not the elements) of a tree, in order.
(define (pt-items-for-each t d proc)
  (when t
    (define f (lvl-front t))
    (for ([i (in-range (chunk-length f))])
      (proc (chunk-ref f i)))
    (pt-items-for-each (lvl-middle t)
                       (add1 d)
                       (lambda (c)
                         (for ([i (in-range (chunk-length c))])
                           (proc (chunk-ref c i)))))
    (define b (lvl-back t))
    (for ([i (in-range (chunk-length b))])
      (proc (chunk-ref b i)))))

(define (pt-of-list xs owner)
  (for/fold ([t #f]) ([x (in-list xs)])
    (pt-push-back t x 1 0 owner)))
