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
         "config.rkt"
         "chunk.rkt")

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
         pt-split
         pt-concat
         pt-for-each
         pt-items-for-each
         pt-of-list)

;; weight : total number of atomic elements below this level
(struct lvl (weight front middle back) #:authentic)

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
(define (pt-populate-sides f m b d owner)
  (cond
    [(not m)
     (if (and (chunk-empty? f) (chunk-empty? b))
         #f
         (make-level f #f b))]
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
         (make-level f* m** b*))]))

;; ----------------------------------------------------------------- get / set

;; Descend from a chunk of depth d items to the atomic element at index i.
(define (chunk-ref-atomic c i d)
  (if (eqv? d 0)
      (chunk-ref c i)
      (let-values ([(q j) (chunk-item-at c i d)])
        (chunk-ref-atomic (chunk-ref c q) j (sub1 d)))))

(define (pt-ref t i d)
  (define f (lvl-front t))
  (define m (lvl-middle t))
  (define wf (chunk-weight f))
  (define wm (pt-weight m))
  (cond
    [(< i wf) (chunk-ref-atomic f i d)]
    [(< i (+ wf wm)) (pt-ref m (- i wf) (add1 d))]
    [else (chunk-ref-atomic (lvl-back t) (- i wf wm) d)]))

(define (chunk-set-atomic c i x d owner)
  (cond
    [(eqv? d 0) (chunk-set c i x 1 1 owner)]
    [else
     (define-values (q j) (chunk-item-at c i d))
     (define inner (chunk-ref c q))
     (define inner* (chunk-set-atomic inner j x (sub1 d) owner))
     (if (eq? inner inner*)
         c
         (chunk-set c q inner* (chunk-weight inner) (chunk-weight inner*) owner))]))

(define (pt-set t i x d owner)
  (define f (lvl-front t))
  (define m (lvl-middle t))
  (define b (lvl-back t))
  (define w (lvl-weight t))
  (define wf (chunk-weight f))
  (define wm (pt-weight m))
  (cond
    [(< i wf) (lvl w (chunk-set-atomic f i x d owner) m b)]
    [(< i (+ wf wm)) (lvl w f (pt-set m (- i wf) x (add1 d) owner) b)]
    [else (lvl w f m (chunk-set-atomic b (- i wf wm) x d owner))]))

;; ---------------------------------------------------- first / last / update

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

;; 3-way split (§3.2): returns the sequence before the item that holds atomic
;; index i, that item, the index of i within it, and the sequence after it.
(define (pt-split3 t i d owner)
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
     (define-values (q j) (chunk-item-at f i d))
     (values (solo (chunk-sub f 0 q mw))
             (chunk-ref f q)
             j
             (pt-populate-sides (chunk-sub f (add1 q) (- (chunk-length f) q 1) mw) m b d owner))]
    [(>= i (+ wf wm))
     (define-values (q j) (chunk-item-at b (- i wf wm) d))
     (values (pt-populate-sides f m (chunk-sub b 0 q mw) d owner)
             (chunk-ref b q)
             j
             (solo (chunk-sub b (add1 q) (- (chunk-length b) q 1) mw)))]
    [else
     (define-values (m1 c j0 m2) (pt-split3 m (- i wf) (add1 d) owner))
     (define-values (q j) (chunk-item-at c j0 d))
     (values (pt-populate-sides f m1 (chunk-sub c 0 q mw) d owner)
             (chunk-ref c q)
             j
             (pt-populate-sides (chunk-sub c (add1 q) (- (chunk-length c) q 1) mw) m2 b d owner))]))

;; 2-way split at the top level, where every item weighs one.
(define (pt-split t i owner)
  (cond
    [(eqv? i 0) (values #f t)]
    [(eqv? i (pt-weight t)) (values t #f)]
    [else
     (define-values (t1 e j t2) (pt-split3 t i 0 owner))
     (values t1 (pt-push-front t2 e 1 0 owner))]))

;; ------------------------------------------------------------ concatenation

;; Fuse adjacent chunks, left to right, whenever two of them fit in one chunk.
;; Afterwards any two adjacent results hold more than k items together, which
;; is exactly the density invariant.
(define (fuse-chunks cs k owner)
  (let loop ([cs (filter (lambda (c) (not (chunk-empty? c))) cs)]
             [acc '()])
    (cond
      [(null? cs) (reverse acc)]
      [(null? acc) (loop (cdr cs) (list (car cs)))]
      [(<= (+ (chunk-length (car acc)) (chunk-length (car cs))) k)
       (loop (cdr cs) (cons (chunk-fuse (car acc) (car cs) owner) (cdr acc)))]
      [else (loop (cdr cs) (cons (car cs) acc))])))

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
     (define k (capacity-at d))
     ;; if the front chunk of the left level is empty then so is its middle,
     ;; and the back chunk can take its place
     (define-values (f1 b1)
       (if (chunk-empty? (lvl-front t1))
           (values (lvl-back t1) (make-chunk k owner))
           (values (lvl-front t1) (lvl-back t1))))
     (define-values (f2 b2)
       (if (chunk-empty? (lvl-back t2))
           (values (make-chunk k owner) (lvl-front t2))
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
     (define rs (fuse-chunks (append L (list (pt-first-item m2))) kf owner))
     (for/fold ([t (pt-update-front m2 (last rs) d owner)])
               ([c (in-list (reverse (drop-right rs 1)))])
       (pt-push-front t c (chunk-weight c) d owner))]
    [(not m2)
     (define rs (fuse-chunks (cons (pt-last-item m1) L) kf owner))
     (for/fold ([t (pt-update-back m1 (car rs) d owner)]) ([c (in-list (cdr rs))])
       (pt-push-back t c (chunk-weight c) d owner))]
    [else
     (define rs (fuse-chunks (append (list (pt-last-item m1)) L (list (pt-first-item m2))) kf owner))
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
        (merge-levels (pt-update-back m1 (car rs) d owner)
                      (drop-right (cdr rs) 1)
                      (pt-update-front m2 (last rs) d owner)
                      d
                      owner)])]))

;; The non-terminal case of merge: peel one level off each side, stow the
;; leftover chunks of L in the adjoining chunks, and recurse one level down.
(define (merge-levels m1 L m2 d owner)
  (define k (capacity-at d))
  (define-values (F1 B1)
    (if (chunk-empty? (lvl-front m1))
        (values (lvl-back m1) (make-chunk k owner))
        (values (lvl-front m1) (lvl-back m1))))
  (define-values (F2 B2)
    (if (chunk-empty? (lvl-back m2))
        (values (make-chunk k owner) (lvl-front m2))
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
  (define L*
    (filter (lambda (c) (not (chunk-empty? c)))
            (append (list B1*) (group-into-chunks rest* k owner) (list F2*))))
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
