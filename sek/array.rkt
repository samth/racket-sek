#lang racket/base
;; Transient arrays -- Charguéraud & Pottier, ICFP 2026, §2.
;;
;; A transient array is a fixed-size sequence available in two flavours -- an
;; ephemeral one that is updated in place and a persistent one that is not --
;; with O(1) conversions between them.
;;
;; The representation is L'orange's transient K-way tree (§2.4).  Every node
;; carries an ownership id and every ephemeral array carries one too; when they
;; match, the node is uniquely owned by that array and can be written in place.
;; A persistent array is the same tree with nobody's id on it, so every write
;; copies the path from the root (§2.3).  Converting between the flavours is
;; just a matter of handing out a fresh id: the old id disappears, and with it
;; every claim of unique ownership.
;;
;; Arity varies by layer, as suggested in §2.5: leaves hold (capacity-at 0)
;; elements and internal nodes hold (capacity-at 1) children.

(require racket/vector
         racket/fixnum
         racket/performance-hint
         "config.rkt"
         "chunk.rkt")

;; Compiled in unsafe mode.  Every function here that a caller outside the
;; library can reach checks its arguments explicitly, with `unless` rather
;; than by relying on a struct accessor or a vector reference to raise --
;; in unsafe mode those do not raise, they read whatever is at the offset.
(#%declare #:unsafe)

(provide (rename-out [parr? parray?] [earr? earray?])
         make-parray
         make-earray
         parray-length
         earray-length
         parray-ref
         earray-ref
         parray-set
         earray-set!
         earray-snapshot
         parray-edit
         parray->vector
         earray->vector
         vector->parray
         vector->earray
         parray->list
         earray->list)

;; A node holds either elements (at depth 0) or subtrees (above).
(struct node (id data) #:authentic #:sealed)

(struct parr (depth tree length) #:authentic #:sealed)
(struct earr ([id #:mutable] depth [tree #:mutable] length) #:authentic #:sealed)

;; The number of elements spanned by one child of a node at depth d, and the
;; number of elements a whole tree of depth d can hold.

;; Argument checking is explicit here, because this module is compiled in
;; unsafe mode: a struct accessor no longer raises on the wrong kind of
;; value, it reads whatever happens to be at that offset.
(define-syntax-rule (check-parray who v)
  (unless (parr? v) (raise-argument-error who "parray?" v)))
(define-syntax-rule (check-earray who v)
  (unless (earr? v) (raise-argument-error who "earray?" v)))

(begin-encourage-inline
  (define (span d)
    (max-item-weight d)))
(define (tree-capacity d)
  (* (span d) (capacity-at d)))

(define (depth-for n)
  (let loop ([d 0])
    (if (>= (tree-capacity d) (max n 1))
        d
        (loop (add1 d)))))

;; Descending one level splits an index into a child position and an index
;; within that child.  That is a division and a remainder by the child's span,
;; and an integer division is tens of cycles -- but the spans are products of
;; the capacities, and both defaults are powers of two, so the usual case is a
;; shift and a mask.  `max-item-weight-shift` already tabulates the exponent
;; for exactly this, and `chunk-item-at` already uses it; this module was
;; dividing.
(define-syntax-rule (let-descend ([q j] i d) body ...)
  (let* ([sh (max-item-weight-shift d)]
         [s (if sh 0 (span d))]
         [q (if sh (fxrshift i sh) (fxquotient i s))]
         [j (if sh (fxand i (fx- (fxlshift 1 sh) 1)) (fxremainder i s))])
    body ...))

(define (build d x id)
  (define k (capacity-at d))
  (if (fx= d 0)
      (node id (make-vector k x))
      (node id (build-vector k (lambda (_) (build (fx- d 1) x id))))))

;; ---------------------------------------------------------------- creation

(define (make-parray n x)
  (define d (depth-for n))
  (parr d (build d x no-owner) n))

(define (make-earray n x)
  (define d (depth-for n))
  (define id (fresh-id!))
  (earr id d (build d x id) n))

;; ------------------------------------------------------------------ access

(define (tree-ref t d i)
  (if (fx= d 0)
      (vector-ref (node-data t) i)
      (let-descend ([q j] i d)
        (tree-ref (vector-ref (node-data t) q) (fx- d 1) j))))

;; `fixnum?` and not `exact-nonnegative-integer?`, because the descent below
;; indexes with `fx` operations; a length is a fixnum, so a bignum index is out
;; of range by definition and this reports it as such.
(define (check-index who len i)
  (unless (and (fixnum? i) (fx>= i 0) (fx< i len))
    (bad-index who len i)))

;; Anything that is not a nonnegative fixnum below the length is out of range,
;; which is what this module reported before the guard was narrowed and what it
;; must go on reporting.
(define (bad-index who len i)
  (raise-arguments-error who "index out of range" "index" i "length" len))

(define (parray-length a)
  (check-parray 'parray-length a)
  (parr-length a))
(define (earray-length a)
  (check-earray 'earray-length a)
  (earr-length a))

(define (parray-ref a i)
  (check-index 'parray-ref (parr-length a) i)
  (tree-ref (parr-tree a) (parr-depth a) i))

(define (earray-ref a i)
  (check-index 'earray-ref (earr-length a) i)
  (tree-ref (earr-tree a) (earr-depth a) i))

;; -------------------------------------------------------------------- write

;; Copy-on-write down the path (§2.3).
(define (tree-set-p t d i x)
  (define data (vector-copy (node-data t)))
  (cond
    [(fx= d 0) (vector-set! data i x)]
    [else
     (let-descend ([q j] i d)
       (vector-set! data q (tree-set-p (vector-ref data q) (fx- d 1) j x)))])
  (node no-owner data))

(define (parray-set a i x)
  (check-index 'parray-set (parr-length a) i)
  (parr (parr-depth a) (tree-set-p (parr-tree a) (parr-depth a) i x) (parr-length a)))

;; Update in place wherever the node is uniquely owned, and copy (claiming
;; ownership of the copy) wherever it is not (§2.4).
(define (tree-set-e t d i x id)
  ;; `eq?`, not `eqv?`: an ownership id is a record, and `eqv?` on values the
  ;; compiler cannot prove are fixnums is three tag tests and a call
  (define t*
    (if (eq? (node-id t) id)
        t
        (node id (vector-copy (node-data t)))))
  (define data (node-data t*))
  (cond
    [(fx= d 0) (vector-set! data i x)]
    [else
     (let-descend ([q j] i d)
       (define child (vector-ref data q))
       (define child* (tree-set-e child (fx- d 1) j x id))
       (unless (eq? child child*)
         (vector-set! data q child*)))])
  t*)

(define (earray-set! a i x)
  (check-index 'earray-set! (earr-length a) i)
  (define t (tree-set-e (earr-tree a) (earr-depth a) i x (earr-id a)))
  (unless (eq? t (earr-tree a))
    (set-earr-tree! a t))
  (void))

;; -------------------------------------------------------------- conversions

;; Handing the array a fresh id makes every node in it unrecognizable as
;; uniquely owned, hence immutable (§2.4).
(define (earray-snapshot a)
  (check-earray 'earray-snapshot a)
  (set-earr-id! a (fresh-id!))
  (parr (earr-depth a) (earr-tree a) (earr-length a)))

(define (parray-edit a)
  (check-parray 'parray-edit a)
  (earr (fresh-id!) (parr-depth a) (parr-tree a) (parr-length a)))

(define (parray->vector a)
  (build-vector (parr-length a) (lambda (i) (parray-ref a i))))

(define (earray->vector a)
  (check-earray 'earray->vector a)
  (build-vector (earr-length a) (lambda (i) (earray-ref a i))))

(define (parray->list a)
  (check-parray 'parray->list a)
  (vector->list (parray->vector a)))
(define (earray->list a)
  (vector->list (earray->vector a)))

(define (vector->earray v)
  (define n (vector-length v))
  (cond
    [(eqv? n 0) (make-earray 0 #f)]
    [else
     (define a (make-earray n (vector-ref v 0)))
     (for ([i (in-range n)])
       (earray-set! a i (vector-ref v i)))
     a]))

(define (vector->parray v)
  (earray-snapshot (vector->earray v)))
