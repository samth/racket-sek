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
         "config.rkt"
         "chunk.rkt")

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
(define (span d)
  (max-item-weight d))
(define (tree-capacity d)
  (* (span d) (capacity-at d)))

(define (depth-for n)
  (let loop ([d 0])
    (if (>= (tree-capacity d) (max n 1))
        d
        (loop (add1 d)))))

(define (build d x id)
  (define k (capacity-at d))
  (if (eqv? d 0)
      (node id (make-vector k x))
      (node id (build-vector k (lambda (_) (build (sub1 d) x id))))))

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
  (if (eqv? d 0)
      (vector-ref (node-data t) i)
      (let ([s (span d)])
        (tree-ref (vector-ref (node-data t) (quotient i s)) (sub1 d) (remainder i s)))))

(define (check-index who len i)
  (unless (and (exact-nonnegative-integer? i) (< i len))
    (raise-arguments-error who "index out of range" "index" i "length" len)))

(define (parray-length a)
  (parr-length a))
(define (earray-length a)
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
    [(eqv? d 0) (vector-set! data i x)]
    [else
     (define s (span d))
     (define q (quotient i s))
     (vector-set! data q (tree-set-p (vector-ref data q) (sub1 d) (remainder i s) x))])
  (node no-owner data))

(define (parray-set a i x)
  (check-index 'parray-set (parr-length a) i)
  (parr (parr-depth a) (tree-set-p (parr-tree a) (parr-depth a) i x) (parr-length a)))

;; Update in place wherever the node is uniquely owned, and copy (claiming
;; ownership of the copy) wherever it is not (§2.4).
(define (tree-set-e t d i x id)
  (define t*
    (if (eqv? (node-id t) id)
        t
        (node id (vector-copy (node-data t)))))
  (define data (node-data t*))
  (cond
    [(eqv? d 0) (vector-set! data i x)]
    [else
     (define s (span d))
     (define q (quotient i s))
     (define child (vector-ref data q))
     (define child* (tree-set-e child (sub1 d) (remainder i s) x id))
     (unless (eq? child child*)
       (vector-set! data q child*))])
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
  (set-earr-id! a (fresh-id!))
  (parr (earr-depth a) (earr-tree a) (earr-length a)))

(define (parray-edit a)
  (earr (fresh-id!) (parr-depth a) (parr-tree a) (parr-length a)))

(define (parray->vector a)
  (build-vector (parr-length a) (lambda (i) (parray-ref a i))))

(define (earray->vector a)
  (build-vector (earr-length a) (lambda (i) (earray-ref a i))))

(define (parray->list a)
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
