#lang racket/base
;; Tunable settings for the Sek data structure.
;;
;; Charguéraud & Pottier, "A Catenable, Splittable, Transient Sequence Data
;; Structure", ICFP 2026, §4.1: the chunk capacity may depend on the depth at
;; which the chunk appears.  Their defaults are 128 at the leaves and 16 at
;; internal nodes, with a threshold of 32 for the array representation of short
;; persistent sequences (§3.5).

(provide capacity-at
         max-item-weight
         leaf-capacity
         node-capacity
         short-threshold
         overwrite-empty-slots?
         check-iterator-validity?
         sek-configure!
         (struct-out exn:fail:sek))

(struct exn:fail:sek exn:fail ())

(define leaf-cap 128)
(define node-cap 16)
(define thresh 32)

(define (leaf-capacity)
  leaf-cap)
(define (node-capacity)
  node-cap)
(define (short-threshold)
  thresh)

;; capacity-at : depth -> capacity
;; A chunk that holds items of depth d has capacity (capacity-at d).  Depth 0
;; items are the sequence's own elements; depth d+1 items are chunks of depth d
;; items, so an item of depth d+1 is a chunk of capacity (capacity-at d).
(define (capacity-at d)
  (if (eqv? d 0) leaf-cap node-cap))

;; max-item-weight : depth -> nat
;; The largest weight an item of depth d can have, i.e. the product of the
;; capacities of the levels below it.  A chunk all of whose items have this
;; weight is "packed" (§3.2) and can be indexed in O(1) by division.
(define (max-item-weight d)
  (cond
    [(eqv? d 0) 1]
    [(eqv? d 1) leaf-cap]
    [else (* leaf-cap (expt node-cap (sub1 d)))]))

;; Should a slot that becomes logically empty be overwritten?  Leaving it
;; alone saves one write but lets the garbage collector retain a value that
;; the sequence no longer holds.  Overwriting is safer and is the default.
(define overwrite? #t)
(define (overwrite-empty-slots?) overwrite?)

;; Should the use of an invalidated iterator be detected at runtime?  This
;; costs a version-number comparison per iterator operation and a sign test
;; per update; it catches a real class of programming mistake, so it too is
;; on by default.
(define checking? #t)
(define (check-iterator-validity?) checking?)

;; Settings must be chosen before any sequence is built; mixing capacities
;; within one structure breaks the density invariant checks.
(define (sek-configure! #:leaf-capacity [k0 leaf-cap]
                        #:node-capacity [k1 node-cap]
                        #:short-threshold [t thresh]
                        #:overwrite-empty-slots? [ow overwrite?]
                        #:check-iterator-validity? [ck checking?])
  (unless (and (exact-integer? k0) (>= k0 2))
    (raise-argument-error 'sek-configure! "(and/c exact-integer? (>=/c 2))" k0))
  (unless (and (exact-integer? k1) (>= k1 2))
    (raise-argument-error 'sek-configure! "(and/c exact-integer? (>=/c 2))" k1))
  (unless (and (exact-integer? t) (>= t 0) (<= t k0))
    (raise-argument-error 'sek-configure! "threshold in [0, leaf-capacity]" t))
  (set! leaf-cap k0)
  (set! node-cap k1)
  (set! thresh t)
  (set! overwrite? (and ow #t))
  (set! checking? (and ck #t)))
