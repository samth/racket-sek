#lang racket/base
;; Tunable settings for the Sek data structure.
;;
;; Charguéraud & Pottier, "A Catenable, Splittable, Transient Sequence Data
;; Structure", ICFP 2026, §4.1: the chunk capacity may depend on the depth at
;; which the chunk appears.  Their defaults are 128 at the leaves and 16 at
;; internal nodes, with a threshold of 32 for the array representation of short
;; persistent sequences (§3.5).

(require racket/fixnum
         racket/performance-hint)

;; Compiled in unsafe mode, and the accessors below are inlined: they are
;; consulted on every operation in the library, and a cross-module call to
;; read one mutable variable was costing more than the operation.
(#%declare #:unsafe)

(provide print-sek
         hash-elements
         capacity-at
         max-item-weight
         max-item-weight-shift
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

(begin-encourage-inline
  (define (leaf-capacity)
    leaf-cap))
(begin-encourage-inline
  (define (node-capacity)
    node-cap))
(begin-encourage-inline
  (define (short-threshold)
    thresh))

;; capacity-at : depth -> capacity
;; A chunk that holds items of depth d has capacity (capacity-at d).  Depth 0
;; items are the sequence's own elements; depth d+1 items are chunks of depth d
;; items, so an item of depth d+1 is a chunk of capacity (capacity-at d).
(begin-encourage-inline
  (define (capacity-at d)
    (if (eqv? d 0) leaf-cap node-cap)))

;; max-item-weight : depth -> nat
;; The largest weight an item of depth d can have, i.e. the product of the
;; capacities of the levels below it.  A chunk all of whose items have this
;; weight is "packed" (§3.2) and can be indexed in O(1) by division.
;; It is consulted once per level on every indexed access, so it is tabulated
;; rather than recomputed; 32 levels is past the reach of any real sequence
;; (with the smallest legal capacity that is already 2^32 elements).
(define miw-depth 32)
(define miw-cache (make-vector miw-depth 1))
;; log2 of the same value when it is a power of two, else #f.  Indexing a
;; packed chunk is a division by this weight, and capacities are powers of two
;; often enough -- both defaults are -- that it is worth turning those
;; divisions into shifts.
(define miw-shift-cache (make-vector miw-depth #f))

(define (exact-log2 n)
  (and (positive? n) (zero? (bitwise-and n (sub1 n)))
       (let loop ([n n] [k 0]) (if (eqv? n 1) k (loop (arithmetic-shift n -1) (add1 k))))))

(define (recompute-max-item-weights!)
  (vector-set! miw-cache 0 1)
  (vector-set! miw-shift-cache 0 0)
  (for ([d (in-range 1 miw-depth)])
    (define w (* leaf-cap (expt node-cap (sub1 d))))
    (vector-set! miw-cache d w)
    (vector-set! miw-shift-cache d (exact-log2 w))))

(begin-encourage-inline
  (define (max-item-weight d)
    (if (< d miw-depth)
        (vector-ref miw-cache d)
        (* leaf-cap (expt node-cap (sub1 d))))))

(begin-encourage-inline
  (define (max-item-weight-shift d)
    (and (< d miw-depth) (vector-ref miw-shift-cache d))))

;; Should a slot that becomes logically empty be overwritten?  Leaving it
;; alone saves one write but lets the garbage collector retain a value that
;; the sequence no longer holds.  Overwriting is safer and is the default.
(define overwrite? #t)
(begin-encourage-inline
  (define (overwrite-empty-slots?) overwrite?))

;; Should the use of an invalidated iterator be detected at runtime?  This
;; costs a version-number comparison per iterator operation and a sign test
;; per update; it catches a real class of programming mistake, so it too is
;; on by default.
(define checking? #t)
(begin-encourage-inline
  (define (check-iterator-validity?) checking?))

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
  (set! checking? (and ck #t))
  (recompute-max-item-weights!))

(recompute-max-item-weights!)

;; Hashing a sequence walks its elements rather than building a list of them.
;; The mask keeps the accumulator small enough that `(fx* h 31)` cannot leave
;; fixnum range even where fixnums are 30 bits wide.
(define hash-mask #xFFFFFF)

(define (hash-elements seq n rec)
  (for/fold ([h (fxand n hash-mask)]) ([x seq])
    (fxand (fx+ (fx* h 31) (fxand (rec x) hash-mask)) hash-mask)))

;; Print as racket/treelist does: `#<pseq: 1 "a">` for write and display, and
;; the constructor form `(pseq 1 "a")` for print, so that a printed sequence
;; reads back as an expression that rebuilds it.  The struct must also carry
;; `prop:custom-print-quotable 'never`, or print mode quotes the form.
;;
;; `for-each-element` rather than a list of elements: this runs on every
;; display of a sequence, and there is no reason for it to allocate.
(define (print-sek name empty? for-each-element port mode)
  (case mode
    [(#t #f)
     (write-string "#<" port)
     (write-string name port)
     (unless empty? (write-string ":" port))]
    [else
     (write-string "(" port)
     (write-string name port)])
  (for-each-element
   (lambda (e)
     (write-string " " port)
     (case mode
       [(#t) (write e port)]
       [(#f) (display e port)]
       [else (print e port)])))
  (case mode
    [(#t #f) (write-string ">" port)]
    [else (write-string ")" port)]))
