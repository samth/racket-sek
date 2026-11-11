#lang racket/base
;; Tunable settings for the Sek data structure.
;;
;; Charguéraud & Pottier, "A Catenable, Splittable, Transient Sequence Data
;; Structure", ICFP 2026, §4.1: the chunk capacity may depend on the depth at
;; which the chunk appears.  Their defaults are 128 at the leaves and 16 at
;; internal nodes, with a threshold of 32 for the array representation of short
;; persistent sequences (§3.5).

(require "unchecked.rkt"
         racket/performance-hint)

;; The accessors below are inlined: they are consulted on every operation in
;; the library, and a cross-module call to read one mutable variable was
;; costing more than the operation.  Once inlined, the variables cost nothing
;; measurable: making them constants moved the geometric mean of the
;; benchmark suite by 0.3%, well inside its noise.  So the settings stay
;; mutable, and the tests use small capacities to reach deep trees.

(provide print-sek
         hash-elements
         capacity-at
         capacity-bits
         packed-item-weight
         packed-item-shift
         leaf-capacity
         node-capacity
         short-threshold
         overwrite-empty-slots?
         check-iterator-validity?
         max-length
         check-append-length
         check-procedure
         check-direction
         check-side
         sek-sequencer
         sek-all2?
         set-sek-traversals!
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

;; packed-item-weight : depth -> (or/c fixnum #f)
;; The largest weight an item of depth d can have is the product of the
;; capacities of the levels below it.  A chunk all of whose items have that
;; weight is "packed" (§3.2) and can be indexed in O(1) by division.  It is
;; consulted once per level on every indexed access, so it is tabulated.
;;
;; The division and the test for packing are fixnum operations, which are
;; unchecked, so the table holds the weight only where a full chunk of such
;; items weighs a fixnum; elsewhere it holds #f, and the lookup scans.  The
;; weight itself is no bound on a sequence: sharing builds a sequence whose
;; length is a fixnum and whose maximum item weights, at small capacities, are
;; not.  A full chunk at depth d weighs at least 2^(d+1), so beyond the table
;; no depth qualifies, whatever the capacities.
(define miw-depth 62)
(define miw-cache (make-vector miw-depth #f))
;; log2 of the same weight when it is a power of two, else #f.  Capacities are
;; powers of two often enough -- both defaults are -- that it is worth turning
;; those divisions into shifts.
(define miw-shift-cache (make-vector miw-depth #f))

(define (exact-log2 n)
  (and (positive? n) (zero? (bitwise-and n (sub1 n)))
       (let loop ([n n] [k 0]) (if (eqv? n 1) k (loop (arithmetic-shift n -1) (add1 k))))))

(define (recompute-max-item-weights!)
  (for ([d (in-range miw-depth)])
    (define w (if (eqv? d 0) 1 (* leaf-cap (expt node-cap (sub1 d)))))
    (define ok? (fixnum? (* (capacity-at d) w)))
    (vector-set! miw-cache d (and ok? w))
    (vector-set! miw-shift-cache d (and ok? (exact-log2 w)))))

(begin-encourage-inline
  (define (packed-item-weight d)
    (and (< d miw-depth) (vector-ref miw-cache d))))

(begin-encourage-inline
  (define (packed-item-shift d)
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

;; A support keeps its head and its size in one fixnum (see chunk.rkt), each in
;; `capacity-bits` bits, so a capacity must be below 2^capacity-bits.  That is
;; 2^30 where fixnums reach 2^60 - 1, and 2^14 where they are 30 bits wide.
(define capacity-bits (if (fixnum? (sub1 (expt 2 60))) 30 14))
(define max-capacity (sub1 (expt 2 capacity-bits)))

;; The longest sequence a concatenation may build: the largest power of two
;; that is a fixnum.  Sharing makes such lengths cheap -- a sequence appended
;; to itself doubles at the cost of one concatenation -- so without a limit a
;; length could leave fixnum range, and lengths and weights are added with
;; unchecked fixnum operations.  Pushes add one element at a time, and no
;; program pushes enough to carry a length from here past the fixnum range.
(define max-length
  (let loop ([n 1]) (if (fixnum? (* 2 n)) (loop (* 2 n)) n)))

(define (check-append-length who n1 n2)
  (unless (<= (+ n1 n2) max-length)
    (raise-arguments-error who "resulting sequence would be too long"
                           "first length" n1
                           "second length" n2
                           "maximum length" max-length)))

;; Checks of the arguments besides the sequence, shared by every module with
;; an entry point.  A callback is checked before the traversal, as
;; racket/treelist checks one, so that a wrong one is reported even when the
;; sequence is empty.
(define (check-procedure who p arity)
  (unless (and (procedure? p) (procedure-arity-includes? p arity))
    (raise-argument-error who (format "(procedure-arity-includes/c ~a)" arity) p)))

(define (check-direction who d)
  (unless (or (eq? d 'forward) (eq? d 'backward))
    (raise-argument-error who "(or/c 'forward 'backward)" d)))

(define (check-side who side)
  (unless (or (eq? side 'front) (eq? side 'back))
    (raise-argument-error who "(or/c 'front 'back)" side)))

;; How a sequence is traversed when it is used directly as a Racket sequence,
;; and by `equal?` and hashing.  generic.rkt installs the traversals by runs
;; that `in-sek` and the folds use; the modules that define the sequences load
;; before it and cannot name them.  Until they are installed those modules
;; fall back to the reader in iterate.rkt, which only code that loads them
;; without generic.rkt sees.
;;
;;   sequencer : who seq -> sequence?       the elements as a Racket sequence
;;   all2?     : who seq seq proc -> any    whether proc holds of each pair
(define sequencer #f)
(define all2? #f)
(define (sek-sequencer) sequencer)
(define (sek-all2?) all2?)
(define (set-sek-traversals! s a)
  (set! sequencer s)
  (set! all2? a))

;; Settings must be chosen before any sequence is built; mixing capacities
;; within one structure breaks the density invariant checks.
(define (sek-configure! #:leaf-capacity [k0 leaf-cap]
                        #:node-capacity [k1 node-cap]
                        #:short-threshold [t thresh]
                        #:overwrite-empty-slots? [ow overwrite?]
                        #:check-iterator-validity? [ck checking?])
  (unless (and (exact-integer? k0) (<= 2 k0 max-capacity))
    (raise-argument-error 'sek-configure! (format "(integer-in 2 ~a)" max-capacity) k0))
  (unless (and (exact-integer? k1) (<= 2 k1 max-capacity))
    (raise-argument-error 'sek-configure! (format "(integer-in 2 ~a)" max-capacity) k1))
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

;; Print as racket/treelist does: `#<psek: 1 "a">` for write and display, and
;; the constructor form `(psek 1 "a")` for print, so that a printed sequence
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
