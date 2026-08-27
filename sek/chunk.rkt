#lang racket/base
;; Transient chunks -- Charguéraud & Pottier, ICFP 2026, §3.3, Figures 11-13.
;;
;; A chunk is a pair of a *support* (a fixed-capacity circular buffer, some of
;; whose slots are occupied) and a *view* (a sub-range of the occupied region).
;; Several chunks may share one support.  Two escape hatches let us update a
;; support in place even though chunks behave persistently:
;;
;;   * a *monotonic* update fills a slot that lies outside the view of every
;;     chunk that points to the support, so no chunk can observe the change;
;;   * an *ownership* update writes into a chunk whose id matches the id of the
;;     ephemeral sequence performing the operation, which by the ownership
;;     invariant means the chunk is not shared (§2.4).
;;
;; Ownership invariant: if a chunk is uniquely owned then its view coincides
;; with the occupied range of its support, and no other chunk shares that
;; support.

(require "config.rkt")

(provide chunk-weight
         chunk-length
         chunk-capacity
         chunk-empty?
         chunk-full?
         chunk-owned?
         chunk-ref
         chunk-first
         chunk-last
         fresh-id!
         no-owner
         empty-chunk
         make-chunk
         chunk-of-list
         chunk-of-vector
         chunk-singleton
         chunk-sub
         chunk-fuse
         chunk-push-front
         chunk-push-back
         chunk-pop-front
         chunk-pop-back
         chunk-set
         chunk-item-at
         chunk-ref-atomic
         chunk-set-atomic
         unit-measure
         weight-measure
         measure-at
         chunk-well-formed?
         chunk-aligned?
         chunk-data
         chunk-segment-at
         chunk-index->support-index)

;; ---------------------------------------------------------------- ownership

;; An ownership id is a positive integer; #f means "owned by nobody", which is
;; the state of every chunk reachable from a persistent sequence.
(define no-owner #f)

(define id-counter 0)
(define (fresh-id!)
  (set! id-counter (add1 id-counter))
  id-counter)

;; ------------------------------------------------------------------- layout

;; The value written into a slot that is logically empty.  Overwriting emptied
;; slots costs one write but avoids retaining garbage (§4.1).
(define none (string->uninterned-symbol "sek-empty-slot"))

;; data : (vectorof any), of length K
;; head : index of the first occupied slot
;; size : number of occupied slots, 0 <= size <= K; the occupied region is
;;        [head, head+size) taken modulo K
(struct support ([data #:mutable] [head #:mutable] [size #:mutable]) #:authentic)

;; support : the underlying circular buffer
;; head, size : the view, which must be a sub-range of the support's range
;; weight : total number of atomic elements transitively held by the view
;; id : ownership id, or #f
(struct chunk
        ([support #:mutable] [head #:mutable] [size #:mutable] [weight #:mutable] [id #:mutable])
  #:authentic)

(define (wrap+ i k)
  (if (< i k)
      i
      (- i k)))
(define (wrap- i k)
  (if (< i 0)
      (+ i k)
      i))

(define (chunk-capacity c)
  (vector-length (support-data (chunk-support c))))
(define (chunk-length c)
  (chunk-size c))
(define (chunk-empty? c)
  (eqv? 0 (chunk-size c)))
(define (chunk-full? c)
  (= (chunk-size c) (chunk-capacity c)))

(define (chunk-owned? c owner)
  (and owner (eqv? (chunk-id c) owner)))

;; ------------------------------------------------------------------ measure

;; The weight of an item of depth d: elements weigh 1, deeper items are chunks
;; that carry their own weight.
(define (chunk-data c) (support-data (chunk-support c)))

(define (chunk-index->support-index c i)
  (define k (chunk-capacity c))
  (wrap+ (+ (chunk-head c) i) k))

;; The occupied region of a chunk wraps around at most once, so the chunk's
;; items form at most two contiguous runs of its support.  This returns the
;; run that holds item i, as the support index where the run starts, its
;; length, and the index within the chunk of its first item.
(define (chunk-segment-at c i)
  (define s (chunk-support c))
  (define k (vector-length (support-data s)))
  (define head (chunk-head c))
  (define n (chunk-size c))
  (define k1 (min n (- k head)))
  (if (< i k1)
      (values head k1 0)
      (values 0 (- n k1) k1)))

(define (chunk-well-formed? c)
  (define s (chunk-support c))
  (define k (vector-length (support-data s)))
  (cond
    [(eqv? k 0) (and (eqv? (chunk-size c) 0) (eqv? (support-size s) 0))]
    [else
     (and (<= 0 (chunk-size c) (support-size s) k)
          (< -1 (chunk-head c) k)
          (< -1 (support-head s) k)
          ;; the view must lie inside the support's occupied region
          (or (eqv? (chunk-size c) 0)
              (<= (+ (modulo (- (chunk-head c) (support-head s)) k) (chunk-size c))
                  (support-size s))))]))

;; A chunk is aligned with its support when their ranges coincide; the
;; ownership invariant requires this of every uniquely-owned chunk.
(define (chunk-aligned? c)
  (define s (chunk-support c))
  (and (eqv? (chunk-head c) (support-head s))
       (eqv? (chunk-size c) (support-size s))))

(define (unit-measure x)
  1)
(define weight-measure chunk-weight)
(define (measure-at d)
  (if (eqv? d 0) unit-measure weight-measure))

;; --------------------------------------------------------------- allocation

(define (make-chunk cap owner)
  (chunk (support (make-vector cap none) 0 0) 0 0 0 owner))

;; The shared stand-in for an empty inner chunk (§3.6).  Its capacity is zero,
;; so it is never pushed into; code replaces it wholesale.
(define empty-chunk (chunk (support (vector) 0 0) 0 0 0 #f))

(define (chunk-ref c i)
  (define s (chunk-support c))
  (define data (support-data s))
  (vector-ref data (wrap+ (+ (chunk-head c) i) (vector-length data))))

(define (chunk-first c)
  (chunk-ref c 0))
(define (chunk-last c)
  (chunk-ref c (sub1 (chunk-size c))))

;; Copy the items of c that lie in [start, start+len) into dst at position at.
;; The occupied region wraps around at most once, so this is one or two
;; vector-copy!s rather than a loop that redoes the wrap arithmetic per item.
(define (chunk-blit! c start len dst at)
  (define data (support-data (chunk-support c)))
  (define k (vector-length data))
  (define from (if (eqv? k 0) 0 (wrap+ (+ (chunk-head c) start) k)))
  (define run (min len (- k from)))
  (vector-copy! dst at data from (+ from run))
  (when (< run len)
    (vector-copy! dst (+ at run) data 0 (- len run))))

;; The total weight of those same items.  At depth 0 every item weighs one, so
;; the scan is skipped entirely.
(define (chunk-items-weight c start len mw)
  (if (eq? mw unit-measure)
      len
      (for/fold ([w 0]) ([j (in-range start (+ start len))])
        (+ w (mw (chunk-ref c j))))))

(define (chunk-of-vector v cap mw owner)
  (define n (vector-length v))
  (define data (make-vector cap none))
  (vector-copy! data 0 v 0 n)
  (define w
    (if (eq? mw unit-measure)
        n
        (for/fold ([w 0]) ([i (in-range n)]) (+ w (mw (vector-ref v i))))))
  (chunk (support data 0 n) 0 n w owner))

(define (chunk-of-list xs cap mw owner)
  (chunk-of-vector (list->vector xs) cap mw owner))

(define (chunk-singleton x w cap owner)
  (define data (make-vector cap none))
  (vector-set! data 0 x)
  (chunk (support data 0 1) 0 1 w owner))

;; The sub-chunk holding items [start, start+len) of c.  It gets a fresh
;; support: sharing one would break the ownership invariant if c happens to be
;; owned by somebody.
(define (chunk-sub c start len mw)
  (define data (make-vector (chunk-capacity c) none))
  (chunk-blit! c start len data 0)
  (chunk (support data 0 len) 0 len (chunk-items-weight c start len mw) no-owner))

;; The concatenation of two chunks, which must fit within one capacity.
(define (chunk-fuse a b owner)
  (define na (chunk-size a))
  (define nb (chunk-size b))
  (define cap (max (chunk-capacity a) (chunk-capacity b)))
  (define data (make-vector cap none))
  (chunk-blit! a 0 na data 0)
  (chunk-blit! b 0 nb data na)
  (chunk (support data 0 (+ na nb)) 0 (+ na nb) (+ (chunk-weight a) (chunk-weight b)) owner))

;; --------------------------------------------------------------------- push

;; In-place push into a uniquely-owned chunk: the view and the support's range
;; coincide, so extending both keeps them aligned.
(define (owned-push-back! c x w)
  (define s (chunk-support c))
  (define k (chunk-capacity c))
  (vector-set! (support-data s) (wrap+ (+ (chunk-head c) (chunk-size c)) k) x)
  (set-support-size! s (add1 (support-size s)))
  (set-chunk-size! c (add1 (chunk-size c)))
  (set-chunk-weight! c (+ (chunk-weight c) w))
  c)

(define (owned-push-front! c x w)
  (define s (chunk-support c))
  (define k (chunk-capacity c))
  (define i (wrap- (sub1 (chunk-head c)) k))
  (vector-set! (support-data s) i x)
  (set-support-head! s i)
  (set-support-size! s (add1 (support-size s)))
  (set-chunk-head! c i)
  (set-chunk-size! c (add1 (chunk-size c)))
  (set-chunk-weight! c (+ (chunk-weight c) w))
  c)

;; Persistent push: either a monotonic in-place update of a slot that no view
;; covers, or a copy of the view into a fresh support.
(define (persistent-push-back c x w owner)
  (define s (chunk-support c))
  (define k (chunk-capacity c))
  (define n (chunk-size c))
  (define view-back (wrap+ (+ (chunk-head c) n) k))
  (define support-back (wrap+ (+ (support-head s) (support-size s)) k))
  (cond
    [(and (< (support-size s) k) (= view-back support-back))
     (vector-set! (support-data s) support-back x)
     (set-support-size! s (add1 (support-size s)))
     (chunk s (chunk-head c) (add1 n) (+ (chunk-weight c) w) (chunk-id c))]
    [else
     (define data (make-vector k none))
     (chunk-blit! c 0 n data 0)
     (vector-set! data n x)
     (chunk (support data 0 (add1 n)) 0 (add1 n) (+ (chunk-weight c) w) owner)]))

(define (persistent-push-front c x w owner)
  (define s (chunk-support c))
  (define k (chunk-capacity c))
  (define n (chunk-size c))
  (cond
    [(and (< (support-size s) k) (= (chunk-head c) (support-head s)))
     (define i (wrap- (sub1 (support-head s)) k))
     (vector-set! (support-data s) i x)
     (set-support-head! s i)
     (set-support-size! s (add1 (support-size s)))
     (chunk s i (add1 n) (+ (chunk-weight c) w) (chunk-id c))]
    [else
     (define data (make-vector k none))
     (vector-set! data 0 x)
     (chunk-blit! c 0 n data 1)
     (chunk (support data 0 (add1 n)) 0 (add1 n) (+ (chunk-weight c) w) owner)]))

(define (chunk-push-back c x w owner)
  (if (chunk-owned? c owner)
      (owned-push-back! c x w)
      (persistent-push-back c x w owner)))

(define (chunk-push-front c x w owner)
  (if (chunk-owned? c owner)
      (owned-push-front! c x w)
      (persistent-push-front c x w owner)))

;; ---------------------------------------------------------------------- pop

;; Popping from a persistent chunk merely shrinks the view (§3.3); the support
;; is left alone, so other chunks that share it are unaffected.
(define (chunk-pop-back c mw owner)
  (define n (chunk-size c))
  (define x (chunk-ref c (sub1 n)))
  (define w (mw x))
  (cond
    [(chunk-owned? c owner)
     (define s (chunk-support c))
     (define k (chunk-capacity c))
     (when (overwrite-empty-slots?)
       (vector-set! (support-data s) (wrap+ (+ (chunk-head c) (sub1 n)) k) none))
     (set-support-size! s (sub1 (support-size s)))
     (set-chunk-size! c (sub1 n))
     (set-chunk-weight! c (- (chunk-weight c) w))
     (values x c)]
    [else
     (values x
             (chunk (chunk-support c) (chunk-head c) (sub1 n) (- (chunk-weight c) w) (chunk-id c)))]))

(define (chunk-pop-front c mw owner)
  (define x (chunk-ref c 0))
  (define w (mw x))
  (define k (chunk-capacity c))
  (cond
    [(chunk-owned? c owner)
     (define s (chunk-support c))
     (when (overwrite-empty-slots?)
       (vector-set! (support-data s) (chunk-head c) none))
     (set-support-head! s (wrap+ (add1 (chunk-head c)) k))
     (set-support-size! s (sub1 (support-size s)))
     (set-chunk-head! c (wrap+ (add1 (chunk-head c)) k))
     (set-chunk-size! c (sub1 (chunk-size c)))
     (set-chunk-weight! c (- (chunk-weight c) w))
     (values x c)]
    [else
     (values x
             (chunk (chunk-support c)
                    (wrap+ (add1 (chunk-head c)) k)
                    (sub1 (chunk-size c))
                    (- (chunk-weight c) w)
                    (chunk-id c)))]))

;; --------------------------------------------------------------------- set

;; Replace item i.  wold/wnew are the weights of the outgoing and incoming
;; items.
(define (chunk-set c i x wold wnew owner)
  (cond
    [(chunk-owned? c owner)
     (define s (chunk-support c))
     (define k (chunk-capacity c))
     (vector-set! (support-data s) (wrap+ (+ (chunk-head c) i) k) x)
     (set-chunk-weight! c (+ (chunk-weight c) (- wnew wold)))
     c]
    [else
     (define k (chunk-capacity c))
     (define n (chunk-size c))
     (define data (make-vector k none))
     (chunk-blit! c 0 n data 0)
     (vector-set! data i x)
     (chunk (support data 0 n) 0 n (+ (chunk-weight c) (- wnew wold)) owner)]))

;; -------------------------------------------------------------- get-from-chunk

;; §3.2's get-from-chunk.  c holds items of depth d; i is an atomic index into
;; the sequence that c denotes.  Returns the position of the item that holds
;; index i together with the index of that element relative to the item.
;;
;; A chunk is *packed* when all of its items have maximal weight, which is
;; detected in O(1) and lets the linear scan be replaced by a division.
(define (chunk-item-at c i d)
  (define n (chunk-size c))
  (define mw (max-item-weight d))
  (define (scan)
    (let loop ([q 0] [acc 0])
      (define w (chunk-weight (chunk-ref c q)))
      (if (< i (+ acc w))
          (values q (- i acc))
          (loop (add1 q) (+ acc w)))))
  (cond
    [(eqv? mw 1) (values i 0)]
    [else
     (define sh (max-item-weight-shift d))
     (cond
       [sh
        (if (eqv? (chunk-weight c) (arithmetic-shift n sh))
            (values (arithmetic-shift i (- sh)) (bitwise-and i (sub1 mw)))
            (scan))]
       [(eqv? (chunk-weight c) (* n mw))
        (let-values ([(q r) (quotient/remainder i mw)]) (values q r))]
       [else (scan)])]))

;; Descend from a chunk of depth d items to the atomic element at index i.
;; This lives here, beside chunk-item-at and chunk-ref, so that the compiler
;; can see through the calls; it is the inner loop of every indexed access.
(define (chunk-ref-atomic c i d)
  (if (eqv? d 0)
      (chunk-ref c i)
      (let-values ([(q j) (chunk-item-at c i d)])
        (chunk-ref-atomic (chunk-ref c q) j (sub1 d)))))

;; The same descent, rebuilding the chunks along the way.
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
