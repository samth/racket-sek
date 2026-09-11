#lang racket/base
;; First-class iterators -- the OCaml library's `Iterator` module, whose design
;; the paper omits for space (§5.4).
;;
;; The logical model is an integer index in [-1, n], where -1 and n are two
;; sentinel positions, one at each end, and the indices in [0, n) designate
;; the elements.  An iterator is created at a sentinel or at an end, and moves
;; one element at a time in either direction, or jumps to an arbitrary index.
;;
;; The point of the design is that `move` is O(1) as long as the iterator
;; stays inside one *segment*.  A cursor therefore caches the segment it is
;; sitting on -- the backing vector plus the bounds of the contiguous run of
;; slots that holds the current element -- so the common step is one integer
;; increment and a comparison.  Crossing a segment boundary, a chunk boundary,
;; or a level of the tree costs more, but happens only once every K elements.
;;
;; A cursor's position in the tree is its `path`:
;;   'sf, 'sb    -- at the front or back sentinel
;;   'front      -- inside the front chunk of this level
;;   'back       -- inside the back chunk
;;   'short      -- inside the vector of a short persistent sequence
;;   a cursor    -- inside the middle sequence, at the chunk that cursor
;;                  currently designates (one level deeper)
;;
;; Iterating an ephemeral sequence is guarded by version numbers: any update
;; to the sequence invalidates every live iterator, and using an invalidated
;; iterator raises an error rather than quietly reading stale memory.

(require (only-in racket/unsafe/ops
                  unsafe-vector*-ref unsafe-vector*-set!
                  unsafe-fx+ unsafe-fx- unsafe-fx< unsafe-fx>)
         "config.rkt"
         "chunk.rkt"
         "ptree.rkt"
         "persistent.rkt"
         "ephemeral.rkt"
         "segment.rkt")

(provide sek-iterator
         sek-iterator-at-sentinel
         sek-iter?
         sek-iter-copy
         sek-iter-reset!
         sek-iter-sequence
         sek-iter-length
         sek-iter-index
         sek-iter-finished?
         sek-iter-valid?
         sek-iter-get
         sek-iter-get*
         sek-iter-move!
         sek-iter-get-and-move!
         sek-iter-get-and-move*!
         sek-iter-jump!
         sek-iter-reach!
         sek-iter-segment
         sek-iter-segment*
         sek-iter-segment-and-jump!
         sek-iter-segment-and-jump*!
         sek-iter-set!
         sek-iter-set-and-move!
         sek-iter-writable-segment
         sek-iter-writable-segment*
         sek-iter-writable-segment-and-jump!
         sek-iter-writable-segment-and-jump*!
         sek-iter-check)

;; ---------------------------------------------------------------- cursors

;; A cursor over the items of one level of the tree.  At depth 0 the items are
;; the sequence's elements, each of weight one; deeper down they are chunks,
;; and every index is a weight index -- the number of elements to the left.
(struct cur
        (depth [wt #:mutable]
               [front #:mutable]
               [middle #:mutable]
               [back #:mutable]
               [sv #:mutable] ; the vector of a short sequence, or #f
               [path #:mutable]
               [chunk #:mutable]
               [support #:mutable]
               [icur #:mutable]
               [ishd #:mutable]
               [istl #:mutable]
               [ishd-sch #:mutable]
               ;; at depth 0, the weight index of the head of the current
               ;; segment; deeper down, the weight index of the current item
               [w #:mutable]
               ;; the weight index of the first item of the current chunk,
               ;; which is what lets a short hop skip the descent
               [wbase #:mutable])
  #:authentic)

(define empty-support (vector))

(define (cur-finished? c)
  (define p (cur-path c))
  (or (eq? p 'sf) (eq? p 'sb)))

;; The weight index of the current position.
(define (cur-index c)
  (if (eqv? (cur-depth c) 0)
      (unsafe-fx+ (cur-w c) (unsafe-fx- (cur-icur c) (cur-ishd c)))
      (cur-w c)))

;; icur always indexes inside the current segment, which is a range of the
;; support vector, so the bound check is redundant here.
(define (cur-get c)
  (unsafe-vector*-ref (cur-support c) (cur-icur c)))

(define (cur-item-weight c)
  (if (eqv? (cur-depth c) 0)
      1
      (chunk-weight (cur-get c))))

;; ------------------------------------------------------------ construction

(define (blank-cur depth wt front middle back sv)
  (cur depth wt front middle back sv 'sf #f empty-support 0 0 0 0 -1 0))

(define (cur-of-tree t depth)
  (if t
      (blank-cur depth (lvl-weight t) (lvl-front t) (lvl-middle t) (lvl-back t) #f)
      (blank-cur depth 0 #f #f #f #f)))

(define (cur-of-level f m b depth wt)
  (blank-cur depth wt f m b #f))

(define (cur-of-vector v)
  (blank-cur 0 (vector-length v) #f #f #f v))

(define (cur-copy c)
  (define p (cur-path c))
  (cur (cur-depth c)
       (cur-wt c)
       (cur-front c)
       (cur-middle c)
       (cur-back c)
       (cur-sv c)
       (if (cur? p)
           (cur-copy p)
           p)
       (cur-chunk c)
       (cur-support c)
       (cur-icur c)
       (cur-ishd c)
       (cur-istl c)
       (cur-ishd-sch c)
       (cur-w c)
       (cur-wbase c)))

;; ------------------------------------------------------------- positioning

(define (goto-sentinel! c which)
  (set-cur-path! c which)
  (set-cur-chunk! c #f)
  (set-cur-support! c empty-support)
  (set-cur-icur! c 0)
  (set-cur-ishd! c 0)
  (set-cur-istl! c 0)
  (set-cur-ishd-sch! c 0)
  (set-cur-w! c
              (if (eq? which 'sf)
                  -1
                  (cur-wt c))))

;; Install the segment of the current chunk that covers item i; wcur is the
;; weight index of that item within the level.
(define (install-in-chunk! c i wcur)
  (define ch (cur-chunk c))
  (define-values (start len shd) (chunk-segment-at ch i))
  (set-cur-support! c (chunk-data ch))
  (set-cur-ishd! c start)
  (set-cur-istl! c (+ start len))
  (set-cur-icur! c (+ start (- i shd)))
  (set-cur-ishd-sch! c shd)
  (set-cur-w! c
              (if (eqv? (cur-depth c) 0)
                  (- wcur (- i shd))
                  wcur)))

;; base is the weight index of the chunk's first item within this level.
(define (install-chunk! c path ch i wcur base)
  (set-cur-path! c path)
  (set-cur-chunk! c ch)
  (set-cur-wbase! c base)
  (install-in-chunk! c i wcur))

;; The index of the current item within its chunk.
(define (icur-sch c)
  (unsafe-fx+ (cur-ishd-sch c) (unsafe-fx- (cur-icur c) (cur-ishd c))))

;; The weight of the front chunk and of the middle sequence, which together
;; give the weight index at which each of the three regions of a level starts.
(define (wf-of c)
  (let ([f (cur-front c)])
    (if f
        (chunk-weight f)
        0)))
(define (wm-of c)
  (pt-weight (cur-middle c)))

;; ------------------------------------------------------------------- move

(define (cur-move! c dir)
  (cond
    [(eq? dir 'forward)
     (define i (unsafe-fx+ (cur-icur c) 1))
     (cond
       [(unsafe-fx< i (cur-istl c))
        (unless (eqv? (cur-depth c) 0)
          (set-cur-w! c (unsafe-fx+ (cur-w c) (cur-item-weight c))))
        (set-cur-icur! c i)]
       [else (move-next-segment! c dir)])]
    [else
     (cond
       [(unsafe-fx> (cur-icur c) (cur-ishd c))
        (set-cur-icur! c (unsafe-fx- (cur-icur c) 1))
        (unless (eqv? (cur-depth c) 0)
          (set-cur-w! c (unsafe-fx- (cur-w c) (cur-item-weight c))))]
       [else (move-next-segment! c dir)])]))

;; A chunk holds at most two segments, because its occupied region wraps
;; around at most once; if the other one is where we are headed, stay put.
(define (move-next-segment! c dir)
  (define ch (cur-chunk c))
  (cond
    [(not ch) (move-next-chunk! c dir)]
    [else
     (define i
       (if (eq? dir 'forward)
           (add1 (icur-sch c))
           (sub1 (icur-sch c))))
     (cond
       [(and (>= i 0) (< i (chunk-length ch)))
        ;; a genuine one-item step, so the weight index moves by one item
        (define w
          (if (eq? dir 'forward)
              (+ (cur-index c) (cur-item-weight c))
              (- (cur-index c)
                 (if (eqv? (cur-depth c) 0)
                     1
                     (chunk-weight (chunk-ref ch i))))))
        (install-in-chunk! c i w)]
       [else (move-next-chunk! c dir)])]))

(define (finish! c dir)
  (goto-sentinel! c (if (eq? dir 'forward) 'sb 'sf)))

;; Focus the first item of ch (going forward) or its last item (going
;; backward).  `base` is the weight index of ch's first item within the level;
;; the caller knows it because it knows which region ch belongs to.
(define (enter-chunk! c dir path ch base)
  (define n (chunk-length ch))
  (define i
    (if (eq? dir 'forward)
        0
        (sub1 n)))
  (define w
    (if (eq? dir 'forward)
        base
        (- (+ base (chunk-weight ch))
           (if (eqv? (cur-depth c) 0)
               1
               (chunk-weight (chunk-ref ch i))))))
  (install-chunk! c path ch i w base))

;; Enter the middle sequence from one end, or continue with an existing
;; cursor into it.
(define (enter-middle! c dir mi)
  (enter-chunk! c dir mi (cur-get mi) (+ (wf-of c) (cur-index mi))))

(define (fresh-middle-cursor c dir)
  (define mi (cur-of-tree (cur-middle c) (add1 (cur-depth c))))
  (goto-sentinel! mi (if (eq? dir 'forward) 'sf 'sb))
  (cur-move! mi dir)
  mi)

;; Enter the first nonempty region at or after position k in the traversal
;; order, where the order is front-middle-back going forward and the reverse
;; going backward.  Skipping empty regions explicitly means the cursor does
;; not depend on invariant 1 holding at the root, which it need not for an
;; ephemeral sequence.
(define (enter-region! c dir k)
  (define fwd? (eq? dir 'forward))
  (let try ([k k])
    (cond
      [(> k 2) (finish! c dir)]
      [else
       (define r
         (if fwd?
             k
             (- 2 k)))
       (define f (cur-front c))
       (define b (cur-back c))
       (cond
         [(and (eqv? r 0) f (not (chunk-empty? f))) (enter-chunk! c dir 'front f 0)]
         [(and (eqv? r 1) (cur-middle c)) (enter-middle! c dir (fresh-middle-cursor c dir))]
         [(and (eqv? r 2) b (not (chunk-empty? b)))
          (enter-chunk! c dir 'back b (+ (wf-of c) (wm-of c)))]
         [else (try (add1 k))])])))

(define (enter-short! c dir)
  (define v (cur-sv c))
  (cond
    [(eqv? (vector-length v) 0) (finish! c dir)]
    [else
     (set-cur-path! c 'short)
     (set-cur-chunk! c #f)
     (set-cur-support! c v)
     (set-cur-ishd! c 0)
     (set-cur-istl! c (vector-length v))
     (set-cur-icur! c
                    (if (eq? dir 'forward)
                        0
                        (sub1 (vector-length v))))
     (set-cur-ishd-sch! c 0)
     (set-cur-w! c 0)]))

(define (move-next-chunk! c dir)
  (define p (cur-path c))
  (cond
    [(eq? p 'short) (finish! c dir)]
    [(or (eq? p 'sf) (eq? p 'sb))
     (unless (eq? (eq? p 'sf) (eq? dir 'forward))
       (raise-arguments-error 'sek-iter-move! "cannot move past the sentinel"))
     (if (cur-sv c)
         (enter-short! c dir)
         (enter-region! c dir 0))]
    [(or (eq? p 'front) (eq? p 'back))
     ;; leaving a side chunk inwards continues with the middle; leaving it
     ;; outwards runs off the end of the sequence
     (if (if (eq? dir 'forward)
             (eq? p 'front)
             (eq? p 'back))
         (enter-region! c dir 1)
         (finish! c dir))]
    [else
     ;; already inside the middle sequence: step the deeper cursor
     (cur-move! p dir)
     (if (cur-finished? p)
         (enter-region! c dir 2)
         (enter-middle! c dir p))]))

;; ------------------------------------------------------------ random access

(define (cur-reach! c target)
  (cond
    [(< target 0) (goto-sentinel! c 'sf)]
    [(>= target (cur-wt c)) (goto-sentinel! c 'sb)]
    [(cur-sv c)
     (define v (cur-sv c))
     (set-cur-path! c 'short)
     (set-cur-chunk! c #f)
     (set-cur-support! c v)
     (set-cur-ishd! c 0)
     (set-cur-istl! c (vector-length v))
     (set-cur-icur! c target)
     (set-cur-ishd-sch! c 0)
     (set-cur-w! c 0)]
    ;; Still inside the run of storage the cursor is on: one write.  At depth
    ;; 0 the items weigh one each, so the offset within the segment is just
    ;; the difference of the weight indices.
    [(and (eqv? (cur-depth c) 0)
          (let ([off (- target (cur-w c))])
            (and (>= off 0)
                 (< off (- (cur-istl c) (cur-ishd c)))
                 (begin (set-cur-icur! c (+ (cur-ishd c) off)) #t))))
     (void)]
    [else
     ;; Still inside the chunk the cursor is on: move within it instead of
     ;; descending from the root.  A hop shorter than a chunk stays put nearly
     ;; every time, which is the common case for a scan.
     (define ch (cur-chunk c))
     (define base (cur-wbase c))
     (cond
       [(and ch (>= target base) (< target (+ base (chunk-weight ch))))
        (define off (- target base))
        ;; where the cursor already sits inside this chunk, so that an
        ;; unpacked chunk is scanned from there rather than from its start
        (define cur-off (- (cur-index c) base))
        (define-values (q j)
          (if (>= off cur-off)
              (chunk-item-at/from ch off (cur-depth c) (icur-sch c) cur-off)
              (chunk-item-at ch off (cur-depth c))))
        (install-in-chunk! c q (- target j))]
       [else (reach-inside! c target)])]))

;; Descend to the item that covers weight index `target`, exactly as `get`
;; does on the tree itself.
(define (reach-inside! c target)
  (define d (cur-depth c))
  (define f (cur-front c))
  (define m (cur-middle c))
  (define b (cur-back c))
  (define wf
    (if f
        (chunk-weight f)
        0))
  (define wm (pt-weight m))
  (cond
    [(< target wf)
     (define-values (q j) (chunk-item-at f target d))
     (install-chunk! c 'front f q (- target j) 0)]
    [(>= target (+ wf wm))
     (define t2 (- target wf wm))
     (define-values (q j) (chunk-item-at b t2 d))
     (install-chunk! c 'back b q (- target j) (+ wf wm))]
    [else
     (define p (cur-path c))
     (define mi
       (if (cur? p)
           p
           (cur-of-tree m (add1 d))))
     (define t2 (- target wf))
     (cur-reach! mi t2)
     (define ch (cur-get mi))
     (define j (- t2 (cur-index mi)))
     (define-values (q j2) (chunk-item-at ch j d))
     (install-chunk! c mi ch q (- target j2) (+ wf (cur-index mi)))]))

(define (cur-jump! c dir n)
  (unless (eqv? n 0)
    (define delta
      (if (eq? dir 'forward)
          n
          (- n)))
    (define i (+ (cur-icur c) delta))
    (if (and (not (cur-finished? c)) (>= i (cur-ishd c)) (< i (cur-istl c)))
        (set-cur-icur! c i)
        (cur-reach! c (+ (cur-index c) delta)))))

;; The run of slots from the current position to the end of the segment, in
;; the given direction.
(define (cur-segment c dir)
  (if (eq? dir 'forward)
      (segment (cur-support c) (cur-icur c) (- (cur-istl c) (cur-icur c)))
      (segment (cur-support c) (cur-ishd c) (add1 (- (cur-icur c) (cur-ishd c))))))

;; ------------------------------------------------------- the public iterator

(struct siter (seq kind [birth #:mutable] cursor)
  #:authentic
  #:reflection-name 'sek-iterator
  #:methods gen:custom-write
  [(define (write-proc it port mode)
     (write-string
      (format "#<sek-iterator:~a/~a>" (cur-index (siter-cursor it)) (cur-wt (siter-cursor it)))
      port))])

(define sek-iter? siter?)

(define (make-cursor s)
  (cond
    [(pseq? s)
     (define r (pseq-rep s))
     (cond
       [(not r) (blank-cur 0 0 #f #f #f #f)]
       [(vector? r) (cur-of-vector r)]
       [else (cur-of-tree r 0)])]
    [else (cur-of-level (eseq-front s) (eseq-middle s) (eseq-back s) 0 (eseq-length s))]))

;; Refresh a cursor from its sequence, discarding any position.
(define (reload! it)
  (define s (siter-seq it))
  (define c (siter-cursor it))
  (define fresh (make-cursor s))
  (set-cur-wt! c (cur-wt fresh))
  (set-cur-front! c (cur-front fresh))
  (set-cur-middle! c (cur-middle fresh))
  (set-cur-back! c (cur-back fresh))
  (set-cur-sv! c (cur-sv fresh))
  (goto-sentinel! c 'sf))

(define (sek-iterator-at-sentinel s [side 'front])
  (unless (memq side '(front back))
    (raise-argument-error 'sek-iterator-at-sentinel "(or/c 'front 'back)" side))
  (cond
    [(pseq? s)
     (siter s
            'p
            #f
            (let ([c (make-cursor s)])
              (goto-sentinel! c (if (eq? side 'front) 'sf 'sb))
              c))]
    [(eseq? s)
     (define birth (eseq-iterator-born! s))
     (siter s
            'e
            birth
            (let ([c (make-cursor s)])
              (goto-sentinel! c (if (eq? side 'front) 'sf 'sb))
              c))]
    [else (raise-argument-error 'sek-iterator-at-sentinel "(or/c pseq? eseq?)" s)]))

;; An iterator positioned on the first element (dir 'forward) or the last
;; element (dir 'backward).  On an empty sequence it starts out finished.
(define (sek-iterator s [dir 'forward])
  (unless (memq dir '(forward backward))
    (raise-argument-error 'sek-iterator "(or/c 'forward 'backward)" dir))
  (define it (sek-iterator-at-sentinel s (if (eq? dir 'forward) 'front 'back)))
  (cur-move! (siter-cursor it) dir)
  it)

(define (sek-iter-valid? it)
  (or (eq? (siter-kind it) 'p) (eseq-iterator-valid? (siter-seq it) (siter-birth it))))

(define (check-valid! it who)
  (unless (sek-iter-valid? it)
    (raise-arguments-error who "iterator was invalidated by an update to its sequence")))

(define (sek-iter-sequence it)
  (siter-seq it))
(define (sek-iter-length it)
  (cur-wt (siter-cursor it)))

(define (sek-iter-index it)
  (check-valid! it 'sek-iter-index)
  (cur-index (siter-cursor it)))

(define (sek-iter-finished? it)
  (check-valid! it 'sek-iter-finished?)
  (cur-finished? (siter-cursor it)))

(define (sek-iter-copy it)
  (check-valid! it 'sek-iter-copy)
  (siter (siter-seq it) (siter-kind it) (siter-birth it) (cur-copy (siter-cursor it))))

;; Put the iterator back where a freshly created one would be.  For an
;; ephemeral sequence this also makes an invalidated iterator usable again.
(define (sek-iter-reset! it [dir 'forward])
  (when (eq? (siter-kind it) 'e)
    (set-siter-birth! it (eseq-iterator-born! (siter-seq it))))
  (reload! it)
  (unless (eq? dir 'sentinel)
    (goto-sentinel! (siter-cursor it) (if (eq? dir 'forward) 'sf 'sb))
    (cur-move! (siter-cursor it) dir))
  (void))

(define (sek-iter-get it)
  (check-valid! it 'sek-iter-get)
  (define c (siter-cursor it))
  (when (cur-finished? c)
    (raise-arguments-error 'sek-iter-get "iterator is at a sentinel"))
  (cur-get c))

;; Like sek-iter-get but returns #f at a sentinel instead of raising.
(define (sek-iter-get* it)
  (check-valid! it 'sek-iter-get*)
  (define c (siter-cursor it))
  (and (not (cur-finished? c)) (cur-get c)))

(define (sek-iter-move! it [dir 'forward])
  (check-valid! it 'sek-iter-move!)
  (cur-move! (siter-cursor it) dir))

(define (sek-iter-get-and-move! it [dir 'forward])
  (define x (sek-iter-get it))
  (cur-move! (siter-cursor it) dir)
  x)

(define (sek-iter-get-and-move*! it [dir 'forward])
  (check-valid! it 'sek-iter-get-and-move*!)
  (define c (siter-cursor it))
  (cond
    [(cur-finished? c) #f]
    [else
     (define x (cur-get c))
     (cur-move! c dir)
     x]))

(define (sek-iter-jump! it dir n)
  (check-valid! it 'sek-iter-jump!)
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'sek-iter-jump! "exact-nonnegative-integer?" n))
  (define c (siter-cursor it))
  (define target
    (+ (cur-index c)
       (if (eq? dir 'forward)
           n
           (- n))))
  (unless (<= -1 target (cur-wt c))
    (raise-arguments-error 'sek-iter-jump!
                           "jump lands outside the sequence"
                           "target"
                           target
                           "length"
                           (cur-wt c)))
  (cur-jump! c dir n))

(define (sek-iter-reach! it i)
  (check-valid! it 'sek-iter-reach!)
  (define c (siter-cursor it))
  (unless (and (exact-integer? i) (<= -1 i (cur-wt c)))
    (raise-arguments-error 'sek-iter-reach!
                           "index outside the sequence"
                           "index"
                           i
                           "length"
                           (cur-wt c)))
  (cur-reach! c i))

;; The elements from the current position to the end of the underlying run,
;; as a segment.  Reading past the segment means moving the iterator; the
;; segment is a view into the sequence and must not outlive it.
(define (sek-iter-segment it [dir 'forward])
  (check-valid! it 'sek-iter-segment)
  (define c (siter-cursor it))
  (when (cur-finished? c)
    (raise-arguments-error 'sek-iter-segment "iterator is at a sentinel"))
  (cur-segment c dir))

(define (sek-iter-segment-and-jump! it [dir 'forward])
  (define s (sek-iter-segment it dir))
  (cur-jump! (siter-cursor it) dir (segment-length s))
  s)

;; The same two, returning #f at a sentinel rather than raising -- which is
;; what a traversal loop wants, since reaching a sentinel is how it ends.
(define (sek-iter-segment* it [dir 'forward])
  (check-valid! it 'sek-iter-segment*)
  (define c (siter-cursor it))
  (and (not (cur-finished? c)) (cur-segment c dir)))

(define (sek-iter-segment-and-jump*! it [dir 'forward])
  (define s (sek-iter-segment* it dir))
  (when s (cur-jump! (siter-cursor it) dir (segment-length s)))
  s)

;; ---------------------------------------------------------------- writing

;; Writing through an iterator requires the chunk under the cursor to be
;; uniquely owned by the sequence.  When it is, the write is a single vector
;; store.  When it is not, we go through the sequence's own `set`, which
;; copies the path and leaves the chunk owned, and then rebuild the cursor --
;; so the first write to a shared chunk is expensive and the rest are not.
(define (ensure-owned! it who)
  (define e (siter-seq it))
  (define c (siter-cursor it))
  (cond
    [(chunk-owned? (cur-chunk c) (eseq-id e))
     (set-siter-birth! it (eseq-invalidate-iterators-except! e))]
    [else
     (define i (cur-index c))
     (eseq-own-at! e i)
     (set-siter-birth! it (eseq-iterator-born! e))
     (reload! it)
     (cur-reach! c i)]))

(define (check-writable it who)
  (check-valid! it who)
  (unless (eq? (siter-kind it) 'e)
    (raise-arguments-error who "iterator is on a persistent sequence"))
  (when (cur-finished? (siter-cursor it))
    (raise-arguments-error who "iterator is at a sentinel")))

(define (sek-iter-set! it x)
  (check-writable it 'sek-iter-set!)
  (ensure-owned! it 'sek-iter-set!)
  (define c (siter-cursor it))
  (unsafe-vector*-set! (cur-support c) (cur-icur c) x))

(define (sek-iter-set-and-move! it x [dir 'forward])
  (sek-iter-set! it x)
  (cur-move! (siter-cursor it) dir))

(define (sek-iter-writable-segment it [dir 'forward])
  (check-writable it 'sek-iter-writable-segment)
  (ensure-owned! it 'sek-iter-writable-segment)
  (cur-segment (siter-cursor it) dir))

(define (sek-iter-writable-segment* it [dir 'forward])
  (check-valid! it 'sek-iter-writable-segment*)
  (and (not (cur-finished? (siter-cursor it)))
       (sek-iter-writable-segment it dir)))

(define (sek-iter-writable-segment-and-jump! it [dir 'forward])
  (define s (sek-iter-writable-segment it dir))
  (cur-jump! (siter-cursor it) dir (segment-length s))
  s)

(define (sek-iter-writable-segment-and-jump*! it [dir 'forward])
  (define s (sek-iter-writable-segment* it dir))
  (when s (cur-jump! (siter-cursor it) dir (segment-length s)))
  s)

;; ------------------------------------------------------------- validation

;; Check that the cursor's cached segment really describes the position it
;; claims, at every level of the path.
(define (sek-iter-check it)
  (check-valid! it 'sek-iter-check)
  (let loop ([c (siter-cursor it)])
    (define p (cur-path c))
    (cond
      [(or (eq? p 'sf) (eq? p 'sb))
       (unless (and (eqv? (cur-icur c) 0)
                    (eqv? (cur-ishd c) 0)
                    (eqv? (cur-istl c) 0)
                    (not (cur-chunk c)))
         (raise (exn:fail:sek "sek iterator: malformed sentinel position"
                              (current-continuation-marks))))]
      [else
       (unless (and (<= (cur-ishd c) (cur-icur c)) (< (cur-icur c) (cur-istl c)))
         (raise (exn:fail:sek "sek iterator: current index outside its segment"
                              (current-continuation-marks))))
       (unless (<= (cur-istl c) (vector-length (cur-support c)))
         (raise (exn:fail:sek "sek iterator: segment outside its support"
                              (current-continuation-marks))))
       (when (cur-chunk c)
         (unless (eq? (cur-support c) (chunk-data (cur-chunk c)))
           (raise (exn:fail:sek "sek iterator: segment is not part of its chunk"
                                (current-continuation-marks))))
         (unless (< (icur-sch c) (chunk-length (cur-chunk c)))
           (raise (exn:fail:sek "sek iterator: index outside its chunk"
                                (current-continuation-marks)))))
       (when (cur? p)
         (loop p))]))
  it)
