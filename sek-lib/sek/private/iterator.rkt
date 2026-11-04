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
;;   'ifront     -- inside the inner front chunk (the top level of an
;;                  ephemeral sequence only)
;;   'iback      -- inside the inner back chunk (likewise)
;;   'back       -- inside the back chunk
;;   'short      -- inside the vector of a short persistent sequence
;;   a cursor    -- inside the middle sequence, at the chunk that cursor
;;                  currently designates (one level deeper)
;;
;; A level is thus up to five regions, in order: front, inner front, middle,
;; inner back, back.  The cursor walks an ephemeral sequence's inner chunks
;; where they are rather than first moving them into the middle sequence, so
;; that creating an iterator does not change the sequence -- several threads
;; may then iterate over one ephemeral sequence at once.
;;
;; Iterating an ephemeral sequence is guarded by version numbers: any update
;; to the sequence invalidates every live iterator, and using an invalidated
;; iterator raises an error rather than quietly reading stale memory.

(require "unchecked.rkt"
         (only-in "unchecked.rkt"
                  unsafe-vector*-ref unsafe-vector*-set!
                  unsafe-fx+ unsafe-fx- unsafe-fx< unsafe-fx>)
         "config.rkt"
         "chunk.rkt"
         "ptree.rkt"
         "persistent.rkt"
         "ephemeral.rkt"
         "segment.rkt")

;; Every function here that a caller outside the library can reach checks its
;; arguments explicitly, with `unless`: the accessors of the library's own
;; structures do not check (see unchecked.rkt), so they would not raise on a
;; value of the wrong kind.

(provide sek-iterator
         sek-iterator-at
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
(struct/unchecked cur
        (depth [wt #:mutable]
               [front #:mutable]
               [ifront #:mutable] ; #f below the top level
               [middle #:mutable]
               [iback #:mutable] ; #f below the top level
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
  #:authentic #:sealed)


;; Argument checking is explicit here, because the structure accessors do not
;; check (see unchecked.rkt): on the wrong kind of value they would read
;; whatever happens to be at that offset.
(define-syntax-rule (check-iter who v)
  (unless (siter? v) (raise-argument-error who "sek-iter?" v)))
(define-syntax-rule (check-sek who v)
  (unless (or (psek? v) (esek? v)) (raise-argument-error who "sek?" v)))

(define empty-support (vector))

(define (cur-finished? c)
  (define p (cur-path c))
  (or (eq? p 'sf) (eq? p 'sb)))

;; The weight index of the current position.
(define (cur-index c)
  (if (eqv? (cur-depth c) 0)
      (unsafe-fx+ (cur-w c) (unsafe-fx- (cur-icur c) (cur-ishd c)))
      (cur-w c)))

;; A cursor keeps ishd <= icur < istl <= the length of its support, except at
;; a sentinel, where the support is empty and icur = istl = 0.  Correct code
;; never reads at a sentinel, but a data race can: an ephemeral sequence
;; updated during iteration can leave an empty chunk in a middle sequence, or
;; weights that send a descent past the end, and a cursor that expects to land
;; on an item lands on a sentinel instead.  And two threads that use one
;; iterator at once, which is not allowed, can each find the cursor's fields
;; half rewritten by the other: icur from one run and support from another.
;; So the test is against the vector actually read.
(define (cur-get c)
  (define v (cur-support c))
  (define i (cur-icur c))
  (if (and (unsafe-fx>= i 0) (unsafe-fx< i (unsafe-vector*-length v)))
      (unsafe-vector*-ref v i)
      (inconsistent-structure)))

(define (cur-item-weight c)
  (if (eqv? (cur-depth c) 0)
      1
      (chunk-weight (cur-get c))))

;; ------------------------------------------------------------ construction

(define (blank-cur depth wt front ifront middle iback back sv)
  (cur depth wt front ifront middle iback back sv 'sf #f empty-support 0 0 0 0 -1 0))

(define (cur-of-tree t depth)
  (if t
      (blank-cur depth (lvl-weight t) (lvl-front t) #f (lvl-middle t) #f (lvl-back t) #f)
      (blank-cur depth 0 #f #f #f #f #f #f)))

(define (cur-of-level f fi m bi b depth wt)
  (blank-cur depth wt f fi m bi b #f))

(define (cur-of-vector v)
  (blank-cur 0 (vector-length v) #f #f #f #f #f v))

(define (cur-copy c)
  (define p (cur-path c))
  (cur (cur-depth c)
       (cur-wt c)
       (cur-front c)
       (cur-ifront c)
       (cur-middle c)
       (cur-iback c)
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

;; Install the segment of chunk ch, the current chunk, that covers item i; wcur
;; is the weight index of that item within the level.  The caller passes the
;; chunk rather than this reading it back, since another thread using the same
;; iterator could have replaced it, perhaps with #f.
(define (install-in-chunk! c ch i wcur)
  (define-values (start len shd) (chunk-segment-at ch i))
  ;; The loops that walk the run read it unchecked, so the run must lie inside
  ;; the vector; and a traversal by segments advances by the length of each,
  ;; so the run must hold the item.  While the invariants hold both are true;
  ;; a data race on an ephemeral sequence can break them.
  (let ([offset (- i shd)])
    (unless (and (<= 0 start) (<= 0 offset) (< offset len)
                 (<= (+ start len) (vector-length (chunk-data ch))))
      (inconsistent-structure)))
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
  (install-in-chunk! c ch i wcur))

;; The index of the current item within its chunk.
(define (icur-sch c)
  (unsafe-fx+ (cur-ishd-sch c) (unsafe-fx- (cur-icur c) (cur-ishd c))))

;; The regions of a level, numbered in forward order: 0 front, 1 inner front,
;; 2 middle, 3 inner back, 4 back.  A missing chunk weighs nothing.
(define (weight-of ch)
  (if ch
      (chunk-weight ch)
      0))

;; The weight index at which region r starts.
(define (region-base c r)
  (define b1 (weight-of (cur-front c)))
  (define b2 (+ b1 (weight-of (cur-ifront c))))
  (case r
    [(0) 0]
    [(1) b1]
    [(2) b2]
    [else
     (define b3 (+ b2 (pt-weight (cur-middle c))))
     (if (eqv? r 3)
         b3
         (+ b3 (weight-of (cur-iback c))))]))

(define (region-of-path p)
  (case p
    [(front) 0]
    [(ifront) 1]
    [(iback) 3]
    [else 4]))

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
     ;; icur is read once, so what is written back is at least ishd, never -1
     (define i (cur-icur c))
     (cond
       [(unsafe-fx> i (cur-ishd c))
        (set-cur-icur! c (unsafe-fx- i 1))
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
        (install-in-chunk! c ch i w)]
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
  (enter-chunk! c dir mi (cur-get mi) (+ (region-base c 2) (cur-index mi))))

(define (fresh-middle-cursor c m dir)
  (define mi (cur-of-tree m (add1 (cur-depth c))))
  (goto-sentinel! mi (if (eq? dir 'forward) 'sf 'sb))
  (cur-move! mi dir)
  mi)

;; Enter the first nonempty region at or after position k in the traversal
;; order, where the order is the regions' numbering going forward and the
;; reverse going backward.  Skipping empty regions explicitly means the cursor
;; does not depend on invariant 1 holding at the root, which it need not for an
;; ephemeral sequence.
(define (enter-region! c dir k)
  (define fwd? (eq? dir 'forward))
  (define (nonempty ch) (and ch (not (chunk-empty? ch)) ch))
  (let try ([k k])
    (cond
      [(> k 4) (finish! c dir)]
      [else
       (define r
         (if fwd?
             k
             (- 4 k)))
       (case r
         [(0) (define f (nonempty (cur-front c)))
              (if f (enter-chunk! c dir 'front f 0) (try (add1 k)))]
         [(1) (define fi (nonempty (cur-ifront c)))
              (if fi (enter-chunk! c dir 'ifront fi (region-base c 1)) (try (add1 k)))]
         [(2) (define m (cur-middle c))
              (if m
                  (enter-middle! c dir (fresh-middle-cursor c m dir))
                  (try (add1 k)))]
         [(3) (define bi (nonempty (cur-iback c)))
              (if bi (enter-chunk! c dir 'iback bi (region-base c 3)) (try (add1 k)))]
         [else (define b (nonempty (cur-back c)))
               (if b (enter-chunk! c dir 'back b (region-base c 4)) (try (add1 k)))])])))

(define (enter-short! c dir v)
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
     (define v (cur-sv c))
     (if v
         (enter-short! c dir v)
         (enter-region! c dir 0))]
    [(symbol? p)
     ;; leaving a chunk region continues with the next region in the direction
     ;; of travel; past the last one lies the sentinel
     (define r (region-of-path p))
     (enter-region! c dir (if (eq? dir 'forward) (add1 r) (- 5 r)))]
    [else
     ;; already inside the middle sequence: step the deeper cursor, and after
     ;; it the next region is the inner back going forward and the inner front
     ;; going backward, both at position 3 of their traversal order
     (cur-move! p dir)
     (if (cur-finished? p)
         (enter-region! c dir 3)
         (enter-middle! c dir p))]))

;; ------------------------------------------------------------ random access

(define (cur-reach! c target)
  (cond
    [(< target 0) (goto-sentinel! c 'sf)]
    [(>= target (cur-wt c)) (goto-sentinel! c 'sb)]
    [(cur-sv c)
     => (lambda (v)
          (set-cur-path! c 'short)
          (set-cur-chunk! c #f)
          (set-cur-support! c v)
          (set-cur-ishd! c 0)
          (set-cur-istl! c (vector-length v))
          (set-cur-icur! c target)
          (set-cur-ishd-sch! c 0)
          (set-cur-w! c 0))]
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
        (install-in-chunk! c ch q (- target j))]
       [else (reach-inside! c target)])]))

;; Descend to the item that covers weight index `target`, exactly as `get`
;; does on the tree itself.
(define (reach-inside! c target)
  (define d (cur-depth c))
  (define m (cur-middle c))
  (define b1 (region-base c 1))
  (define b2 (region-base c 2))
  (define b3 (+ b2 (pt-weight m)))
  (define b4 (+ b3 (weight-of (cur-iback c))))
  ;; the item of chunk ch, which starts at weight index base, that covers
  ;; target; ch is #f only where another thread has rewritten the cursor
  (define (in-chunk! path ch base)
    (unless ch (inconsistent-structure))
    (define-values (q j) (chunk-item-at ch (- target base) d))
    (install-chunk! c path ch q (- target j) base))
  (cond
    [(< target b1) (in-chunk! 'front (cur-front c) 0)]
    [(< target b2) (in-chunk! 'ifront (cur-ifront c) b1)]
    [(< target b3)
     (define p (cur-path c))
     (define mi
       (if (cur? p)
           p
           (cur-of-tree m (add1 d))))
     (define t2 (- target b2))
     (cur-reach! mi t2)
     (define ch (cur-get mi))
     (define j (- t2 (cur-index mi)))
     (define-values (q j2) (chunk-item-at ch j d))
     (install-chunk! c mi ch q (- target j2) (+ b2 (cur-index mi)))]
    [(< target b4) (in-chunk! 'iback (cur-iback c) b3)]
    [else (in-chunk! 'back (cur-back c) b4)]))

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

(struct/unchecked siter (seq kind [birth #:mutable] cursor)
  #:authentic #:sealed
  #:reflection-name 'sek-iterator
  #:methods gen:custom-write
  [(define (write-proc it port mode)
     (write-string
      (format "#<sek-iterator:~a/~a>" (cur-index (siter-cursor it)) (cur-wt (siter-cursor it)))
      port))])

(define sek-iter? siter?)

(define (make-cursor s)
  (cond
    [(psek? s)
     (define r (psek-rep s))
     (cond
       [(not r) (blank-cur 0 0 #f #f #f #f #f #f)]
       [(vector? r) (cur-of-vector r)]
       [else (cur-of-tree r 0)])]
    [else
     ;; read the five parts one at a time, so that a jitter build can pause
     ;; between them
     (define f (esek-front s))
     (jitter!)
     (define fi (esek-ifront s))
     (define m (esek-middle s))
     (jitter!)
     (define bi (esek-iback s))
     (define b (esek-back s))
     (cur-of-level f fi m bi b 0 (esek-length s))]))

;; Refresh a cursor from its sequence, discarding any position.
(define (reload! it)
  (define s (siter-seq it))
  (define c (siter-cursor it))
  (define fresh (make-cursor s))
  (set-cur-wt! c (cur-wt fresh))
  (set-cur-front! c (cur-front fresh))
  (set-cur-ifront! c (cur-ifront fresh))
  (set-cur-middle! c (cur-middle fresh))
  (set-cur-iback! c (cur-iback fresh))
  (set-cur-back! c (cur-back fresh))
  (set-cur-sv! c (cur-sv fresh))
  (goto-sentinel! c 'sf))

(define (sek-iterator-at-sentinel s [side 'front])
  (unless (memq side '(front back))
    (raise-argument-error 'sek-iterator-at-sentinel "(or/c 'front 'back)" side))
  (cond
    [(psek? s)
     (siter s
            'p
            #f
            (let ([c (make-cursor s)])
              (goto-sentinel! c (if (eq? side 'front) 'sf 'sb))
              c))]
    [(esek? s)
     (define birth (esek-iterator-born! s))
     (siter s
            'e
            birth
            (let ([c (make-cursor s)])
              (goto-sentinel! c (if (eq? side 'front) 'sf 'sb))
              c))]
    [else (raise-argument-error 'sek-iterator-at-sentinel "(or/c psek? esek?)" s)]))

;; An iterator positioned on the first element (dir 'forward) or the last
;; element (dir 'backward).  On an empty sequence it starts out finished.
(define (sek-iterator s [dir 'forward])
  (check-sek 'sek-iterator s)
  (unless (memq dir '(forward backward))
    (raise-argument-error 'sek-iterator "(or/c 'forward 'backward)" dir))
  (define it (sek-iterator-at-sentinel s (if (eq? dir 'forward) 'front 'back)))
  (cur-move! (siter-cursor it) dir)
  it)

;; The constructor the library exports as `sek-iterator`: an iterator at index
;; i, where -1 and the length are the two sentinels.  Starting at either end
;; costs O(1); anywhere else, a descent from the root.
(define (sek-iterator-at s [i 0])
  (check-sek 'sek-iterator s)
  (define n (if (psek? s) (psek-length s) (esek-length s)))
  (unless (and (exact-integer? i) (<= -1 i n))
    (raise-argument-error 'sek-iterator (format "(integer-in -1 ~a)" n) i))
  (cond
    [(eqv? i 0) (sek-iterator s 'forward)]
    [(eqv? i (sub1 n)) (sek-iterator s 'backward)]
    [else
     (define it (sek-iterator-at-sentinel s 'front))
     (cur-reach! (siter-cursor it) i)
     it]))

;; The validity test without the argument check, for the callers below that
;; have just made it themselves.
(define (iter-valid? it)
  (or (eq? (siter-kind it) 'p) (esek-iterator-valid? (siter-seq it) (siter-birth it))))

(define (sek-iter-valid? it)
  (check-iter 'sek-iter-valid? it)
  (iter-valid? it))

;; Checks both things an iterator argument can be wrong about, and blames the
;; operation the caller actually named rather than the test it reached.
(define (check-valid! it who)
  (check-iter who it)
  (unless (iter-valid? it)
    (raise-arguments-error who "iterator was invalidated by an update to its sequence")))

(define (sek-iter-sequence it)
  (check-iter 'sek-iter-sequence it)
  (siter-seq it))
(define (sek-iter-length it)
  (check-iter 'sek-iter-length it)
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
  (check-iter 'sek-iter-reset! it)
  (when (eq? (siter-kind it) 'e)
    (set-siter-birth! it (esek-iterator-born! (siter-seq it))))
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
  (check-direction 'sek-iter-move! dir)
  (cur-move! (siter-cursor it) dir))

(define (sek-iter-get-and-move! it [dir 'forward])
  (check-iter 'sek-iter-get-and-move! it)
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
  (check-iter 'sek-iter-segment-and-jump! it)
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
  (check-iter 'sek-iter-segment-and-jump*! it)
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
  (define ch (cur-chunk c))
  (cond
    [(and ch (chunk-owned? ch (esek-id e)))
     (set-siter-birth! it (esek-invalidate-iterators-except! e))]
    [else
     (define i (cur-index c))
     (esek-own-at! e i)
     (set-siter-birth! it (esek-iterator-born! e))
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
  ;; the same guard as `cur-get`: ensure-owned! may have moved the cursor
  (define v (cur-support c))
  (define i (cur-icur c))
  (if (and (unsafe-fx>= i 0) (unsafe-fx< i (unsafe-vector*-length v)))
      (unsafe-vector*-set! v i x)
      (inconsistent-structure)))

(define (sek-iter-set-and-move! it x [dir 'forward])
  (check-iter 'sek-iter-set-and-move! it)
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
  (check-iter 'sek-iter-writable-segment-and-jump! it)
  (define s (sek-iter-writable-segment it dir))
  (cur-jump! (siter-cursor it) dir (segment-length s))
  s)

(define (sek-iter-writable-segment-and-jump*! it [dir 'forward])
  (check-iter 'sek-iter-writable-segment-and-jump*! it)
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
       (define ch (cur-chunk c))
       (when ch
         (unless (eq? (cur-support c) (chunk-data ch))
           (raise (exn:fail:sek "sek iterator: segment is not part of its chunk"
                                (current-continuation-marks))))
         (unless (< (icur-sch c) (chunk-length ch))
           (raise (exn:fail:sek "sek iterator: index outside its chunk"
                                (current-continuation-marks)))))
       (when (cur? p)
         (loop p))]))
  it)

;; For a test that stores wrong values in a cursor on purpose, as another
;; thread using the same iterator can (sek-test/tests/sek/aufheben/torn.rkt).
(module+ testing
  (provide siter-cursor cur? cur-depth cur-path cur-chunk cur-support cur-wt
           set-cur-wt! set-cur-front! set-cur-ifront! set-cur-middle! set-cur-iback!
           set-cur-back! set-cur-path! set-cur-chunk! set-cur-support! set-cur-icur!
           set-cur-ishd! set-cur-istl! set-cur-ishd-sch! set-cur-w! set-cur-wbase!))
