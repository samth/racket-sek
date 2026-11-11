#lang racket/base
;; Persistent sequences -- Charguéraud & Pottier, ICFP 2026, §3.5.
;;
;; A persistent sequence is one of
;;   * #f, the empty sequence;
;;   * a vector of 1..T elements, the compact representation of a short
;;     sequence (the paper's One and Short constructors, unified here);
;;   * a Sek tree (§3.1), for everything longer.
;; The compact representation exists so that a short sequence costs O(n) words
;; instead of the O(K) implied by a level's two chunks.  It appears only at the
;; top of the structure; middle sequences are always trees.

(require racket/vector
         "unchecked.rkt"
         racket/serialize
         racket/stream
         racket/performance-hint
         (only-in "unchecked.rkt"
                  unsafe-vector*-ref unsafe-vector*-length
                  unsafe-fx>= unsafe-fx<= unsafe-fx<)
         "config.rkt"
         "chunk.rkt"
         "ptree.rkt"
         "iterate.rkt")

;; Every function here that a caller outside the library can reach checks its
;; arguments explicitly, with `unless`: the accessors of the library's own
;; structures do not check (see unchecked.rkt), so they would not raise on a
;; value of the wrong kind.

(provide (rename-out [psq? psek?] [psq-rep psek-rep])
         psek-length
         psek-empty?
         empty-psek
         psek-cons
         psek-add
         psek-push-front
         psek-push-back
         psek-pop-front
         psek-pop-back
         psek-first
         psek-last
         psek-ref
         psek-ref/who
         psek-set
         psek-append
         append-psek
         psek-split
         psek-build
         build-tree
         make-psek-builder
         psek-builder-add!
         psek-builder-close
         psek-take
         psek-drop
         psek->list
         list->psek
         psek->vector
         vector->psek
         in-psek
         psek-for-each
         psek-map
         psek
         wrap-rep
         psek-of-tree)

;; ------------------------------------------------------------- the datatype


;; Argument checking is explicit here, because the structure accessors do not
;; check (see unchecked.rkt): on the wrong kind of value they would read
;; whatever happens to be at that offset.
(define-syntax-rule (check-psek who v)
  (unless (psq? v) (raise-argument-error who "psek?" v)))

(define (rep->list rep)
  (cond
    [(not rep) '()]
    [(vector? rep) (vector->list rep)]
    [else
     (let ([acc '()])
       (pt-for-each rep 0 (lambda (x) (set! acc (cons x acc))))
       (reverse acc))]))

(struct/unchecked psq (rep)
  #:constructor-name wrap-rep
  #:authentic #:sealed
  #:property prop:sequence
  (lambda (s) (elements s))
  #:property prop:stream
  (vector (lambda (s) (psek-empty? s))
          (lambda (s) (psek-first s))
          (lambda (s) (let-values ([(v rest) (psek-pop-front s)]) rest)))
  #:property prop:serializable
  (make-serialize-info (lambda (v) (vector (psek->vector v)))
                       (cons 'deserialize-psek
                             (module-path-index-join
                              '(submod "." deserialize)
                              (variable-reference->module-path-index
                               (#%variable-reference))))
                       #f
                       (or (current-load-relative-directory) (current-directory)))
  #:methods gen:equal+hash
  [(define (equal-proc a b rec)
     ;; Length is O(1) and settles most comparisons; the walk after it
     ;; allocates nothing, where converting to lists allocated both.
     (and (fx= (psek-length a) (psek-length b))
          (let ([all2? (sek-all2?)])
            (if all2?
                (all2? 'equal? a b rec)
                (for/and ([x (in-psek a)] [y (in-psek b)]) (rec x y))))))
   (define (hash-proc a rec)
     (hash-elements (elements a) (psek-length a) rec))
   (define (hash2-proc a rec)
     (hash-elements (elements a) (psek-length a) rec))]
  #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define (write-proc s port mode)
     (print-sek "psek" (psek-empty? s) (lambda (f) (psek-for-each s f)) port mode))])

(define empty-psek (wrap-rep #f))

(module+ deserialize
  (provide deserialize-psek)
  (define deserialize-psek
    (make-deserialize-info (lambda (v) (vector->psek v))
                           (lambda () (error 'deserialize-psek "cycles not supported")))))
(define (psek-of-tree t)
  (wrap-rep (normalize t)))

(define (psek-empty? s)
  (check-psek 'psek-empty? s)
  (not (psq-rep s)))

;; Build n elements a whole chunk at a time, rather than pushing one element at
;; a time, which is what the reference's `init` does (`create_by_segments`,
;; filling each chunk with `EChunk.init`).  A push tests whether the back chunk
;; is full, invalidates the iterators and consults the ownership id once per
;; element; filling a chunk does that once per K elements and writes the rest
;; straight into a vector.
;;
;; The tree is then assembled directly: the first chunk is the front, the last
;; is the back, and everything between is pushed into the middle sequence,
;; which is the shape `make-level` wants.  Every chunk but the last is full, so
;; the density invariant holds by construction.
(define (psek-build n proc)
  (cond
    [(eqv? n 0) empty-psek]
    [(<= n (short-threshold)) (wrap-rep (build-vector n proc))]
    [else (wrap-rep (build-tree n proc no-owner))]))

;; The tree of n > 0 elements, element i being (proc i), with every chunk
;; owned by `owner`.  proc is called on 0 through n-1 in order.
(define (build-tree n proc owner)
  (define k (capacity-at 0))
  (define (chunk-at i len)
    (chunk-build k len (lambda (j) (proc (+ i j))) owner))
  (cond
    [(<= n k) (make-level (chunk-at 0 n) #f (make-chunk k owner))]
    [else
     (define rest (remainder n k))
     (define last-start (- n (if (eqv? rest 0) k rest)))
     (define front (chunk-at 0 k))
     (define middle
       (for/fold ([t #f]) ([i (in-range k last-start k)])
         (pt-push-back t (chunk-at i k) k 1 owner)))
     (define back (chunk-at last-start (- n last-start)))
     (make-level front middle back)]))

;; The same idea as `psek-build`, for callers that do not know the length in
;; advance: collect elements into a chunk-sized buffer and emit a whole chunk
;; when it fills, rather than pushing each element into a sequence.  One chunk
;; is held back so that the last full one can become the back of the level when
;; the buffer is empty at the end.
;; `cap` is the leaf capacity, which never changes over a builder's life, so
;; the hot path reads it from the builder rather than re-deriving it.
(struct/unchecked bld ([buf #:mutable] [n #:mutable] cap [front #:mutable]
             [held #:mutable] [middle #:mutable])
  #:authentic #:sealed)

(define (make-psek-builder)
  (define k (capacity-at 0))
  (bld (make-vector k #f) 0 k #f #f #f))

(define (psek-builder-emit! b c)
  (cond
    [(not (bld-front b)) (set-bld-front! b c)]
    [(not (bld-held b)) (set-bld-held! b c)]
    [else
     (define h (bld-held b))
     (set-bld-middle! b (pt-push-back (bld-middle b) h (chunk-weight h) 1 no-owner))
     (set-bld-held! b c)]))

;; Adding one element is a store and a bounds test.  It is split so that the
;; part that runs per element is small enough for the inliner to copy into
;; callers in other modules, which is what lets `sek-map` and `sek-filter` run
;; their loops without a call per element; the chunk-full case is a call.
(begin-encourage-inline
  (define (psek-builder-add! b x)
    (define n (bld-n b))
    (vector-set! (bld-buf b) n x)
    (define n* (fx+ n 1))
    (if (fx< n* (bld-cap b))
        (set-bld-n! b n*)
        (psek-builder-cut! b))))

;; The buffer is full: hand it over as a chunk and start a new one.  It is
;; replaced here and never read again, so the chunk can adopt it rather than
;; copy it.
(define (psek-builder-cut! b)
  (psek-builder-emit! b (chunk-of-fresh-vector (bld-buf b) no-owner))
  (set-bld-buf! b (make-vector (bld-cap b) #f))
  (set-bld-n! b 0))

(define (psek-builder-close b)
  (define k (bld-cap b))
  (define n (bld-n b))
  (define buf (bld-buf b))
  ;; the elements still in the buffer, as a chunk; built only where needed
  (define (partial)
    (and (> n 0) (chunk-build k n (lambda (i) (unsafe-vector*-ref buf i)) no-owner)))
  (define front (bld-front b))
  (define held (bld-held b))
  (cond
    ;; nothing emitted: everything is still in the buffer.  That does not make
    ;; it short enough for the compact representation -- the threshold may be
    ;; below the chunk capacity -- so it still has to be tested.
    [(not front)
     (cond
       [(eqv? n 0) empty-psek]
       [(<= n (short-threshold)) (wrap-rep (vector-copy (bld-buf b) 0 n))]
       [else (wrap-rep (make-level (partial) #f (make-chunk k no-owner)))])]
    [else
     (define-values (mid back)
       (cond
         [(partial)
          => (lambda (last)
               (values (if held
                           (pt-push-back (bld-middle b) held (chunk-weight held) 1 no-owner)
                           (bld-middle b))
                       last))]
         [held (values (bld-middle b) held)]
         [else (values (bld-middle b) (make-chunk k no-owner))]))
     (wrap-rep (normalize (make-level front mid back)))]))

(define (rep-length r)
  (cond
    [(not r) 0]
    [(vector? r) (vector-length r)]
    [else (lvl-weight r)]))

(define (psek-length s)
  (check-psek 'psek-length s)
  (rep-length (psq-rep s)))

;; ------------------------------------------------------- representation swaps

;; A tree that has become short enough is folded back into a vector, which is
;; what keeps the space bound of short sequences independent of K.
(define (normalize t)
  (cond
    [(not t) #f]
    [(<= (lvl-weight t) (short-threshold)) (tree->vector t)]
    [else t]))

(define (tree->vector t)
  (define v (make-vector (pt-weight t) #f))
  (define i 0)
  (pt-for-each t
               0
               (lambda (x)
                 (vector-set! v i x)
                 (set! i (add1 i))))
  v)

;; A compact vector holds at most T elements, and T is at most the leaf
;; capacity, so it always fits in one chunk.
(define (vector->tree v)
  (define k (capacity-at 0))
  (make-level (chunk-of-vector v k unit-measure no-owner) #f (make-chunk k no-owner)))

;; The rep of s as a tree, converting the compact form if necessary.
(define (rep->tree r)
  (if (vector? r)
      (vector->tree r)
      r))

;; ------------------------------------------------------------------ push/pop

(define (psek-cons s x)
  (check-psek 'psek-cons s)
  (define r (psq-rep s))
  (define T (short-threshold))
  (wrap-rep (cond
          [(not r)
           (if (>= T 1)
               (vector x)
               (pt-push-front #f x 1 0 no-owner))]
          [(vector? r)
           (if (< (vector-length r) T)
               (let ([v (make-vector (add1 (vector-length r)) x)])
                 (vector-copy! v 1 r)
                 v)
               (pt-push-front (vector->tree r) x 1 0 no-owner))]
          [else (pt-push-front r x 1 0 no-owner)])))

(define (psek-add s x)
  (check-psek 'psek-add s)
  (define r (psq-rep s))
  (define T (short-threshold))
  (wrap-rep (cond
          [(not r)
           (if (>= T 1)
               (vector x)
               (pt-push-back #f x 1 0 no-owner))]
          [(vector? r)
           (if (< (vector-length r) T)
               (let ([v (make-vector (add1 (vector-length r)) x)])
                 (vector-copy! v 0 r)
                 v)
               (pt-push-back (vector->tree r) x 1 0 no-owner))]
          [else (pt-push-back r x 1 0 no-owner)])))

(define (psek-pop-front s)
  (check-psek 'psek-pop-front s)
  (define r (psq-rep s))
  (cond
    [(not r) (raise-arguments-error 'psek-pop-front "sequence is empty")]
    [(vector? r)
     (values (vector-ref r 0)
             (wrap-rep (if (eqv? (vector-length r) 1)
                       #f
                       (vector-drop r 1))))]
    [else
     (define-values (x t) (pt-pop-front r 0 no-owner))
     (values x (wrap-rep (normalize t)))]))

(define (psek-pop-back s)
  (check-psek 'psek-pop-back s)
  (define r (psq-rep s))
  (cond
    [(not r) (raise-arguments-error 'psek-pop-back "sequence is empty")]
    [(vector? r)
     (define n (vector-length r))
     (values (vector-ref r (sub1 n))
             (wrap-rep (if (eqv? n 1)
                       #f
                       (vector-copy r 0 (sub1 n)))))]
    [else
     (define-values (x t) (pt-pop-back r 0 no-owner))
     (values x (wrap-rep (normalize t)))]))

(define (psek-first s)
  (check-psek 'psek-first s)
  (define r (psq-rep s))
  (cond
    [(not r) (raise-arguments-error 'psek-first "sequence is empty")]
    [(vector? r) (vector-ref r 0)]
    [else (pt-ref r 0 0)]))

(define (psek-last s)
  (check-psek 'psek-last s)
  (define r (psq-rep s))
  (cond
    [(not r) (raise-arguments-error 'psek-last "sequence is empty")]
    [(vector? r) (vector-ref r (sub1 (vector-length r)))]
    [else (pt-ref r (sub1 (lvl-weight r)) 0)]))

;; ------------------------------------------------------------------ get/set

;; `fixnum?` rather than `exact-nonnegative-integer?` for the same reason as
;; in `psek-ref`: the operations below index with `unsafe-fx`, and a length is
;; a fixnum, so a bignum index is out of range by definition.
(define (check-index who s i)
  (check-psek who s)
  (define n (rep-length (psq-rep s)))
  (unless (and (fixnum? i) (unsafe-fx>= i 0) (unsafe-fx< i n))
    (raise-arguments-error who "index out of range" "index" i "length" n)))

;; The bounds check and the dispatch both have to look at the representation,
;; so do it once: `check-index` would go back through `psek-length`, which
;; re-tests whether the rep is a vector or a tree.
;; The guard is `fixnum?` rather than `exact-nonnegative-integer?` for two
;; reasons.  It is faster -- disassembling this procedure with the wider test
;; showed the tag of `i` being examined three times, because nothing downstream
;; can carry "is an exact nonnegative integer" as "is a fixnum", so the
;; comparison re-tests it.  And it is what the rest of the code already
;; assumes: everything below here indexes with `unsafe-fx` operations, which on
;; a bignum are undefined.  A length is a fixnum, so a bignum index is out of
;; range by definition and belongs on the slow path anyway.
;;
;; The body is a macro so that `sek-ref` gets the same code with its own name
;; in the errors, without a second call on the fast path.
(define-syntax-rule (psek-ref-body who s i)
  (let ()
    (check-psek who s)
    (define r (psq-rep s))
    (cond
      [(and (lvl? r) (fixnum? i) (unsafe-fx>= i 0) (unsafe-fx< i (lvl-weight r)))
       (pt-ref r i 0)]
      [(and (vector? r) (fixnum? i) (unsafe-fx>= i 0)
            (unsafe-fx< i (unsafe-vector*-length r)))
       (vector-ref r i)]
      [else
       (check-index who s i)
       (if (vector? r) (vector-ref r i) (pt-ref r i 0))])))

(define (psek-ref s i)
  (psek-ref-body 'psek-ref s i))

(define (psek-ref/who who s i)
  (psek-ref-body who s i))

(define (psek-set s i x)
  (check-index 'psek-set s i)
  (define r (psq-rep s))
  (wrap-rep (if (vector? r)
            (let ([v (vector-copy r)])
              (vector-set! v i x)
              v)
            (pt-set r i x 0 no-owner))))

;; ------------------------------------------------------- concat and split

(define (psek-append s1 s2)
  (check-psek 'psek-append s1)
  (check-psek 'psek-append s2)
  (append-psek 'psek-append s1 s2))

;; The concatenation without the argument checks, for callers that have made
;; them and want an error to name the operation they export.
(define (append-psek who s1 s2)
  (define r1 (psq-rep s1))
  (define r2 (psq-rep s2))
  (check-append-length who (rep-length r1) (rep-length r2))
  (cond
    [(not r1) s2]
    [(not r2) s1]
    [(and (vector? r1) (vector? r2) (<= (+ (vector-length r1) (vector-length r2)) (short-threshold)))
     (wrap-rep (vector-append r1 r2))]
    [else (wrap-rep (normalize (pt-concat (rep->tree r1) (rep->tree r2) 0 no-owner)))]))

;; Split into the first i elements and the rest.
(define (psek-split s i)
  (check-psek 'psek-split s)
  (define n (rep-length (psq-rep s)))
  (unless (and (fixnum? i) (unsafe-fx>= i 0) (unsafe-fx<= i n))
    (raise-arguments-error 'psek-split "index out of range"
                           "index" i "length" n))
  (define r (psq-rep s))
  (cond
    [(eqv? i 0) (values empty-psek s)]
    [(eqv? i n) (values s empty-psek)]
    [(vector? r) (values (wrap-rep (vector-copy r 0 i)) (wrap-rep (vector-copy r i)))]
    [else
     (define-values (t1 t2) (pt-split r i no-owner))
     (values (wrap-rep (normalize t1)) (wrap-rep (normalize t2)))]))

;; One-sided versions, which build only the half that is wanted.  The reference
;; specializes `three_way_split` the same way, into `take`, `drop` and `get`
;; (ShareableSequence.ml); `get` is `psek-ref` here and was already separate.
(define (psek-take s i)
  (check-psek 'psek-take s)
  (define n (rep-length (psq-rep s)))
  (unless (and (fixnum? i) (unsafe-fx>= i 0) (unsafe-fx<= i n))
    (raise-arguments-error 'psek-take "index out of range"
                           "index" i "length" n))
  (define r (psq-rep s))
  (cond
    [(eqv? i 0) empty-psek]
    [(eqv? i n) s]
    [(vector? r) (wrap-rep (vector-copy r 0 i))]
    [else (wrap-rep (normalize (pt-take r i no-owner)))]))

(define (psek-drop s i)
  (check-psek 'psek-drop s)
  (define n (rep-length (psq-rep s)))
  (unless (and (fixnum? i) (unsafe-fx>= i 0) (unsafe-fx<= i n))
    (raise-arguments-error 'psek-drop "index out of range"
                           "index" i "length" n))
  (define r (psq-rep s))
  (cond
    [(eqv? i 0) s]
    [(eqv? i n) empty-psek]
    [(vector? r) (wrap-rep (vector-copy r i))]
    [else (wrap-rep (normalize (pt-drop r i no-owner)))]))

;; -------------------------------------------------------------- conversions

(define (psek->list s)
  (check-psek 'psek->list s)
  (rep->list (psq-rep s)))

(define (psek->vector s)
  (check-psek 'psek->vector s)
  (define r (psq-rep s))
  (cond
    [(not r) (vector)]
    [(vector? r) (vector-copy r)]
    [else (tree->vector r)]))

(define (psek-for-each s proc)
  (check-psek 'psek-for-each s)
  (check-procedure 'psek-for-each proc 1)
  (define r (psq-rep s))
  (cond
    [(not r) (void)]
    [(vector? r)
     (for ([x (in-vector r)])
       (proc x))]
    [else (pt-for-each r 0 proc)]))

;; Walks the tree into a builder, calling proc from first to last.
(define (psek-map s proc)
  (check-psek 'psek-map s)
  (check-procedure 'psek-map proc 1)
  (define r (psq-rep s))
  (cond
    [(not r) empty-psek]
    [(vector? r) (wrap-rep (vector-map proc r))]
    [else
     (define b (make-psek-builder))
     (pt-for-each r 0 (lambda (x) (psek-builder-add! b (proc x))))
     (psek-builder-close b)]))

;; The elements as a Racket sequence: what `for` gets when it is handed a
;; persistent sequence directly, and what equality and hashing walk.
(define (elements s)
  (define r (psq-rep s))
  (define f (sek-sequencer))
  (cond
    [(vector? r) (in-vector r)]
    [f (f 'in-psek s)]
    [else (in-psek s)]))

;; The fallback: a reader that walks the tree directly.
(define (in-psek s)
  (define r (psq-rep s))
  (cond
    [(not r) (in-list '())]
    [(vector? r) (in-vector r)]
    [else (reader->sequence (lambda () (make-reader (list (cons r 0)))))]))

;; A short list becomes the compact vector directly; a longer one goes
;; through a builder, which fills a chunk at a time.
(define (list->psek xs)
  (unless (list? xs) (raise-argument-error 'list->psek "list?" xs))
  (define n (length xs))
  (cond
    [(eqv? n 0) empty-psek]
    [(<= n (short-threshold)) (wrap-rep (list->vector xs))]
    [else
     (define b (make-psek-builder))
     (for ([x (in-list xs)]) (psek-builder-add! b x))
     (psek-builder-close b)]))

;; The push names are what the paper and the OCaml library call these, and
;; are kept so that code written against either goes on working.
(define psek-push-front psek-cons)
(define psek-push-back psek-add)

;; Reads through vector-ref, so a chaperoned vector is copied as it reads.
(define (vector->psek v)
  (unless (vector? v) (raise-argument-error 'vector->psek "vector?" v))
  (psek-build (vector-length v) (lambda (i) (vector-ref v i))))

(define (psek . xs)
  (list->psek xs))
