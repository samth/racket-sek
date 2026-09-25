#lang racket/base
;; Ephemeral sequences -- Charguéraud & Pottier, ICFP 2026, §3.6.
;;
;; An ephemeral sequence is the unboxed root level of a Sek tree, plus an
;; ownership id and two extra chunks: the *inner front* and *inner back*, each
;; of which is either empty or full.  The inner chunks sit between the outer
;; chunks and the middle sequence.  They exist to defeat the scenario in which
;; alternating pushes and pops repeatedly cascade to the bottom of the tree;
;; with them, such a cascade is paid for by at least K operations, which is
;; what makes push and pop amortized O(log_K N).
;;
;; The sequence denoted by an ephemeral sequence is
;;     front ++ ifront ++ middle ++ iback ++ back
;;
;; Every chunk whose id equals the sequence's id is uniquely owned by it and
;; may be updated in place.  Taking a snapshot installs a fresh id, at which
;; point every chunk in the structure silently becomes immutable.

(require racket/fixnum
         racket/serialize
         (only-in racket/unsafe/ops
                  unsafe-fx+ unsafe-fx- unsafe-fx< unsafe-fx<=
                  unsafe-fx> unsafe-fx>= unsafe-fx=)
         "config.rkt"
         "chunk.rkt"
         "ptree.rkt"
         "persistent.rkt"
         "iterate.rkt")

;; Compiled in unsafe mode.  Every function here that a caller outside the
;; library can reach checks its arguments explicitly, with `unless` rather
;; than by relying on a struct accessor or a vector reference to raise --
;; in unsafe mode those do not raise, they read whatever is at the offset.
(#%declare #:unsafe)

(provide (rename-out [esq? eseq?]
                     [esq-id eseq-id]
                     [esq-front eseq-front]
                     [esq-ifront eseq-ifront]
                     [esq-middle eseq-middle]
                     [esq-iback eseq-iback]
                     [esq-back eseq-back])
         make-eseq
         eseq
         eseq-length
         eseq-empty?
         eseq-cons!
         eseq-add!
         eseq-push-front!
         eseq-push-back!
         eseq-pop-front!
         eseq-pop-back!
         eseq-first
         eseq-last
         eseq-ref
         eseq-set!
         eseq-own-at!
         eseq-snapshot
         pseq-edit
         eseq-copy
         eseq-append!
         eseq-concat!
         eseq-split!
         eseq-carve!
         eseq-take!
         eseq-drop!
         eseq->list
         list->eseq
         eseq->vector
         eseq-for-each
         in-eseq
         eseq-clear!
         eseq-assign!
         eseq-snapshot-and-clear!
         ;; internals used by the iterator and the derived operations
         eseq-flush-inner!
         eseq-iterator-born!
         eseq-iterator-valid?
         eseq-invalidate-iterators!
         eseq-invalidate-iterators-except!
         eseq-become!)

(struct esq
        ([id #:mutable] [front #:mutable]
                        [ifront #:mutable]
                        [middle #:mutable]
                        [iback #:mutable]
                        [back #:mutable]
                        [version #:mutable])
  #:authentic #:sealed
  #:property prop:sequence
  (lambda (e) (in-eseq e))
  #:property prop:serializable
  (make-serialize-info (lambda (v) (vector (eseq->vector v)))
                       (cons 'deserialize-eseq
                             (module-path-index-join
                              '(submod "." deserialize)
                              (variable-reference->module-path-index
                               (#%variable-reference))))
                       #f
                       (or (current-load-relative-directory) (current-directory)))
  #:methods gen:equal+hash
  [(define (equal-proc a b rec)
     (and (fx= (eseq-length a) (eseq-length b))
          (for/and ([x (in-eseq a)] [y (in-eseq b)]) (rec x y))))
   (define (hash-proc a rec)
     (hash-elements (in-eseq a) (eseq-length a) rec))
   (define (hash2-proc a rec)
     (hash-elements (in-eseq a) (eseq-length a) rec))]
  #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define (write-proc e port mode)
     (print-sek "eseq" (eseq-empty? e) (lambda (f) (eseq-for-each e f)) port mode))])

;; The front and back chunks start as the shared zero-capacity stand-in, and
;; a real one is allocated by the first push to that side.  Creating a
;; sequence is then O(1) rather than O(K), which matters when a program makes
;; many short-lived ones; nothing downstream gets slower, because the first
;; push allocates exactly the chunk it needs.

;; Argument checking is explicit here, because this module is compiled in
;; unsafe mode: a struct accessor no longer raises on the wrong kind of
;; value, it reads whatever happens to be at that offset.
(define-syntax-rule (check-eseq who v)
  (unless (esq? v) (raise-argument-error who "eseq?" v)))
(define-syntax-rule (check-pseq who v)
  (unless (pseq? v) (raise-argument-error who "pseq?" v)))

(define (make-eseq [n 0] [v #f])
  (define id (fresh-id!))
  (define e (esq id empty-chunk empty-chunk #f empty-chunk empty-chunk 0))
  (for ([_ (in-range n)]) (eseq-add! e v))
  e)

(define (eseq . xs)
  (list->eseq xs))

;; ------------------------------------------------------- iterator validity

;; Iterator validity is tracked with a version number, following the OCaml
;; library.  The sign of the version doubles as a flag: while it is negative
;; or zero, no iterator is live, so an update need only test the sign.  An
;; iterator records the version at its birth and is valid exactly while the
;; sequence still carries that (positive) version.
;; Nothing here consults `check-iterator-validity?`, even though it governs
;; whether invalidation happens at all.  It does not need to: the sign of the
;; version already carries that information.  Only the two functions below --
;; which run when an iterator is created, not when the sequence is updated --
;; ever make the version positive, and they decline to when checking is off.
;; So a mutation tests the sign of a field it has already loaded, and never
;; reads the setting.  That matters because this runs on every push, pop and
;; set, and reading a module-level variable from another module is two
;; dependent loads however it is stored -- an assigned variable, a box, a
;; vector and a mutable struct field all compile to the same pair.
;; This runs on every push, pop and write, so it reads the field once and does
;; its arithmetic in fixnums: with generic `>` and `-` it reloaded the version
;; after testing it and carried an overflow check on the negation.
(define (eseq-invalidate-iterators! e)
  (define v (esq-version e))
  (when (unsafe-fx> v 0)
    (set-esq-version! e (unsafe-fx- 0 v))))

;; Invalidate every iterator, and return a birth date for the one iterator
;; that is allowed to survive.
(define (eseq-invalidate-iterators-except! e)
  (cond
    [(check-iterator-validity?)
     (define v (esq-version e))
     (set-esq-version! e (if (unsafe-fx> v 0)
                             (unsafe-fx+ v 1)
                             (unsafe-fx+ (unsafe-fx- 0 v) 1)))
     (esq-version e)]
    ;; with checking off the version stays non-positive, so no iterator is ever
    ;; considered live and the birth date is not consulted
    [else (esq-version e)]))

;; Prepare the sequence for iteration and return a birth date.  Flushing the
;; inner chunks is what lets an iterator see the plain front/middle/back shape
;; of a level.
(define (eseq-iterator-born! e)
  (eseq-flush-inner! e)
  (define v (esq-version e))
  (when (and (check-iterator-validity?) (unsafe-fx<= v 0))
    (set-esq-version! e (unsafe-fx+ (unsafe-fx- 0 v) 1)))
  (esq-version e))

(define (eseq-iterator-valid? e birth)
  (or (not (check-iterator-validity?))
      (let ([v (esq-version e)])
        (and (unsafe-fx> v 0) (unsafe-fx= birth v)))))

;; Push both inner chunks into the middle sequence, so that the sequence is
;; exactly a front chunk, a middle sequence and a back chunk.
(define (eseq-flush-inner! e)
  (define id (esq-id e))
  (define ibk (esq-iback e))
  (unless (chunk-empty? ibk)
    (set-esq-middle! e (pt-push-back (esq-middle e) ibk (chunk-weight ibk) 1 id))
    (set-esq-iback! e empty-chunk))
  (define ifr (esq-ifront e))
  (unless (chunk-empty? ifr)
    (set-esq-middle! e (pt-push-front (esq-middle e) ifr (chunk-weight ifr) 1 id))
    (set-esq-ifront! e empty-chunk)))

;; Every component weight is bounded by the length of the sequence, so these
;; sums are fixnum arithmetic.
(define (eseq-length e)
  (check-eseq 'eseq-length e)
  (unsafe-fx+
   (unsafe-fx+ (chunk-weight (esq-front e)) (chunk-weight (esq-ifront e)))
   (unsafe-fx+ (pt-weight (esq-middle e))
               (unsafe-fx+ (chunk-weight (esq-iback e))
                           (chunk-weight (esq-back e))))))

(define (eseq-empty? e)
  (check-eseq 'eseq-empty? e)
  (eqv? 0 (eseq-length e)))

(define (eseq-clear! e)
  (check-eseq 'eseq-clear! e)
  (eseq-invalidate-iterators! e)
  (set-esq-id! e (fresh-id!))
  (set-esq-front! e empty-chunk)
  (set-esq-ifront! e empty-chunk)
  (set-esq-middle! e #f)
  (set-esq-iback! e empty-chunk)
  (set-esq-back! e empty-chunk))

;; --------------------------------------------------------------------- push

(define (eseq-cons! e x)
  (check-eseq 'eseq-cons! e)
  (eseq-invalidate-iterators! e)
  (define id (esq-id e))
  (define f (esq-front e))
  (cond
    [(not (chunk-full? f))
     ;; an in-place push returns the very same chunk; skip the field write
     (let ([f* (chunk-push-front f x 1 id)])
       (unless (eq? f f*) (set-esq-front! e f*)))]
    [else
     ;; the front chunk is full: demote it to the inner front, first pushing
     ;; the (necessarily full) old inner front into the middle sequence
     (define ifr (esq-ifront e))
     (unless (chunk-empty? ifr)
       (set-esq-middle! e (pt-push-front (esq-middle e) ifr (chunk-weight ifr) 1 id)))
     (set-esq-ifront! e f)
     (set-esq-front! e (chunk-singleton x 1 (capacity-at 0) id))]))

(define (eseq-add! e x)
  (check-eseq 'eseq-add! e)
  (eseq-invalidate-iterators! e)
  (define id (esq-id e))
  (define b (esq-back e))
  (cond
    [(not (chunk-full? b))
     (let ([b* (chunk-push-back b x 1 id)])
       (unless (eq? b b*) (set-esq-back! e b*)))]
    [else
     (define ibk (esq-iback e))
     (unless (chunk-empty? ibk)
       (set-esq-middle! e (pt-push-back (esq-middle e) ibk (chunk-weight ibk) 1 id)))
     (set-esq-iback! e b)
     (set-esq-back! e (chunk-singleton x 1 (capacity-at 0) id))]))

;; ---------------------------------------------------------------------- pop

(define (eseq-pop-front! e)
  (check-eseq 'eseq-pop-front! e)
  (eseq-invalidate-iterators! e)
  (define id (esq-id e))
  (define f (esq-front e))
  (cond
    [(not (chunk-empty? f))
     (define-values (x f*) (chunk-pop-front f unit-measure id))
     (unless (eq? f f*) (set-esq-front! e f*))
     x]
    ;; refill the front chunk from whatever comes next
    [(not (chunk-empty? (esq-ifront e)))
     (set-esq-front! e (esq-ifront e))
     (set-esq-ifront! e empty-chunk)
     (eseq-pop-front! e)]
    [(esq-middle e)
     (define-values (c m) (pt-pop-front (esq-middle e) 1 id))
     (set-esq-middle! e m)
     (set-esq-front! e c)
     (eseq-pop-front! e)]
    [(not (chunk-empty? (esq-iback e)))
     (set-esq-front! e (esq-iback e))
     (set-esq-iback! e empty-chunk)
     (eseq-pop-front! e)]
    [(not (chunk-empty? (esq-back e)))
     (define-values (x b*) (chunk-pop-front (esq-back e) unit-measure id))
     (set-esq-back! e b*)
     x]
    [else (raise-arguments-error 'eseq-pop-front! "sequence is empty")]))

(define (eseq-pop-back! e)
  (check-eseq 'eseq-pop-back! e)
  (eseq-invalidate-iterators! e)
  (define id (esq-id e))
  (define b (esq-back e))
  (cond
    [(not (chunk-empty? b))
     (define-values (x b*) (chunk-pop-back b unit-measure id))
     (unless (eq? b b*) (set-esq-back! e b*))
     x]
    [(not (chunk-empty? (esq-iback e)))
     (set-esq-back! e (esq-iback e))
     (set-esq-iback! e empty-chunk)
     (eseq-pop-back! e)]
    [(esq-middle e)
     (define-values (c m) (pt-pop-back (esq-middle e) 1 id))
     (set-esq-middle! e m)
     (set-esq-back! e c)
     (eseq-pop-back! e)]
    [(not (chunk-empty? (esq-ifront e)))
     (set-esq-back! e (esq-ifront e))
     (set-esq-ifront! e empty-chunk)
     (eseq-pop-back! e)]
    [(not (chunk-empty? (esq-front e)))
     (define-values (x f*) (chunk-pop-back (esq-front e) unit-measure id))
     (set-esq-front! e f*)
     x]
    [else (raise-arguments-error 'eseq-pop-back! "sequence is empty")]))

;; ----------------------------------------------------------------- get/set

;; Locate atomic index i among the five components of the sequence.
(define (eseq-locate e i who)
  (define nf (chunk-weight (esq-front e)))
  (define ni (chunk-weight (esq-ifront e)))
  (define nm (pt-weight (esq-middle e)))
  (define nj (chunk-weight (esq-iback e)))
  (define nb (chunk-weight (esq-back e)))
  (cond
    [(unsafe-fx< i nf) (values 'front i)]
    [(unsafe-fx< i (unsafe-fx+ nf ni)) (values 'ifront (unsafe-fx- i nf))]
    [(unsafe-fx< i (unsafe-fx+ (unsafe-fx+ nf ni) nm))
     (values 'middle (unsafe-fx- (unsafe-fx- i nf) ni))]
    [(unsafe-fx< i (unsafe-fx+ (unsafe-fx+ nf ni) (unsafe-fx+ nm nj)))
     (values 'iback (unsafe-fx- (unsafe-fx- (unsafe-fx- i nf) ni) nm))]
    [(unsafe-fx< i (unsafe-fx+ (unsafe-fx+ nf ni) (unsafe-fx+ nm (unsafe-fx+ nj nb))))
     (values 'back (unsafe-fx- (unsafe-fx- (unsafe-fx- (unsafe-fx- i nf) ni) nm) nj))]
    [else (raise-arguments-error who "index out of range"
                                 "index" i "length" (eseq-length e))]))

;; The index guard on the operations below is `fixnum?` and not the wider
;; `exact-nonnegative-integer?`, because `eseq-locate` indexes with `unsafe-fx`
;; operations and those are undefined on a bignum.  Nothing is given up: a
;; length is a fixnum, so a bignum index is out of range by definition.
;;
;; Reaching here means `i` is not a nonnegative fixnum: either it is not an
;; index at all, or it is a bignum, which such a sequence cannot hold.
(define (bad-index who e i)
  (unless (exact-nonnegative-integer? i)
    (raise-argument-error who "exact-nonnegative-integer?" i))
  (raise-arguments-error who "index out of range"
                         "index" i "length" (eseq-length e)))

(define (eseq-ref e i)
  (check-eseq 'eseq-ref e)
  (unless (and (fixnum? i) (unsafe-fx>= i 0))
    (bad-index 'eseq-ref e i))
  (define-values (where j) (eseq-locate e i 'eseq-ref))
  (case where
    [(front) (chunk-ref (esq-front e) j)]
    [(ifront) (chunk-ref (esq-ifront e) j)]
    [(middle) (pt-ref (esq-middle e) j 1)]
    [(iback) (chunk-ref (esq-iback e) j)]
    [else (chunk-ref (esq-back e) j)]))

(define (eseq-set! e i x)
  (check-eseq 'eseq-set! e)
  (unless (and (fixnum? i) (unsafe-fx>= i 0))
    (bad-index 'eseq-set! e i))
  ;; a set may replace a shared chunk with a private copy, so any iterator
  ;; holding on to the old chunk has to go
  (eseq-invalidate-iterators! e)
  (define id (esq-id e))
  (define-values (where j) (eseq-locate e i 'eseq-set!))
  (case where
    [(front) (set-esq-front! e (chunk-set (esq-front e) j x 1 1 id))]
    [(ifront) (set-esq-ifront! e (chunk-set (esq-ifront e) j x 1 1 id))]
    [(middle) (set-esq-middle! e (pt-set (esq-middle e) j x 1 id))]
    [(iback) (set-esq-iback! e (chunk-set (esq-iback e) j x 1 1 id))]
    [else (set-esq-back! e (chunk-set (esq-back e) j x 1 1 id))]))

;; Take ownership of whichever chunk holds index i, so that a caller may write
;; into its data vector directly.  The middle sequence is handled by writing
;; the element back through pt-set, which copies the spine; only the end chunks
;; are on the hot path for iterators.
(define (eseq-own-at! e i)
  (eseq-invalidate-iterators! e)
  (define id (esq-id e))
  (define-values (where j) (eseq-locate e i 'eseq-own-at!))
  (case where
    [(front) (set-esq-front! e (chunk-own (esq-front e) id))]
    [(ifront) (set-esq-ifront! e (chunk-own (esq-ifront e) id))]
    [(middle) (set-esq-middle! e (pt-own (esq-middle e) j 1 id))]
    [(iback) (set-esq-iback! e (chunk-own (esq-iback e) id))]
    [else (set-esq-back! e (chunk-own (esq-back e) id))]))

(define (eseq-first e)
  (check-eseq 'eseq-first e)
  (when (eseq-empty? e)
    (raise-arguments-error 'eseq-first "sequence is empty"))
  (eseq-ref e 0))

(define (eseq-last e)
  (check-eseq 'eseq-last e)
  (when (eseq-empty? e)
    (raise-arguments-error 'eseq-last "sequence is empty"))
  (eseq-ref e (sub1 (eseq-length e))))

;; --------------------------------------------------------------- conversions

;; snapshot (§3.6): install a fresh id, which strips ownership from every
;; chunk in the structure, then fold the inner chunks into the middle sequence
;; and restore invariant 1.
;;
;; This does not invalidate live iterators: an iterator can only be live if no
;; update has happened since it was born, in which case the inner chunks are
;; still empty and this leaves the shape of the sequence alone.  Losing
;; ownership of the chunks is something an iterator tolerates; it only means
;; that a subsequent write through it takes the copy-on-write path.
(define (eseq-snapshot e)
  (check-eseq 'eseq-snapshot e)
  ;; Flush first, while the sequence still owns its chunks, so that the push
  ;; can update them in place; only then hand out a fresh id, which is what
  ;; makes every chunk immutable and safe to share with the snapshot.
  (eseq-flush-inner! e)
  (set-esq-id! e (fresh-id!))
  (pseq-of-tree
   (pt-populate-sides (esq-front e) (esq-middle e) (esq-back e) 0 no-owner)))

;; Take the snapshot and empty the sequence.  This avoids leaving the two
;; structures sharing chunks, so later updates to the sequence never pay for
;; copy-on-write; it is the cheaper operation when the old contents are not
;; needed.
(define (eseq-snapshot-and-clear! e)
  (check-eseq 'eseq-snapshot-and-clear! e)
  (define s (eseq-snapshot e))
  (eseq-clear! e)
  s)

;; edit (§2.1): a fresh ephemeral sequence over the same immutable structure.
(define (pseq-edit s)
  (check-pseq 'pseq-edit s)
  (define id (fresh-id!))
  (define k (capacity-at 0))
  (define r (pseq-rep s))
  (cond
    [(not r) (make-eseq)]
    [(vector? r)
     (esq id (chunk-of-vector r k unit-measure id) empty-chunk #f empty-chunk
          empty-chunk 0)]
    [else
     (esq id (lvl-front r) empty-chunk (lvl-middle r) empty-chunk (lvl-back r) 0)]))

;; mode 'copy, the default, walks the elements, which costs O(n) but leaves
;; both structures with their own chunks.  Mode 'share leaves the copy sharing
;; chunks with e, which is O(1) now and pays for itself later only if neither
;; sequence is updated much -- and e loses ownership of its chunks as well.
(define (eseq-copy e #:mode [mode 'copy])
  (check-eseq 'eseq-copy e)
  (case mode
    [(share)
     ;; Hand both sequences a fresh identity and let them share everything.
     ;; Neither owns a chunk any more, so a later push either extends a
     ;; support monotonically -- which no view can observe -- or copies the
     ;; chunk it is writing to; either way the two stay independent.  The
     ;; reference's shallow_copy duplicates the end chunks up front; deferring
     ;; that makes the copy itself O(1).
     (set-esq-id! e (fresh-id!))
     (esq (fresh-id!) (esq-front e) (esq-ifront e) (esq-middle e)
          (esq-iback e) (esq-back e) 0)]
    [(copy) (list->eseq (eseq->list e))]
    [else (raise-argument-error 'eseq-copy "(or/c 'share 'copy)" mode)]))

(define (eseq-become! e s)
  (define e* (pseq-edit s))
  (eseq-invalidate-iterators! e)
  (set-esq-id! e (esq-id e*))
  (set-esq-front! e (esq-front e*))
  (set-esq-ifront! e (esq-ifront e*))
  (set-esq-middle! e (esq-middle e*))
  (set-esq-iback! e (esq-iback e*))
  (set-esq-back! e (esq-back e*))
  (void))

;; Move the contents of e2 into e1 and empty e2.
(define (eseq-assign! e1 e2)
  (check-eseq 'eseq-assign! e1)
  (check-eseq 'eseq-assign! e2)
  (unless (eq? e1 e2)
    (eseq-become! e1 (eseq-snapshot e2))
    (eseq-clear! e2))
  (void))

;; Append the contents of `other` to `e` at the given end.  As in the OCaml
;; library, an ephemeral `other` is emptied: handing over its representation
;; rather than sharing it is what keeps later updates to either sequence out
;; of the copy-on-write path.  A persistent `other` is of course untouched.
(define (eseq-append! e other [side 'back])
  (check-eseq 'eseq-append! e)
  (unless (or (esq? other) (pseq? other))
    (raise-argument-error 'eseq-append! "(or/c eseq? pseq?)" other))
  (when (eq? e other)
    (raise-arguments-error 'eseq-append! "the two sequences must be distinct"))
  (define o (if (esq? other) (eseq-snapshot-and-clear! other) other))
  (define self (eseq-snapshot-and-clear! e))
  (eseq-become! e (if (eq? side 'front) (pseq-append o self) (pseq-append self o))))

;; The concatenation of e1 and e2, as a new sequence; both are emptied.
(define (eseq-concat! e1 e2)
  (check-eseq 'eseq-concat! e1)
  (check-eseq 'eseq-concat! e2)
  (when (eq? e1 e2)
    (raise-arguments-error 'eseq-concat! "the two sequences must be distinct"))
  (pseq-edit (pseq-append (eseq-snapshot-and-clear! e1)
                          (eseq-snapshot-and-clear! e2))))

;; Split e at index i into two new sequences; e is emptied.
(define (eseq-split! e i)
  (check-eseq 'eseq-split! e)
  (define-values (s1 s2) (pseq-split (eseq-snapshot-and-clear! e) i))
  (values (pseq-edit s1) (pseq-edit s2)))

;; Split e at index i, keeping one part in e and returning the other:
;; 'back keeps the front part, 'front keeps the back part.
(define (eseq-carve! e i [side 'back])
  (check-eseq 'eseq-carve! e)
  (define-values (s1 s2) (pseq-split (eseq-snapshot-and-clear! e) i))
  (cond
    [(eq? side 'back) (eseq-become! e s1) (pseq-edit s2)]
    [else (eseq-become! e s2) (pseq-edit s1)]))

;; Truncate e at index i, keeping the front part ('front) or the back part
;; ('back).
;; Only one side is kept, so only one side is built -- pseq-take and pseq-drop
;; are the specialized splits.
(define (eseq-take! e i [side 'front])
  (check-eseq 'eseq-take! e)
  (define s (eseq-snapshot-and-clear! e))
  (eseq-become! e (if (eq? side 'front) (pseq-take s i) (pseq-drop s i))))

(define (eseq-drop! e i [side 'front])
  (check-eseq 'eseq-drop! e)
  (eseq-take! e i (if (eq? side 'front) 'back 'front)))

(define (eseq-for-each e proc)
  (check-eseq 'eseq-for-each e)
  (define (chunk-elems c)
    (for ([j (in-range (chunk-length c))])
      (proc (chunk-ref c j))))
  (chunk-elems (esq-front e))
  (chunk-elems (esq-ifront e))
  (pt-for-each (esq-middle e) 1 proc)
  (chunk-elems (esq-iback e))
  (chunk-elems (esq-back e)))

(define (eseq->list e)
  (check-eseq 'eseq->list e)
  (define acc '())
  (eseq-for-each e (lambda (x) (set! acc (cons x acc))))
  (reverse acc))

(define (eseq->vector e)
  (check-eseq 'eseq->vector e)
  (define v (make-vector (eseq-length e) #f))
  (define i 0)
  (eseq-for-each e
                 (lambda (x)
                   (vector-set! v i x)
                   (set! i (add1 i))))
  v)

;; Streaming traversal.  As with any iterator over a mutable structure, the
;; result must not be used across an update to e.
(define (in-eseq e)
  (reader->sequence
   (lambda ()
     (make-reader (list (cons (esq-front e) 0)
                        (cons (esq-ifront e) 0)
                        (cons (esq-middle e) 1)
                        (cons (esq-iback e) 0)
                        (cons (esq-back e) 0))))))

(module+ deserialize
  (provide deserialize-eseq)
  (define deserialize-eseq
    (make-deserialize-info (lambda (v) (list->eseq (vector->list v)))
                           (lambda () (error 'deserialize-eseq "cycles not supported")))))

(define (list->eseq xs)
  (define e (make-eseq))
  (for ([x (in-list xs)])
    (eseq-add! e x))
  e)

;; The push names are what the paper and the OCaml library call these, and are
;; kept so that code written against either goes on working.
(define eseq-push-front! eseq-cons!)
(define eseq-push-back! eseq-add!)
