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

(require "unchecked.rkt"
         racket/performance-hint
         racket/serialize
         (only-in "unchecked.rkt"
                  unsafe-fx+ unsafe-fx- unsafe-fx< unsafe-fx<=
                  unsafe-fx> unsafe-fx>= unsafe-fx= unsafe-fxabs)
         "config.rkt"
         "chunk.rkt"
         "ptree.rkt"
         "persistent.rkt"
         "iterate.rkt")

;; Every function here that a caller outside the library can reach checks its
;; arguments explicitly, with `unless`: the accessors of the library's own
;; structures do not check (see unchecked.rkt), so they would not raise on a
;; value of the wrong kind.

(provide (rename-out [esq? esek?]
                     [esq-id esek-id]
                     [esq-front esek-front]
                     [esq-ifront esek-ifront]
                     [esq-middle esek-middle]
                     [esq-iback esek-iback]
                     [esq-back esek-back])
         make-esek
         esek-build
         esek
         esek-length
         esek-empty?
         esek-cons!
         esek-add!
         esek-push-front!
         esek-push-back!
         esek-pop-front!
         esek-pop-back!
         esek-first
         esek-last
         esek-ref
         esek-set!
         esek-own-at!
         esek-snapshot
         psek-edit
         esek-copy
         esek-append!
         esek-concat!
         esek-split!
         esek-carve!
         esek-take!
         esek-drop!
         esek->list
         list->esek
         esek->vector
         esek-for-each
         in-esek
         esek-clear!
         esek-assign!
         esek-snapshot-and-clear!
         ;; internals used by the iterator and the derived operations
         esek-ref/who
         append-esek!
         esek-iterator-born!
         esek-iterator-valid?
         (rename-out [esq-version esek-version])
         raise-updated
         esek-invalidate-iterators-except!)

(struct/unchecked esq
        ([id #:mutable] [front #:mutable]
                        [ifront #:mutable]
                        [middle #:mutable]
                        [iback #:mutable]
                        [back #:mutable]
                        ;; see "iterator validity" below: its magnitude counts
                        ;; updates, and its sign says whether `folded` is set
                        [version #:mutable]
                        ;; the middle sequence with the inner chunks folded in,
                        ;; left by a snapshot for the next update to adopt
                        [folded #:mutable])
  #:authentic #:sealed
  #:property prop:sequence
  (lambda (e) (elements e))
  #:property prop:serializable
  (make-serialize-info (lambda (v) (vector (esek->vector v)))
                       (cons 'deserialize-esek
                             (module-path-index-join
                              '(submod "." deserialize)
                              (variable-reference->module-path-index
                               (#%variable-reference))))
                       #f
                       (or (current-load-relative-directory) (current-directory)))
  #:methods gen:equal+hash
  [(define (equal-proc a b rec)
     (and (fx= (esek-length a) (esek-length b))
          (let ([all2? (sek-all2?)])
            (if all2?
                (all2? 'equal? a b rec)
                (for/and ([x (in-esek a)] [y (in-esek b)]) (rec x y))))))
   (define (hash-proc a rec)
     (hash-elements (elements a) (esek-length a) rec))
   (define (hash2-proc a rec)
     (hash-elements (elements a) (esek-length a) rec))]
  #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define (write-proc e port mode)
     (print-sek "esek" (esek-empty? e) (lambda (f) (esek-for-each e f)) port mode))])

;; The front and back chunks start as the shared zero-capacity stand-in, and
;; a real one is allocated by the first push to that side.  Creating a
;; sequence is then O(1) rather than O(K), which matters when a program makes
;; many short-lived ones; nothing downstream gets slower, because the first
;; push allocates exactly the chunk it needs.

;; Argument checking is explicit here, because the structure accessors do not
;; check (see unchecked.rkt): on the wrong kind of value they would read
;; whatever happens to be at that offset.
(define-syntax-rule (check-esek who v)
  (unless (esq? v) (raise-argument-error who "esek?" v)))
(define-syntax-rule (check-psek who v)
  (unless (psek? v) (raise-argument-error who "psek?" v)))

(define (make-esek [n 0] [v #f])
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'make-esek "exact-nonnegative-integer?" n))
  (esek-build n (lambda (_) v)))

;; An ephemeral sequence of n elements, element i being (proc i), built a
;; chunk at a time as `psek-build` builds, but with every chunk owned by the
;; new sequence, so that its own later writes copy nothing.  proc is called on
;; 0 through n-1 in order.
(define (esek-build n proc)
  (define id (fresh-id!))
  (cond
    [(eqv? n 0) (esq id empty-chunk empty-chunk #f empty-chunk empty-chunk 1 #f)]
    [else
     (define t (build-tree n proc id))
     (esq id (lvl-front t) empty-chunk (lvl-middle t) empty-chunk (lvl-back t) 1 #f)]))

(define (esek . xs)
  (list->esek xs))

;; ------------------------------------------------------- iterator validity

;; Iterator validity is tracked with a version number, following the OCaml
;; library.  Every update advances the version; an iterator records the
;; version at its birth and is valid exactly while the sequence still carries
;; it.  Creating an iterator therefore only reads the sequence, which is what
;; lets several threads iterate over one ephemeral sequence at once.
;;
;; The field holds the version's magnitude, which starts at 1, and its sign
;; says whether a snapshot has left a fold for the next update to adopt (see
;; `esek-snapshot`): negative means it has.  Every update runs
;; `esek-invalidate-iterators!` before it looks at the structure, so that is
;; where the fold is adopted, and an update without one pays a sign test on the
;; value it has loaded anyway.  It advances the version whether or not any
;; iterator is live, and whether or not `check-iterator-validity?` is on, and
;; never reads the setting, which lives in another module and would cost two
;; dependent loads.  The setting is read only when an iterator's validity is
;; tested.
;;
;; Every push and pop runs this, and with the branch to `adopt-fold!` it is
;; just past the size that the compiler inlines on its own; left as a call it
;; cost each push and pop about a nanosecond.
(begin-encourage-inline
  (define (esek-invalidate-iterators! e)
    (define v (esq-version e))
    (if (unsafe-fx< v 0)
        (adopt-fold! e v)
        (set-esq-version! e (unsafe-fx+ v 1)))))

;; An update has exclusive access, so it may install the fold -- several
;; fields at once -- where a snapshot, which may run beside readers, could
;; not.  Nothing has changed since the fold was made, because any update would
;; have adopted it first.
(define (adopt-fold! e v)
  (set-esq-middle! e (esq-folded e))
  (jitter!)
  (set-esq-ifront! e empty-chunk)
  (set-esq-iback! e empty-chunk)
  (set-esq-folded! e #f)
  (set-esq-version! e (unsafe-fx+ (unsafe-fx- 0 v) 1)))

;; Invalidate every iterator, and return a birth date for the one iterator
;; that is allowed to survive.
(define (esek-invalidate-iterators-except! e)
  (esek-invalidate-iterators! e)
  (esq-version e))

;; A birth date for a new iterator.
(define (esek-iterator-born! e)
  (unsafe-fxabs (esq-version e)))

(define (esek-iterator-valid? e birth)
  (or (not (check-iterator-validity?))
      (unsafe-fx= birth (unsafe-fxabs (esq-version e)))))

;; The same test without the setting, for a traversal, which consults the
;; setting once rather than at every element.  generic.rkt makes it with
;; `esek-version`, which it can inline: this procedure, which reads a field
;; this module does not export, it could not.
(begin-encourage-inline
  (define (esek-unchanged? e birth)
    (unsafe-fx= birth (unsafe-fxabs (esq-version e)))))

;; A traversal that hands elements to the caller's code reads them from a run
;; of contiguous storage, and that code may update the sequence: a write to the
;; run shows through, and a pop leaves the private empty-slot marker in a slot
;; the run still covers.  So such a traversal checks, before each element it
;; hands over, that the sequence has not been updated since it began, and
;; raises as an invalidated iterator would.
(define (raise-updated who)
  (raise-arguments-error who "sequence was updated during the traversal"))

;; The middle sequence with both inner chunks pushed onto it.  With e's own id
;; as `owner` the pushes may update e's chunks in place; with `no-owner` they
;; leave e untouched.
(define (middle-with-inner e owner)
  (define ibk (esq-iback e))
  (define ifr (esq-ifront e))
  (let* ([m (esq-middle e)]
         [m (if (chunk-empty? ibk) m (pt-push-back m ibk (chunk-weight ibk) 1 owner))])
    (if (chunk-empty? ifr) m (pt-push-front m ifr (chunk-weight ifr) 1 owner))))

;; Push both inner chunks into the middle sequence, so that the sequence is
;; exactly a front chunk, a middle sequence and a back chunk.
(define (esek-flush-inner! e)
  (esek-invalidate-iterators! e)
  (set-esq-middle! e (middle-with-inner e (esq-id e)))
  (set-esq-iback! e empty-chunk)
  (set-esq-ifront! e empty-chunk))

;; Every component weight is bounded by the length of the sequence, so these
;; sums are fixnum arithmetic.
(define (esek-length e)
  (check-esek 'esek-length e)
  (unsafe-fx+
   (unsafe-fx+ (chunk-weight (esq-front e)) (chunk-weight (esq-ifront e)))
   (unsafe-fx+ (pt-weight (esq-middle e))
               (unsafe-fx+ (chunk-weight (esq-iback e))
                           (chunk-weight (esq-back e))))))

(define (esek-empty? e)
  (check-esek 'esek-empty? e)
  (eqv? 0 (esek-length e)))

(define (esek-clear! e)
  (check-esek 'esek-clear! e)
  (esek-invalidate-iterators! e)
  (set-esq-id! e (fresh-id!))
  (set-esq-front! e empty-chunk)
  (set-esq-ifront! e empty-chunk)
  (set-esq-middle! e #f)
  (set-esq-iback! e empty-chunk)
  (set-esq-back! e empty-chunk))

;; --------------------------------------------------------------------- push

(define (esek-cons! e x)
  (check-esek 'esek-cons! e)
  (esek-invalidate-iterators! e)
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
     (jitter!)
     (set-esq-ifront! e f)
     (set-esq-front! e (chunk-singleton x 1 (capacity-at 0) id))]))

(define (esek-add! e x)
  (check-esek 'esek-add! e)
  (esek-invalidate-iterators! e)
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
     (jitter!)
     (set-esq-iback! e b)
     (set-esq-back! e (chunk-singleton x 1 (capacity-at 0) id))]))

;; ---------------------------------------------------------------------- pop

(define (esek-pop-front! e)
  (check-esek 'esek-pop-front! e)
  (esek-invalidate-iterators! e)
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
     (esek-pop-front! e)]
    ;; the tree that was tested is the one popped: a racing writer could empty
    ;; the field between two reads, and the tree accessors take no #f
    [(esq-middle e)
     => (lambda (t)
          (define-values (c m) (pt-pop-front t 1 id))
          ;; a chunk from the middle sequence is never empty (invariant 2)
          ;; unless a data race has broken the structure; recurring on one
          ;; could go on forever
          (when (chunk-empty? c) (inconsistent-structure))
          (set-esq-middle! e m)
          (set-esq-front! e c)
          (esek-pop-front! e))]
    [(not (chunk-empty? (esq-iback e)))
     (set-esq-front! e (esq-iback e))
     (set-esq-iback! e empty-chunk)
     (esek-pop-front! e)]
    [(not (chunk-empty? (esq-back e)))
     (define-values (x b*) (chunk-pop-front (esq-back e) unit-measure id))
     (set-esq-back! e b*)
     x]
    [else (raise-arguments-error 'esek-pop-front! "sequence is empty")]))

(define (esek-pop-back! e)
  (check-esek 'esek-pop-back! e)
  (esek-invalidate-iterators! e)
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
     (esek-pop-back! e)]
    ;; as in esek-pop-front!
    [(esq-middle e)
     => (lambda (t)
          (define-values (c m) (pt-pop-back t 1 id))
          (when (chunk-empty? c) (inconsistent-structure))
          (set-esq-middle! e m)
          (set-esq-back! e c)
          (esek-pop-back! e))]
    [(not (chunk-empty? (esq-ifront e)))
     (set-esq-back! e (esq-ifront e))
     (set-esq-ifront! e empty-chunk)
     (esek-pop-back! e)]
    [(not (chunk-empty? (esq-front e)))
     (define-values (x f*) (chunk-pop-back (esq-front e) unit-measure id))
     (set-esq-front! e f*)
     x]
    [else (raise-arguments-error 'esek-pop-back! "sequence is empty")]))

;; ----------------------------------------------------------------- get/set

;; Locate atomic index i among the five components of the sequence.
(define (esek-locate e i who)
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
                                 "index" i "length" (esek-length e))]))

;; The middle sequence, where `esek-locate` has just found the index.  Only a
;; data race can have emptied the field since, but the tree accessors take no
;; #f, so read it once and test what was read.
(define (located-middle e)
  (or (esq-middle e) (inconsistent-structure)))

;; The index guard on the operations below is `fixnum?` and not the wider
;; `exact-nonnegative-integer?`, because `esek-locate` indexes with `unsafe-fx`
;; operations and those are undefined on a bignum.  Nothing is given up: a
;; length is a fixnum, so a bignum index is out of range by definition.
;;
;; Reaching here means `i` is not a nonnegative fixnum: either it is not an
;; index at all, or it is a bignum, which such a sequence cannot hold.
(define (bad-index who e i)
  (unless (exact-nonnegative-integer? i)
    (raise-argument-error who "exact-nonnegative-integer?" i))
  (raise-arguments-error who "index out of range"
                         "index" i "length" (esek-length e)))

;; A macro, as `psek-ref-body` is, so that `sek-ref` names itself in errors.
(define-syntax-rule (esek-ref-body who e i)
  (let ()
    (check-esek who e)
    (unless (and (fixnum? i) (unsafe-fx>= i 0))
      (bad-index who e i))
    (define-values (where j) (esek-locate e i who))
    (case where
      [(front) (chunk-ref (esq-front e) j)]
      [(ifront) (chunk-ref (esq-ifront e) j)]
      [(middle) (pt-ref (located-middle e) j 1)]
      [(iback) (chunk-ref (esq-iback e) j)]
      [else (chunk-ref (esq-back e) j)])))

(define (esek-ref e i)
  (esek-ref-body 'esek-ref e i))

(define (esek-ref/who who e i)
  (esek-ref-body who e i))

(define (esek-set! e i x)
  (check-esek 'esek-set! e)
  (unless (and (fixnum? i) (unsafe-fx>= i 0))
    (bad-index 'esek-set! e i))
  ;; a set may replace a shared chunk with a private copy, so any iterator
  ;; holding on to the old chunk has to go
  (esek-invalidate-iterators! e)
  (define id (esq-id e))
  (define-values (where j) (esek-locate e i 'esek-set!))
  (case where
    [(front) (set-esq-front! e (chunk-set (esq-front e) j x 1 1 id))]
    [(ifront) (set-esq-ifront! e (chunk-set (esq-ifront e) j x 1 1 id))]
    [(middle) (set-esq-middle! e (pt-set (located-middle e) j x 1 id))]
    [(iback) (set-esq-iback! e (chunk-set (esq-iback e) j x 1 1 id))]
    [else (set-esq-back! e (chunk-set (esq-back e) j x 1 1 id))]))

;; Take ownership of whichever chunk holds index i, so that a caller may write
;; into its data vector directly.  The middle sequence is handled by writing
;; the element back through pt-set, which copies the spine; only the end chunks
;; are on the hot path for iterators.
(define (esek-own-at! e i)
  (esek-invalidate-iterators! e)
  (define id (esq-id e))
  (define-values (where j) (esek-locate e i 'esek-own-at!))
  (case where
    [(front) (set-esq-front! e (chunk-own (esq-front e) id))]
    [(ifront) (set-esq-ifront! e (chunk-own (esq-ifront e) id))]
    [(middle) (set-esq-middle! e (pt-own (located-middle e) j 1 id))]
    [(iback) (set-esq-iback! e (chunk-own (esq-iback e) id))]
    [else (set-esq-back! e (chunk-own (esq-back e) id))]))

(define (esek-first e)
  (check-esek 'esek-first e)
  (when (esek-empty? e)
    (raise-arguments-error 'esek-first "sequence is empty"))
  (esek-ref e 0))

(define (esek-last e)
  (check-esek 'esek-last e)
  (when (esek-empty? e)
    (raise-arguments-error 'esek-last "sequence is empty"))
  (esek-ref e (sub1 (esek-length e))))

;; --------------------------------------------------------------- conversions

;; snapshot (§3.6): install a fresh id, which strips ownership from every
;; chunk in the structure, then build the snapshot's tree from the five parts,
;; with the inner chunks folded into its middle sequence.
;;
;; A snapshot counts as a read: several threads may take one while others read
;; the sequence.  So the inner chunks are pushed onto a persistent copy of the
;; middle sequence, not moved into the sequence's own, and the sequence keeps
;; its shape.  Live iterators stay valid, and losing ownership of the chunks
;; only means that a later write through one of them takes the copy-on-write
;; path.
;;
;; The copy is left in `folded`, and the version made negative, so that the
;; next update adopts it (see `esek-invalidate-iterators!`); until then, a
;; further snapshot reuses it.  Without that, a sequence snapshotted after
;; every few updates would fold the same inner chunks again each time.  The
;; fields written -- the id, `folded` and the sign of the version -- are ones
;; that reads do not consult, or consult only through the version's magnitude.
;; Two snapshots taken at once may each make a fold; either will do.  A
;; snapshot that sees the version negative but `folded` not yet set, as one
;; racing another may, folds for itself.
(define (esek-snapshot e)
  (check-esek 'esek-snapshot e)
  (set-esq-id! e (fresh-id!))
  (define v (esq-version e))
  (define m
    (cond
      [(and (unsafe-fx< v 0) (esq-folded e))]
      [(and (chunk-empty? (esq-ifront e)) (chunk-empty? (esq-iback e))) (esq-middle e)]
      [else
       (define m (middle-with-inner e no-owner))
       (jitter!)
       (set-esq-folded! e m)
       (jitter!)
       (set-esq-version! e (unsafe-fx- 0 (unsafe-fxabs v)))
       m]))
  (psek-of-tree (pt-populate-sides (esq-front e) m (esq-back e) 0 no-owner)))

;; Take the snapshot and empty the sequence.  This avoids leaving the two
;; structures sharing chunks, so later updates to the sequence never pay for
;; copy-on-write; it is the cheaper operation when the old contents are not
;; needed.  Being an update, it may move the inner chunks in place, while the
;; sequence still owns them.
(define (esek-snapshot-and-clear! e)
  (check-esek 'esek-snapshot-and-clear! e)
  (esek-flush-inner! e)
  (set-esq-id! e (fresh-id!))
  (define s
    (psek-of-tree
     (pt-populate-sides (esq-front e) (esq-middle e) (esq-back e) 0 no-owner)))
  (esek-clear! e)
  s)

;; edit (§2.1): a fresh ephemeral sequence over the same immutable structure.
(define (psek-edit s)
  (check-psek 'psek-edit s)
  (define id (fresh-id!))
  (define k (capacity-at 0))
  (define r (psek-rep s))
  (cond
    [(not r) (make-esek)]
    [(vector? r)
     (esq id (chunk-of-vector r k unit-measure id) empty-chunk #f empty-chunk
          empty-chunk 1 #f)]
    [else
     (esq id (lvl-front r) empty-chunk (lvl-middle r) empty-chunk (lvl-back r) 1 #f)]))

;; mode 'copy, the default, walks the elements, which costs O(n) but leaves
;; both structures with their own chunks.  Mode 'share leaves the copy sharing
;; chunks with e, which is O(1) now and pays for itself later only if neither
;; sequence is updated much -- and e loses ownership of its chunks as well.
(define (esek-copy e #:mode [mode 'copy])
  (check-esek 'esek-copy e)
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
          (esq-iback e) (esq-back e) 1 #f)]
    ;; pushed one at a time rather than built, so that the copy owns its
    ;; chunks and its own later writes copy nothing
    [(copy)
     (define c (make-esek))
     (esek-for-each e (lambda (x) (esek-add! c x)))
     c]
    [else (raise-argument-error 'esek-copy "(or/c 'share 'copy)" mode)]))

(define (esek-become! e s)
  (define e* (psek-edit s))
  (esek-invalidate-iterators! e)
  (set-esq-id! e (esq-id e*))
  (set-esq-front! e (esq-front e*))
  (set-esq-ifront! e (esq-ifront e*))
  (set-esq-middle! e (esq-middle e*))
  (set-esq-iback! e (esq-iback e*))
  (set-esq-back! e (esq-back e*))
  (void))

;; Move the contents of e2 into e1 and empty e2.
(define (esek-assign! e1 e2)
  (check-esek 'esek-assign! e1)
  (check-esek 'esek-assign! e2)
  (unless (eq? e1 e2)
    (esek-become! e1 (esek-snapshot-and-clear! e2)))
  (void))

;; Append the contents of `other` to `e` at the given end.  As in the OCaml
;; library, an ephemeral `other` is emptied: handing over its representation
;; rather than sharing it is what keeps later updates to either sequence out
;; of the copy-on-write path.  A persistent `other` is of course untouched.
;;
;; Every operation from here to `esek-drop!` empties its argument before it
;; builds the result, so each checks all of its arguments first: an error
;; raised after the emptying would lose the caller's elements.
(define (esek-append! e other [side 'back])
  (check-esek 'esek-append! e)
  (unless (or (esq? other) (psek? other))
    (raise-argument-error 'esek-append! "(or/c esek? psek?)" other))
  (check-side 'esek-append! side)
  (when (eq? e other)
    (raise-arguments-error 'esek-append! "the two sequences must be distinct"))
  (append-esek! 'esek-append! e other side))

;; The append without the argument checks, for callers that have made them.
(define (append-esek! who e other side)
  (define n (if (esq? other) (esek-length other) (psek-length other)))
  (check-append-length who (esek-length e) n)
  (define o (if (esq? other) (esek-snapshot-and-clear! other) other))
  (define self (esek-snapshot-and-clear! e))
  (esek-become! e (if (eq? side 'front)
                      (append-psek who o self)
                      (append-psek who self o))))

;; The concatenation of e1 and e2, as a new sequence; both are emptied.
(define (esek-concat! e1 e2)
  (check-esek 'esek-concat! e1)
  (check-esek 'esek-concat! e2)
  (when (eq? e1 e2)
    (raise-arguments-error 'esek-concat! "the two sequences must be distinct"))
  (check-append-length 'esek-concat! (esek-length e1) (esek-length e2))
  (psek-edit (append-psek 'esek-concat!
                          (esek-snapshot-and-clear! e1)
                          (esek-snapshot-and-clear! e2))))

(define (check-split-index who e i)
  (define n (esek-length e))
  (unless (and (fixnum? i) (unsafe-fx>= i 0) (unsafe-fx<= i n))
    (raise-arguments-error who "index out of range" "index" i "length" n)))

;; Split e at index i into two new sequences; e is emptied.
(define (esek-split! e i)
  (check-esek 'esek-split! e)
  (check-split-index 'esek-split! e i)
  (define-values (s1 s2) (psek-split (esek-snapshot-and-clear! e) i))
  (values (psek-edit s1) (psek-edit s2)))

;; Split e at index i, keeping one part in e and returning the other:
;; 'back keeps the front part, 'front keeps the back part.
(define (esek-carve! e i [side 'back])
  (check-esek 'esek-carve! e)
  (check-split-index 'esek-carve! e i)
  (check-side 'esek-carve! side)
  (define-values (s1 s2) (psek-split (esek-snapshot-and-clear! e) i))
  (cond
    [(eq? side 'back) (esek-become! e s1) (psek-edit s2)]
    [else (esek-become! e s2) (psek-edit s1)]))

;; Truncate e at index i, keeping the front part ('front) or the back part
;; ('back).
(define (esek-take! e i [side 'front])
  (check-esek 'esek-take! e)
  (check-split-index 'esek-take! e i)
  (check-side 'esek-take! side)
  (keep-part! e i side))

(define (esek-drop! e i [side 'front])
  (check-esek 'esek-drop! e)
  (check-split-index 'esek-drop! e i)
  (check-side 'esek-drop! side)
  (keep-part! e i (if (eq? side 'front) 'back 'front)))

;; Only one side is kept, so only one side is built -- psek-take and psek-drop
;; are the specialized splits.
(define (keep-part! e i side)
  (define s (esek-snapshot-and-clear! e))
  (esek-become! e (if (eq? side 'front) (psek-take s i) (psek-drop s i))))

(define (esek-for-each e proc)
  (check-esek 'esek-for-each e)
  (check-procedure 'esek-for-each proc 1)
  (define birth (esek-iterator-born! e))
  (define checking? (check-iterator-validity?))
  (define (call x)
    (when (and checking? (not (esek-unchanged? e birth)))
      (raise-updated 'esek-for-each))
    (proc x))
  (define (chunk-elems c)
    (for ([j (in-fx-range (chunk-length c))])
      (call (chunk-ref c j))))
  (chunk-elems (esq-front e))
  (chunk-elems (esq-ifront e))
  (pt-for-each (esq-middle e) 1 call)
  (chunk-elems (esq-iback e))
  (chunk-elems (esq-back e)))

(define (esek->list e)
  (check-esek 'esek->list e)
  (define acc '())
  (esek-for-each e (lambda (x) (set! acc (cons x acc))))
  (reverse acc))

(define (esek->vector e)
  (check-esek 'esek->vector e)
  (define v (make-vector (esek-length e) #f))
  (define i 0)
  (esek-for-each e
                 (lambda (x)
                   (vector-set! v i x)
                   (set! i (add1 i))))
  v)

;; The elements as a Racket sequence: what `for` gets when it is handed an
;; ephemeral sequence directly, and what equality and hashing walk.
(define (elements e)
  (define f (sek-sequencer))
  (if f (f 'in-esek e) (in-esek e)))

;; The fallback: a reader over the five parts.  Like an iterator, it raises
;; when the sequence has been updated since the traversal began; the reader
;; alone would go on reading the old chunks.
(define (in-esek e)
  (reader->sequence
   (lambda ()
     (make-reader (list (cons (esq-front e) 0)
                        (cons (esq-ifront e) 0)
                        (cons (esq-middle e) 1)
                        (cons (esq-iback e) 0)
                        (cons (esq-back e) 0))))
   (lambda ()
     (define birth (esek-iterator-born! e))
     (lambda ()
       (unless (esek-iterator-valid? e birth)
         (raise-updated 'in-esek))))))

(module+ deserialize
  (provide deserialize-esek)
  (define deserialize-esek
    (make-deserialize-info (lambda (v)
                             (esek-build (vector-length v) (lambda (i) (vector-ref v i))))
                           (lambda () (error 'deserialize-esek "cycles not supported")))))

;; Pushed one at a time, which leaves the sequence owning its chunks, and
;; which for a list is as fast as building: there is no index to build by.
(define (list->esek xs)
  (unless (list? xs) (raise-argument-error 'list->esek "list?" xs))
  (define e (make-esek))
  (for ([x (in-list xs)])
    (esek-add! e x))
  e)

;; The push names are what the paper and the OCaml library call these, and are
;; kept so that code written against either goes on working.
(define esek-push-front! esek-cons!)
(define esek-push-back! esek-add!)

;; For tests that store wrong values in the structure on purpose, as a race can
;; (sek-test/tests/sek/iterator-tests.rkt and aufheben/torn.rkt).
(module+ testing
  (provide esq-id esq-front esq-ifront esq-middle esq-iback esq-back esq-version esq-folded
           set-esq-id! set-esq-front! set-esq-ifront! set-esq-middle! set-esq-iback!
           set-esq-back! set-esq-version! set-esq-folded!))
