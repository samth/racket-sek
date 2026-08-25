#lang scribble/manual
@(require (for-label racket/base
                     racket/contract
                     sek))

@title{Sek: Catenable, Splittable, Transient Sequences}

@defmodule[sek]

An implementation of the data structure described by Arthur Charguéraud and
François Pottier in
@hyperlink["https://doi.org/10.1145/3828706"]{@italic{A Catenable, Splittable,
Transient Sequence Data Structure}}, Proc. ACM Program. Lang. 10, ICFP,
Article 308 (August 2026).  Section numbers below refer to that paper.

A @deftech{transient} data structure combines an ephemeral data structure, a
persistent one, and fast conversions between them.  Programs can take a
persistent snapshot in constant time whenever they need one, while keeping the
performance of destructive updates in the stretches of code where persistence
is not needed.

This library provides two such structures:

@itemlist[
 @item{@tech{transient arrays} (§2), which represent fixed-size sequences;}
 @item{@tech{transient sequences} (§3), which additionally support pushing and
       popping at either end, concatenation and splitting.}]

@section{Overview}

@racketblock[
(require sek)

(define p (list->pseq '(1 2 3 4 5)))
(pseq->list (pseq-push-front p 0))   (code:comment "'(0 1 2 3 4 5)")
(pseq->list p)                       (code:comment "'(1 2 3 4 5) -- unchanged")

(define e (pseq-edit p))             (code:comment "O(1): switch to in-place updates")
(eseq-push-back! e 6)
(eseq-set! e 0 'a)
(define q (eseq-snapshot e))         (code:comment "O(1): switch back")
(pseq->list q)                       (code:comment "'(a 2 3 4 5 6)")
(pseq->list p)                       (code:comment "'(1 2 3 4 5) -- still unchanged")
]

@section{Transient sequences}

A @deftech{transient sequence} is stored as a tree whose nodes hold arrays of
up to @math{K} items, called @italic{chunks} (§3.1).  Each level of the tree
consists of a front chunk, a back chunk, and a middle sequence, which is itself
a tree of the same shape one level down, holding chunks of the current level's
items.  Because the two ends of the sequence live at the root, pushing and
popping there is cheap; because the tree is balanced by a density invariant on
the middle sequences, indexing, splitting and concatenation are logarithmic.

Throughout, @math{n} is the length of the sequence, @math{K} the chunk
capacity, and @math{T} the threshold below which a persistent sequence is held
in a plain vector.

@subsection{Persistent sequences}

@defproc[(pseq? [v any/c]) boolean?]{
 Recognizes persistent sequences.  A persistent sequence is also a
 @racket[sequence], and two of them are @racket[equal?] when their elements
 are.}

@defthing[empty-pseq pseq?]{The empty persistent sequence.}

@defproc[(pseq [v any/c] ...) pseq?]{
 Returns a persistent sequence holding the given elements.}

@deftogether[(@defproc[(pseq-empty? [s pseq?]) boolean?]
              @defproc[(pseq-length [s pseq?]) exact-nonnegative-integer?])]{
 Emptiness test and length, both @math{O(1)}.}

@deftogether[(@defproc[(pseq-push-front [s pseq?] [v any/c]) pseq?]
              @defproc[(pseq-push-back [s pseq?] [v any/c]) pseq?])]{
 Return a sequence with @racket[v] added at the given end.  @math{O(K
 log_K n)} in the worst case, and @math{O(1)} when the affected chunk admits a
 monotonic in-place update (§3.3).}

@deftogether[(@defproc[(pseq-pop-front [s pseq?]) (values any/c pseq?)]
              @defproc[(pseq-pop-back [s pseq?]) (values any/c pseq?)])]{
 Return the element at the given end and the rest of the sequence.
 @math{O(log_K n)}, or @math{O(T)} when the result becomes short enough to
 switch to the compact representation.  Raises @racket[exn:fail:contract] if
 @racket[s] is empty.}

@deftogether[(@defproc[(pseq-first [s pseq?]) any/c]
              @defproc[(pseq-last [s pseq?]) any/c])]{
 The element at either end, without removing it.}

@deftogether[(@defproc[(pseq-ref [s pseq?] [i exact-nonnegative-integer?]) any/c]
              @defproc[(pseq-set [s pseq?] [i exact-nonnegative-integer?]
                                 [v any/c]) pseq?])]{
 Random access.  @math{O(K log_K n)} in general and @math{O(log_K n)} when
 every chunk on the path is @italic{packed}, which is the case for any sequence
 built without concatenation (§3.2).}

@defproc[(pseq-append [s1 pseq?] [s2 pseq?]) pseq?]{
 Concatenation, in @math{O(K log_K n + log_K^2 n)}.}

@defproc[(pseq-split [s pseq?] [i exact-nonnegative-integer?])
         (values pseq? pseq?)]{
 Returns the first @racket[i] elements and the rest, in
 @math{O(K log_K n + log_K^2 n)}.}

@deftogether[(@defproc[(pseq->list [s pseq?]) list?]
              @defproc[(list->pseq [xs list?]) pseq?]
              @defproc[(pseq->vector [s pseq?]) vector?]
              @defproc[(vector->pseq [v vector?]) pseq?]
              @defproc[(pseq-for-each [s pseq?] [proc (-> any/c any)]) void?]
              @defproc[(pseq-map [s pseq?] [proc (-> any/c any/c)]) pseq?]
              @defproc[(in-pseq [s pseq?]) sequence?])]{
 Conversion and iteration, all @math{O(n)}.  @racket[in-pseq] walks the tree
 lazily, so consuming only a prefix costs only that prefix.}

@subsection{Ephemeral sequences}

@defproc[(eseq? [v any/c]) boolean?]{
 Recognizes ephemeral sequences.  An ephemeral sequence is also a
 @racket[sequence].}

@deftogether[(@defproc[(make-eseq [n exact-nonnegative-integer? 0]
                                  [v any/c #f]) eseq?]
              @defproc[(eseq [v any/c] ...) eseq?])]{
 Create an ephemeral sequence: empty, or holding @racket[n] copies of
 @racket[v], or holding the given elements.}

@deftogether[(@defproc[(eseq-empty? [e eseq?]) boolean?]
              @defproc[(eseq-length [e eseq?]) exact-nonnegative-integer?])]{
 Emptiness test and length, both @math{O(1)}.}

@deftogether[(@defproc[(eseq-push-front! [e eseq?] [v any/c]) void?]
              @defproc[(eseq-push-back! [e eseq?] [v any/c]) void?]
              @defproc[(eseq-pop-front! [e eseq?]) any/c]
              @defproc[(eseq-pop-back! [e eseq?]) any/c])]{
 Update @racket[e] in place at either end.  The paper's key result (§3.6) is
 that these have amortized cost @math{O(log_K N)}, where @math{N} bounds the
 length the sequence reaches, even though the middle of the structure may
 contain chunks shared with snapshots.  The bound relies on the two
 @italic{inner chunks} held at the root, which stop an alternating series of
 pushes and pops from cascading down the tree on every operation.}

@deftogether[(@defproc[(eseq-first [e eseq?]) any/c]
              @defproc[(eseq-last [e eseq?]) any/c]
              @defproc[(eseq-ref [e eseq?] [i exact-nonnegative-integer?]) any/c]
              @defproc[(eseq-set! [e eseq?] [i exact-nonnegative-integer?]
                                  [v any/c]) void?])]{
 Random access.  @racket[eseq-set!] costs @math{O(K log_K n)}, dropping to
 @math{O(log_K n)} once the chunks along the path are uniquely owned -- which
 is what makes a run of updates at nearby indices cheap (§2.4).}

@defproc[(eseq-append! [e eseq?] [other (or/c eseq? pseq?)]
                       [side (or/c 'front 'back) 'back]) void?]{
 Appends the contents of @racket[other] to @racket[e], in place, at the given
 end.  @racket[other] is left with the same contents.}

@defproc[(eseq-split [e eseq?] [i exact-nonnegative-integer?])
         (values eseq? eseq?)]{
 Returns two new ephemeral sequences holding the first @racket[i] elements and
 the rest.  @racket[e] is left with the same contents.}

@defproc[(eseq-clear! [e eseq?]) void?]{Empties @racket[e].}

@defproc[(eseq-assign! [e1 eseq?] [e2 eseq?]) void?]{
 Moves the contents of @racket[e2] into @racket[e1] and empties @racket[e2].
 Does nothing if the two are the same sequence.}

@deftogether[(@defproc[(eseq->list [e eseq?]) list?]
              @defproc[(list->eseq [xs list?]) eseq?]
              @defproc[(eseq->vector [e eseq?]) vector?]
              @defproc[(eseq-for-each [e eseq?] [proc (-> any/c any)]) void?]
              @defproc[(in-eseq [e eseq?]) sequence?])]{
 Conversion and iteration, all @math{O(n)}.  @racket[in-eseq] walks the tree
 lazily; it does not check for concurrent modification, so do not use it
 across an update to @racket[e] -- @racket[in-sek] is the checked alternative.}

@subsection{Converting between the two flavours}

@defproc[(eseq-snapshot [e eseq?]) pseq?]{
 Returns a persistent sequence with the current contents of @racket[e].
 @racket[e] remains usable and keeps its contents; later updates to it do not
 affect the snapshot.

 Constant time: the conversion installs a fresh ownership identifier on
 @racket[e], and every chunk in the structure thereby stops being recognizable
 as uniquely owned, which silently makes it immutable (§2.4).  The cost of
 re-acquiring ownership is paid later, and only for the chunks that are
 actually written.}

@defproc[(pseq-edit [s pseq?]) eseq?]{
 Returns an ephemeral sequence with the contents of @racket[s], sharing its
 representation.  @racket[s] is unaffected by later updates to the result.
 @math{O(K)}.}

@defproc[(eseq-snapshot-and-clear! [e eseq?]) pseq?]{
 Takes the snapshot and empties @racket[e].  Because nothing is left sharing
 chunks with the result, later updates to @racket[e] never pay for
 copy-on-write; this is the cheaper operation when the old contents are not
 needed.}

@defproc[(eseq-copy [e eseq?] [#:mode mode (or/c 'share 'copy) 'share]) eseq?]{
 An independent ephemeral copy of @racket[e].  In @racket['share] mode the
 copy initially shares its chunks with @racket[e] and they are separated
 lazily, which costs @math{O(K)} now and makes the next update to either
 sequence more expensive; in @racket['copy] mode the elements are copied up
 front, which costs @math{O(n)} and leaves no latent cost.}

@section{Iterators}

An @deftech{iterator} is a cursor into a sequence.  Its position is an integer
in @math{[-1, n]}: the indices in @math{[0, n)} designate elements, and the two
extremes are @italic{sentinels}, one just before the sequence and one just
after.  An iterator that sits on a sentinel is @racket[sek-iter-finished?].

Moving one step costs @math{O(1)} as long as the iterator stays inside one
run of contiguous storage, which is the common case; crossing a chunk or a
level of the tree costs more, but happens only once every @math{K} elements.
This is what makes a full traversal @math{O(n)} where repeated
@racket[pseq-ref] would be @math{O(n log_K n)}.

Iterating an ephemeral sequence is guarded: any update to the sequence
invalidates every iterator on it, and using an invalidated iterator raises an
exception instead of quietly reading stale storage.  The check can be turned
off with @racket[sek-configure!], at which point using an invalidated iterator
is undefined.  Iterators on persistent sequences are never invalidated.

@defproc[(sek-iterator [s (or/c pseq? eseq?)]
                       [dir (or/c 'forward 'backward) 'forward]) sek-iter?]{
 An iterator on the first element of @racket[s], or on the last one if
 @racket[dir] is @racket['backward].  On an empty sequence the result is
 already finished.}

@defproc[(sek-iterator-at-sentinel [s (or/c pseq? eseq?)]
                                   [side (or/c 'front 'back) 'front]) sek-iter?]{
 An iterator on the sentinel just before (or just after) the sequence.}

@defproc[(sek-iter? [v any/c]) boolean?]{Recognizes iterators.}

@deftogether[(@defproc[(sek-iter-sequence [it sek-iter?]) (or/c pseq? eseq?)]
              @defproc[(sek-iter-length [it sek-iter?]) exact-nonnegative-integer?]
              @defproc[(sek-iter-index [it sek-iter?]) exact-integer?]
              @defproc[(sek-iter-finished? [it sek-iter?]) boolean?]
              @defproc[(sek-iter-valid? [it sek-iter?]) boolean?])]{
 The sequence an iterator was made from, its length, the iterator's current
 position, whether that position is a sentinel, and whether the iterator is
 still usable.  All @math{O(1)}.}

@deftogether[(@defproc[(sek-iter-get [it sek-iter?]) any/c]
              @defproc[(sek-iter-get* [it sek-iter?]) any/c])]{
 The element under the iterator.  @racket[sek-iter-get] raises an exception at
 a sentinel; @racket[sek-iter-get*] returns @racket[#f] there.  @math{O(1)}.}

@deftogether[(@defproc[(sek-iter-move! [it sek-iter?]
                                       [dir (or/c 'forward 'backward) 'forward]) void?]
              @defproc[(sek-iter-get-and-move! [it sek-iter?]
                                               [dir (or/c 'forward 'backward) 'forward]) any/c]
              @defproc[(sek-iter-get-and-move*! [it sek-iter?]
                                                [dir (or/c 'forward 'backward) 'forward]) any/c])]{
 Step one element.  Moving off the far sentinel raises an exception.
 @math{O(1)} amortized.}

@deftogether[(@defproc[(sek-iter-jump! [it sek-iter?]
                                       [dir (or/c 'forward 'backward)]
                                       [n exact-nonnegative-integer?]) void?]
              @defproc[(sek-iter-reach! [it sek-iter?] [i exact-integer?]) void?])]{
 Move by @racket[n] elements, or to index @racket[i], which may be @racket[-1]
 or the length of the sequence.  A jump that stays inside the current run is
 @math{O(1)}; otherwise the cost is that of an index lookup.}

@deftogether[(@defproc[(sek-iter-copy [it sek-iter?]) sek-iter?]
              @defproc[(sek-iter-reset! [it sek-iter?]
                                        [dir (or/c 'forward 'backward 'sentinel) 'forward])
                       void?])]{
 @racket[sek-iter-copy] duplicates an iterator, so that the two move
 independently.  @racket[sek-iter-reset!] puts an iterator back where a
 freshly created one would be, which is also how an iterator that was
 invalidated by an update is made usable again.}

@defproc[(sek-iter-check [it sek-iter?]) sek-iter?]{
 Check the iterator's internal invariants and return it.  For testing.}

@subsection{Segments}

A @deftech{segment} is a run of contiguous storage inside the sequence: a
vector, a start index and a length.  An iterator can hand out the whole run it
is sitting on, which lets a caller process @math{K} elements with a tight
vector loop instead of @math{K} iterator steps.  This is how
@racket[sek-fold-left] and the rest of the derived operations are implemented.

A segment is a view into the sequence, not a copy.  It is valid only as long
as the iterator that produced it is, and writing through one writes into the
sequence.

@deftogether[(@defproc[(sek-iter-segment [it sek-iter?]
                                         [dir (or/c 'forward 'backward) 'forward])
                       segment?]
              @defproc[(sek-iter-segment-and-jump! [it sek-iter?]
                                                   [dir (or/c 'forward 'backward) 'forward])
                       segment?])]{
 The elements from the current position to the end of the run, in the given
 direction.  Note that a backward segment still lists its elements in
 sequence order; it is the elements at and before the cursor.
 @racket[sek-iter-segment-and-jump!] additionally moves the iterator past the
 segment, which is how a traversal advances run by run.}

@deftogether[(@defproc[(segment [v vector?] [start exact-nonnegative-integer?]
                                [len exact-nonnegative-integer?]) segment?]
              @defproc[(segment? [v any/c]) boolean?]
              @defproc[(segment-vector [s segment?]) vector?]
              @defproc[(segment-start [s segment?]) exact-nonnegative-integer?]
              @defproc[(segment-length [s segment?]) exact-nonnegative-integer?]
              @defproc[(segment-empty? [s segment?]) boolean?]
              @defproc[(segment-ref [s segment?] [i exact-nonnegative-integer?]) any/c]
              @defproc[(segment-set! [s segment?] [i exact-nonnegative-integer?]
                                     [v any/c]) void?]
              @defproc[(segment-for-each [s segment?] [proc (-> any/c any)]
                                         [dir (or/c 'forward 'backward) 'forward]) void?]
              @defproc[(segment-for-each2 [s1 segment?] [s2 segment?]
                                          [proc (-> any/c any/c any)]
                                          [dir (or/c 'forward 'backward) 'forward]) void?]
              @defproc[(in-segment [s segment?]) sequence?]
              @defproc[(segment->list [s segment?]) list?]
              @defproc[(segment->vector [s segment?]) vector?])]{
 Segments and their accessors.}

@subsection{Writing through an iterator}

@deftogether[(@defproc[(sek-iter-set! [it sek-iter?] [v any/c]) void?]
              @defproc[(sek-iter-writable-segment [it sek-iter?]
                                                  [dir (or/c 'forward 'backward) 'forward])
                       segment?])]{
 Write at the iterator's position, or obtain a segment that may be written
 through.  Both require an iterator on an ephemeral sequence, and both
 invalidate every @italic{other} iterator on that sequence.

 The first write into a chunk that is shared with some snapshot costs
 @math{O(K log_K n)}, because the chunk has to be copied and the iterator
 rebuilt; after that, writes into the same chunk are @math{O(1)}.  A sweep
 that writes every element therefore costs @math{O(n + K log_K n)} rather than
 one tree descent per element.}

@section{Operations on either flavour}

The operations in this section accept a persistent or an ephemeral sequence.
Those that build a new sequence return the same flavour they were given, which
is how the OCaml library's two parallel modules are collapsed into one set of
names here.

@deftogether[(@defproc[(sek? [v any/c]) boolean?]
              @defproc[(sek-length [s sek?]) exact-nonnegative-integer?]
              @defproc[(sek-empty? [s sek?]) boolean?]
              @defproc[(sek-ref [s sek?] [i exact-nonnegative-integer?]) any/c]
              @defproc[(sek-first [s sek?]) any/c]
              @defproc[(sek-last [s sek?]) any/c])]{
 Basic accessors, dispatching on the flavour.}

@subsection{Traversal}

@defproc[(in-sek [s sek?] [dir (or/c 'forward 'backward) 'forward]) sequence?]{
 A @racket[sequence] over the elements.  Unlike @racket[in-pseq] and
 @racket[in-eseq], this goes through a checked iterator, so modifying an
 ephemeral sequence during the loop is detected rather than silently
 producing nonsense.}

@deftogether[(@defproc[(sek-for-each [s sek?] [proc (-> any/c any)]
                                     [dir (or/c 'forward 'backward) 'forward]) void?]
              @defproc[(sek-for-each/index [s sek?] [proc (-> exact-nonnegative-integer? any/c any)]
                                           [dir (or/c 'forward 'backward) 'forward]) void?]
              @defproc[(sek-segments-for-each [s sek?] [proc (-> segment? any)]
                                              [dir (or/c 'forward 'backward) 'forward]) void?])]{
 Apply @racket[proc] to each element, to each index and element, or to each
 run of contiguous storage.  The last is the fastest way to sweep a sequence
 and is what the others are built on.}

@deftogether[(@defproc[(sek-fold-left [s sek?] [proc (-> any/c any/c any/c)]
                                      [init any/c]) any/c]
              @defproc[(sek-fold-right [s sek?] [proc (-> any/c any/c any/c)]
                                       [init any/c]) any/c])]{
 Fold from the left or from the right, in @math{O(n)}.}

@deftogether[(@defproc[(sek->list [s sek?] [dir (or/c 'forward 'backward) 'forward]) list?]
              @defproc[(sek->vector [s sek?]) vector?])]{Conversions.}

@subsection{Searching}

@deftogether[(@defproc[(sek-find [s sek?] [pred (-> any/c any/c)]
                                 [dir (or/c 'forward 'backward) 'forward]) any/c]
              @defproc[(sek-find-index [s sek?] [pred (-> any/c any/c)]
                                       [dir (or/c 'forward 'backward) 'forward])
                       (or/c exact-nonnegative-integer? #f)]
              @defproc[(sek-find-map [s sek?] [proc (-> any/c any/c)]
                                     [dir (or/c 'forward 'backward) 'forward]) any/c]
              @defproc[(sek-for-all? [s sek?] [pred (-> any/c any/c)]) boolean?]
              @defproc[(sek-exists? [s sek?] [pred (-> any/c any/c)]) boolean?]
              @defproc[(sek-member? [v any/c] [s sek?] [same? (-> any/c any/c any/c) equal?])
                       boolean?]
              @defproc[(sek-memq? [v any/c] [s sek?]) boolean?])]{
 Search operations, all of which stop as soon as they can.
 @racket[sek-find] returns @racket[#f] when nothing matches, so use
 @racket[sek-find-index] when an element could itself be @racket[#f].}

@subsection{Building new sequences}

@deftogether[(@defproc[(sek-map [s sek?] [proc (-> any/c any/c)]) sek?]
              @defproc[(sek-map/index [s sek?]
                                      [proc (-> exact-nonnegative-integer? any/c any/c)]) sek?]
              @defproc[(sek-filter [s sek?] [pred (-> any/c any/c)]) sek?]
              @defproc[(sek-filter-map [s sek?] [proc (-> any/c any/c)]) sek?]
              @defproc[(sek-partition [s sek?] [pred (-> any/c any/c)])
                       (values sek? sek?)]
              @defproc[(sek-reverse [s sek?]) sek?]
              @defproc[(sek-append* [s sek?]) sek?]
              @defproc[(sek-append-map [s sek?] [proc (-> any/c sek?)]) sek?])]{
 The usual list-shaped operations, each @math{O(n)} plus the cost of
 @racket[proc].  @racket[sek-append*] concatenates a sequence of sequences.}

@deftogether[(@defproc[(sek-sub [s sek?] [start exact-nonnegative-integer?]
                                [size exact-nonnegative-integer?]) sek?]
              @defproc[(sek-take [s sek?] [n exact-nonnegative-integer?]) sek?]
              @defproc[(sek-drop [s sek?] [n exact-nonnegative-integer?]) sek?]
              @defproc[(sek-copy [s sek?] [#:mode mode (or/c 'share 'copy) 'share]) sek?])]{
 @racket[sek-sub] extracts a slice in @math{O(size + K)}, which beats
 splitting when the slice is short; @racket[sek-take] and @racket[sek-drop]
 split instead, in @math{O(K log_K n + log_K^2 n)}.  None of them modifies
 @racket[s].  @racket[sek-copy] is the identity on a persistent sequence.}

@subsection{Ordering}

@deftogether[(@defproc[(sek-sort [s sek?] [less? (-> any/c any/c any/c)]) sek?]
              @defproc[(sek-uniq [s sek?] [same? (-> any/c any/c any/c) equal?]) sek?]
              @defproc[(sek-merge [s1 sek?] [s2 sek?] [less? (-> any/c any/c any/c)]) sek?])]{
 A stable sort in @math{O(n log n)}; removal of adjacent duplicates, which
 removes all duplicates from a sorted sequence; and a stable merge of two
 sorted sequences.}

@subsection{Two sequences at once}

@deftogether[(@defproc[(sek-for-each2 [s1 sek?] [s2 sek?] [proc (-> any/c any/c any)]
                                      [dir (or/c 'forward 'backward) 'forward]) void?]
              @defproc[(sek-fold-left2 [s1 sek?] [s2 sek?]
                                       [proc (-> any/c any/c any/c any/c)] [init any/c]) any/c]
              @defproc[(sek-fold-right2 [s1 sek?] [s2 sek?]
                                        [proc (-> any/c any/c any/c any/c)] [init any/c]) any/c]
              @defproc[(sek-map2 [s1 sek?] [s2 sek?] [proc (-> any/c any/c any/c)]) sek?]
              @defproc[(sek-zip [s1 sek?] [s2 sek?]) sek?]
              @defproc[(sek-unzip [s sek?]) (values sek? sek?)]
              @defproc[(sek-for-all2? [s1 sek?] [s2 sek?] [pred (-> any/c any/c any/c)]) boolean?]
              @defproc[(sek-exists2? [s1 sek?] [s2 sek?] [pred (-> any/c any/c any/c)]) boolean?]
              @defproc[(sek-equal? [s1 sek?] [s2 sek?]
                                   [same? (-> any/c any/c any/c) equal?]) boolean?]
              @defproc[(sek-compare [s1 sek?] [s2 sek?]
                                    [cmp (-> any/c any/c real?)] ) (or/c -1 0 1)])]{
 Binary operations.  They stop at the end of the shorter sequence, except
 @racket[sek-equal?], which first compares lengths, and @racket[sek-compare],
 which orders a proper prefix before the sequence that extends it.
 @racket[sek-zip] pairs elements with @racket[cons]; @racket[sek-unzip] undoes
 it.}

@subsection{Bulk writes}

@deftogether[(@defproc[(sek-fill! [e eseq?] [start exact-nonnegative-integer?]
                                  [size exact-nonnegative-integer?] [v any/c]) void?]
              @defproc[(sek-blit! [src sek?] [src-start exact-nonnegative-integer?]
                                  [dst eseq?] [dst-start exact-nonnegative-integer?]
                                  [size exact-nonnegative-integer?]) void?])]{
 Overwrite a range with one value, or copy a range from one sequence into
 another.  Both go through writable segments, so they cost
 @math{O(size + K log_K n)} rather than one tree descent per element.
 @racket[sek-blit!] handles the case where @racket[src] and @racket[dst] are
 the same sequence and the ranges overlap.}

@subsection{Construction}

@deftogether[(@defproc[(build-pseq [n exact-nonnegative-integer?]
                                   [proc (-> exact-nonnegative-integer? any/c)]) pseq?]
              @defproc[(build-eseq [n exact-nonnegative-integer?]
                                   [proc (-> exact-nonnegative-integer? any/c)]) eseq?]
              @defproc[(make-pseq [n exact-nonnegative-integer?] [v any/c #f]) pseq?]
              @defproc[(sequence->pseq [s sequence?]) pseq?]
              @defproc[(sequence->eseq [s sequence?]) eseq?])]{
 Build a sequence of @racket[n] elements, or from the elements of any Racket
 @racket[sequence], in @math{O(n + K)}.  See also @racket[make-eseq], which
 takes the same arguments as @racket[make-vector].}

@section{Transient arrays}

A @deftech{transient array} (§2) is a fixed-size sequence with random access.
It is a tree of arity @math{K} in which every node carries an ownership
identifier: when a node's identifier matches that of the ephemeral array being
updated, the node is not shared with anybody and can be written in place;
otherwise it is copied, and the copy becomes uniquely owned.

@deftogether[(@defproc[(parray? [v any/c]) boolean?]
              @defproc[(earray? [v any/c]) boolean?])]{
 Recognize persistent and ephemeral arrays.}

@deftogether[(@defproc[(make-parray [n exact-nonnegative-integer?] [v any/c])
                       parray?]
              @defproc[(make-earray [n exact-nonnegative-integer?] [v any/c])
                       earray?])]{
 An array of @racket[n] copies of @racket[v].  @math{O(n)}.}

@deftogether[(@defproc[(parray-length [a parray?]) exact-nonnegative-integer?]
              @defproc[(earray-length [a earray?]) exact-nonnegative-integer?]
              @defproc[(parray-ref [a parray?]
                                   [i exact-nonnegative-integer?]) any/c]
              @defproc[(earray-ref [a earray?]
                                   [i exact-nonnegative-integer?]) any/c])]{
 Length is @math{O(1)}; indexing is @math{O(log_K n)}.}

@deftogether[(@defproc[(parray-set [a parray?] [i exact-nonnegative-integer?]
                                   [v any/c]) parray?]
              @defproc[(earray-set! [a earray?] [i exact-nonnegative-integer?]
                                    [v any/c]) void?])]{
 Update.  @math{O(K log_K n)} in the worst case.  For an ephemeral array the
 cost falls to @math{O(log_K n)} once the path is uniquely owned, so repeated
 writes at the same or nearby indices are cheap.}

@deftogether[(@defproc[(earray-snapshot [a earray?]) parray?]
              @defproc[(parray-edit [a parray?]) earray?])]{
 Convert between the flavours in @math{O(1)}.  Both arrays remain usable.}

@deftogether[(@defproc[(parray->vector [a parray?]) vector?]
              @defproc[(earray->vector [a earray?]) vector?]
              @defproc[(parray->list [a parray?]) list?]
              @defproc[(earray->list [a earray?]) list?]
              @defproc[(vector->parray [v vector?]) parray?]
              @defproc[(vector->earray [v vector?]) earray?])]{
 Conversions, all @math{O(n)}.}

@section{Configuration}

@defproc[(sek-configure! [#:leaf-capacity k0 (and/c exact-integer? (>=/c 2))]
                         [#:node-capacity k1 (and/c exact-integer? (>=/c 2))]
                         [#:short-threshold t exact-nonnegative-integer?]
                         [#:overwrite-empty-slots? overwrite? any/c]
                         [#:check-iterator-validity? check? any/c])
         void?]{
 The settings of §4.1.  Any argument that is not supplied is left as it is.

 @racket[k0] and @racket[k1] are the chunk capacities used at the leaves and
 at internal nodes, and @racket[t] is the length below which a persistent
 sequence is represented by a plain vector (§3.5).  The defaults are 128, 16
 and 32.

 @racket[overwrite?] controls whether a slot that becomes logically empty is
 overwritten.  Leaving it alone saves one write per pop but lets the garbage
 collector retain a value that the sequence no longer holds; overwriting is
 the default.

 @racket[check?] controls whether the use of an invalidated iterator is
 detected at runtime.  Detection costs a comparison per iterator operation and
 a sign test per update, and is on by default; with it off, using an
 invalidated iterator is undefined rather than an error.

 Call the capacity and threshold settings before building any sequences: a
 structure whose chunks were allocated under different settings will not
 satisfy the invariants that @racket[sek-validate-pseq] checks, and its
 density bounds no longer hold.  Small capacities are chiefly useful for
 testing, where they force deep trees.}

@section{Validation}

@deftogether[(@defproc[(sek-validate-pseq [s pseq?]) pseq?]
              @defproc[(sek-validate-eseq [e eseq?]) eseq?])]{
 Check the structural invariants of a sequence and return it, raising an
 exception describing the first violation found.  This is the runtime
 validation function of Appendix A; the test suite calls it after every
 operation.  It costs @math{O(n)} and is meant for testing, not production
 use.}

@section{Differences from the paper}

This library follows the paper, and where the paper is silent, the authors'
OCaml library @hyperlink["https://gitlab.inria.fr/fpottier/sek/"]{Sek}.  It
differs from them in the following ways.

@itemlist[

 @item{Sequences are parameterized by neither an element type nor a
       @tt{default} value.  Logically empty slots are filled with a private
       sentinel instead, which removes the @tt{default} argument that the
       OCaml library has to thread through every constructor.}

 @item{The two flavours are one set of names rather than two parallel modules:
       an operation that builds a sequence returns the same flavour it was
       given.}

 @item{@racket[eseq-append!] leaves its second argument alone, and
       @racket[eseq-split] leaves its argument alone, where the OCaml library
       clears them.  Concatenation and splitting always work on the persistent
       flavour underneath, going through @racket[eseq-snapshot] and
       @racket[pseq-edit], which are both cheap.}

 @item{The iterator supports the operations of the OCaml library's @tt{ITER}
       and @tt{ITER_EPHEMERAL} signatures, but @racket[sek-iter-reach!] always
       descends from the root, where the OCaml version can start from the
       iterator's current position when the target is nearby.  Nearby jumps
       that stay inside one segment are still @math{O(1)}.}

 @item{The paper's @tt{One} and @tt{Short} constructors for short persistent
       sequences are unified into a single vector representation, and appear
       only at the top of the structure, as in the authors' implementation.}

 @item{@racket[pseq-edit] shares the front and back chunks of the persistent
       sequence instead of copying them, so it is @math{O(1)} rather than
       @math{O(K)}; the copy happens on the first write to each, if there is
       one.  Symmetrically, @racket[eseq-snapshot] does not copy the front and
       back chunks, where the OCaml library's does.}

 @item{@racket[eseq-snapshot] folds the two inner chunks into the middle
       sequence, as the OCaml library does, so it costs @math{O(K log_K n)} in
       the worst case rather than the @math{O(1)} of Figure 16.}

 @item{Like the paper's implementation, monotonic in-place updates make the
       persistent flavour unsafe to share across threads without
       synchronization.}]
