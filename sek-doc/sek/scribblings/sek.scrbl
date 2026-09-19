#lang scribble/manual
@(require scribble/example
          (for-label racket/base
                     racket/contract
                     racket/vector
                     racket/treelist
                     racket/mutable-treelist
                     sek))

@(define the-eval (make-base-eval))
@(the-eval '(require sek))

@title{Sek: Catenable, Splittable, Transient Sequences}

@defmodule[sek]

@author[@author+email["Sam Tobin-Hochstadt" "samth@racket-lang.org"]]

An implementation of the sequence data structure of Charguéraud and Pottier
@cite["Chargueraud26"].

This library provides efficient @tech{persistent sequences} and @tech{ephemeral
sequences}, together with cheap conversions between the two.  Both support
random access, pushing and popping at either end, concatenation and splitting.

Those conversions are why the two flavors belong in one library.  A program
holding a persistent sequence can @racket[pseq-edit] it into an ephemeral one,
update that in place as often as it likes, and @racket[eseq-snapshot] it back.
While it edits, the program neither copies the sequence nor pays for
persistent update.  The paper calls that round trip @deftech{transience}: both
flavors share one representation, and converting hands ownership from one to
the other instead of copying.

@section{Overview}

@examples[
#:eval the-eval
(define p (list->pseq '(1 2 3 4 5)))
(pseq->list (pseq-push-front p 0))
(code:comment "p itself is unchanged")
(pseq->list p)
(code:comment "switch to in-place updates in O(1) ...")
(define e (pseq-edit p))
(eseq-push-back! e 6)
(eseq-set! e 0 'a)
(code:comment "... and back again, also in O(1)")
(define q (eseq-snapshot e))
(pseq->list q)
(pseq->list p)
]

@section{Sequences}

A sequence is a tree whose nodes hold arrays of up to @math{K} items, called
@italic{chunks}.  Each level holds a front chunk, a back chunk, and a middle
sequence, itself a tree of the same shape one level down whose chunks hold the
current level's items.  Because the two ends live at the root, pushing and
popping there is cheap; because a density invariant on the middle sequences
keeps the tree balanced, indexing, splitting and concatenation are
logarithmic.

Throughout, @math{N} is the length of the sequence, @math{K} the chunk
capacity, and @math{T} the threshold below which a persistent sequence is held
in a plain vector.  Unless otherwise specified, operations on a sequence of
length @math{N} take @math{O(log_K N)} time.  As for @tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{treelists}, the base of the
@math{log} is large enough that it is effectively constant-time for many
purposes: with the default @math{K} of 128 at the leaves, a sequence of a
million elements is three levels deep.

@section{Comparison with treelists}

Racket's @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{treelists}
solve a similar problem, and for most programs they are the better choice:
they are in the core and they are simpler. Both structures support random
access, concatenation and splitting in @math{O(log N)} time, with a base large
enough that the logarithm is effectively a constant.

The two differ at the ends and in the conversions. Pushing or popping at
either end of an @tech{ephemeral sequence} is @math{O(1)} amortized, where the
corresponding treelist operation takes @math{O(log N)} time.

The conversions differ more sharply. @racket[treelist-copy] and
@racket[mutable-treelist-snapshot] each take @math{O(N)} time, so a program
that moves between the immutable and mutable forms pays for the whole sequence
at every switch. Here @racket[pseq-edit] takes @math{O(1)} time and
@racket[eseq-snapshot] takes @math{O(K log_K N)} time, so a loop can move back
and forth. Likewise @racket[mutable-treelist-append!] takes @math{O(N)} time
in the length of its second argument, where @racket[eseq-append!] does not.

Traversal is @math{O(N)} for both. This library also hands out @tech{segments},
a run of the sequence's own storage that a caller can process with a vector
loop instead of one cursor step per element.

Treelists are RRB trees @cite["Stucki15"], which store one element per leaf
slot. The sequences here store chunks of up to @math{K} elements and keep
track of who owns each chunk. Chunks and ownership together make the ends and
the conversions cheap, and they put the @math{K} into the bounds above.

@subsection{Persistent sequences}

A @deftech{persistent sequence} is immutable: an operation on one produces a
new sequence and leaves the original intact.

A persistent sequence can be used as a single-valued @racket[sequence], whose
elements are the elements of the sequence; see also @racket[in-pseq].  It can
also be used as a @tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{stream}, and it is
@racket[serializable?].  Two persistent sequences are @racket[equal?] when
their elements are.

@defproc[(pseq? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{persistent sequence},
@racket[#f] otherwise.}

@defproc[(pseq [v any/c] ...) pseq?]{

Returns a @tech{persistent sequence} with @racket[v]s as its elements in
order.

@examples[
#:eval the-eval
(pseq 1 "a" 'apple)
]}

@deftogether[(
@defproc[(pseq-empty? [s pseq?]) boolean?]
@defthing[empty-pseq (and/c pseq? pseq-empty?)]
)]{

A predicate and constant for a @tech{persistent sequence} of length 0.}

@defproc[(pseq-length [s pseq?]) exact-nonnegative-integer?]{

Returns the number of elements in @racket[s].  This operation takes
@math{O(1)} time.

@examples[
#:eval the-eval
(pseq-length (pseq 1 "a" 'apple))
]}

@deftogether[(
@defproc[(pseq-add [s pseq?] [v any/c]) pseq?]
@defproc[(pseq-cons [s pseq?] [v any/c]) pseq?]
@defproc[(pseq-push-back [s pseq?] [v any/c]) pseq?]
@defproc[(pseq-push-front [s pseq?] [v any/c]) pseq?]
)]{
 Return a @tech{persistent sequence} with @racket[v] added at the end
 (@racket[pseq-add]) or at the front (@racket[pseq-cons]), the same division of
 labor as @racket[treelist-add] and @racket[treelist-cons].
 @racket[pseq-push-back] and @racket[pseq-push-front] are aliases, under the
 names the paper and the authors' OCaml library use.

 These take @math{O(K log_K N)} time in the worst case, and @math{O(1)} time
 when the affected chunk admits a monotonic in-place update.

 @examples[
 #:eval the-eval
 (define s (pseq 1 2 3))
 (pseq-cons s 0)
 (pseq-add s 4)
 s
 ]}

@deftogether[(@defproc[(pseq-pop-front [s pseq?]) (values any/c pseq?)]
              @defproc[(pseq-pop-back [s pseq?]) (values any/c pseq?)])]{
 Return the element at the given end and the rest of the sequence.
 These operations take @math{O(log_K N)} time, or @math{O(T)} time when the
 result becomes short enough to switch to the compact representation.  Raises
 @racket[exn:fail:contract] if @racket[s] is empty.}

@deftogether[(@defproc[(pseq-first [s pseq?]) any/c]
              @defproc[(pseq-last [s pseq?]) any/c])]{
 Shorthands for using @racket[pseq-ref] to access the first or last element
 of a @tech{persistent sequence}.}

@deftogether[(@defproc[(pseq-ref [s pseq?] [i exact-nonnegative-integer?]) any/c]
              @defproc[(pseq-set [s pseq?] [i exact-nonnegative-integer?]
                                 [v any/c]) pseq?])]{
 Returns the @racket[i]th element of @racket[s], or a sequence with that
 element replaced by @racket[v].  The first element is position @racket[0],
 and the last position is one less than @racket[(pseq-length s)].

 These operations take @math{O(K log_K N)} time in general, and
 @math{O(log_K N)} time when every chunk on the path is @italic{packed}, which
 is the case for any sequence built without concatenation.

 @examples[
 #:eval the-eval
 (define s (list->pseq '(a b c d)))
 (pseq-ref s 2)
 (pseq->list (pseq-set s 2 'C))
 (pseq->list s)
 ]}

@defproc[(pseq-append [s1 pseq?] [s2 pseq?]) pseq?]{
 Returns a @tech{persistent sequence} with the elements of @racket[s1]
 followed by those of @racket[s2], in @math{O(K log_K N + log_K^2 N)}.

 @examples[
 #:eval the-eval
 (pseq->list (pseq-append (pseq 1 2) (pseq 3 4)))
 ]}

@defproc[(pseq-split [s pseq?] [i exact-nonnegative-integer?])
         (values pseq? pseq?)]{
 Returns the first @racket[i] elements and the rest, in
 @math{O(K log_K N + log_K^2 N)}.

 @examples[
 #:eval the-eval
 (define-values (before after) (pseq-split (list->pseq '(a b c d e)) 2))
 (pseq->list before)
 (pseq->list after)
 ]}

@deftogether[(@defproc[(pseq-take [s pseq?] [i exact-nonnegative-integer?]) pseq?]
              @defproc[(pseq-drop [s pseq?] [i exact-nonnegative-integer?]) pseq?])]{
 The two halves of @racket[pseq-split] separately: the first @racket[i]
 elements, or all but the first @racket[i].  Same cost as
 @racket[pseq-split], and @racket[s] is unchanged.

 @examples[
 #:eval the-eval
 (define s (list->pseq '(a b c d e)))
 (pseq->list (pseq-take s 2))
 (pseq->list (pseq-drop s 2))
 ]}

@deftogether[(@defproc[(pseq->list [s pseq?]) list?]
              @defproc[(list->pseq [xs list?]) pseq?]
              @defproc[(pseq->vector [s pseq?]) vector?]
              @defproc[(vector->pseq [v vector?]) pseq?]
              @defproc[(pseq-for-each [s pseq?] [proc (-> any/c any)]) void?]
              @defproc[(pseq-map [s pseq?] [proc (-> any/c any/c)]) pseq?])]{
 Conversion and iteration.  Each of these takes @math{O(N)} time.  See
 @racket[in-pseq] below for
 iterating in a @racket[for] clause.}

@subsection{Ephemeral sequences}

An @deftech{ephemeral sequence} changes in place.  Where an operation on a
@tech{persistent sequence} returns a new sequence, the corresponding operation
here modifies the sequence it is given and returns @racket[void].

An ephemeral sequence can be used as a single-valued @racket[sequence]; see
also @racket[in-eseq].  It is @racket[serializable?], and two ephemeral
sequences are @racket[equal?] when their elements are.  It is not a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{stream}, for the
same reason a @racket[mutable-treelist] is not: a stream's rest is a value,
and this one changes in place.

@defproc[(eseq? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an @tech{ephemeral sequence},
@racket[#f] otherwise.}

@defproc[(eseq [v any/c] ...) eseq?]{

Returns an @tech{ephemeral sequence} with @racket[v]s as its elements in
order.

@examples[
#:eval the-eval
(eseq 1 "a" 'apple)
]}

@defproc[(make-eseq [n exact-nonnegative-integer? 0] [v any/c #f]) eseq?]{

Returns an @tech{ephemeral sequence} of length @racket[n], where every element
is @racket[v].

@examples[
#:eval the-eval
(make-eseq 0)
(make-eseq 3 'pear)
]}

@defproc[(eseq-empty? [e eseq?]) boolean?]{

Returns @racket[#t] if @racket[e] has no elements, @racket[#f] otherwise.
This operation takes @math{O(1)} time.}

@defproc[(eseq-length [e eseq?]) exact-nonnegative-integer?]{

Returns the number of elements in @racket[e].  This operation takes
@math{O(1)} time.

@examples[
#:eval the-eval
(eseq-length (eseq 1 "a" 'apple))
]}

@deftogether[(
@defproc[(eseq-add! [e eseq?] [v any/c]) void?]
@defproc[(eseq-cons! [e eseq?] [v any/c]) void?]
@defproc[(eseq-push-back! [e eseq?] [v any/c]) void?]
@defproc[(eseq-push-front! [e eseq?] [v any/c]) void?]
@defproc[(eseq-pop-back! [e eseq?]) any/c]
@defproc[(eseq-pop-front! [e eseq?]) any/c]
)]{
 Adds @racket[v] at the end (@racket[eseq-add!]) or the front
 (@racket[eseq-cons!]) of @racket[e], or removes and returns the element at
 one of its ends, modifying @racket[e] in place.  @racket[eseq-push-back!] and
 @racket[eseq-push-front!] are aliases, under the names the paper uses.

 These take amortized @math{O(log_K N)} time even though the middle of the
 structure may contain chunks shared with snapshots, which is the paper's main
 result.  The bound rests on the two @italic{inner chunks} held at the root,
 which stop an alternating series of pushes and pops from cascading down the
 tree on every operation.

 @examples[
 #:eval the-eval
 (define items (eseq 1 2 3))
 (eseq-cons! items 0)
 (eseq-add! items 4)
 items
 (eseq-pop-front! items)
 (eseq-pop-back! items)
 items
 ]}

@deftogether[(
@defproc[(eseq-ref [e eseq?] [i exact-nonnegative-integer?]) any/c]
@defproc[(eseq-set! [e eseq?] [i exact-nonnegative-integer?] [v any/c]) void?]
)]{

Returns the @racket[i]th element of @racket[e], or replaces it with
@racket[v].  The first element is position @racket[0], and the last position
is one less than @racket[(eseq-length e)].

@racket[eseq-set!] takes @math{O(K log_K N)} time, dropping to
@math{O(log_K N)} once the chunks along the path are uniquely owned, which is
what makes a run of updates at nearby indices cheap.

@examples[
#:eval the-eval
(define items (eseq 1 "a" 'apple))
(eseq-ref items 2)
(eseq-set! items 2 'pear)
items
]}

@deftogether[(
@defproc[(eseq-first [e eseq?]) any/c]
@defproc[(eseq-last [e eseq?]) any/c]
)]{

Shorthands for using @racket[eseq-ref] to access the first or last element of
an @tech{ephemeral sequence}.}

The five operations that follow rearrange ephemeral sequences in place, and
they @italic{consume} the sequences they are given, leaving each empty.  The
reference library does the same, for a good reason: handing over a sequence's
representation instead of sharing it keeps later updates out of the
copy-on-write path.  Use @racket[sek-take], @racket[sek-drop] and
@racket[sek-sub] when the input must survive.

@defproc[(eseq-append! [e eseq?] [other (or/c eseq? pseq?)]
                       [side (or/c 'front 'back) 'back]) void?]{
 Appends the contents of @racket[other] to @racket[e], in place, at the given
 end.  This empties an ephemeral @racket[other] and leaves a persistent one
 untouched.  The two sequences must be distinct.}

@defproc[(eseq-concat! [e1 eseq?] [e2 eseq?]) eseq?]{
 Returns a new sequence holding the concatenation, and empties both arguments,
 which must be distinct.}

@defproc[(eseq-split! [e eseq?] [i exact-nonnegative-integer?])
         (values eseq? eseq?)]{
 Returns two new sequences holding the first @racket[i] elements and the rest,
 and empties @racket[e].}

@defproc[(eseq-carve! [e eseq?] [i exact-nonnegative-integer?]
                      [side (or/c 'front 'back) 'back]) eseq?]{
 Splits @racket[e] at @racket[i], keeping one part in @racket[e] and returning
 the other: @racket['back] keeps the front part, @racket['front] keeps the
 back part.  Cheaper than @racket[eseq-split!] when one part is going back
 into the same variable.}

@deftogether[(@defproc[(eseq-take! [e eseq?] [i exact-nonnegative-integer?]
                                   [side (or/c 'front 'back) 'front]) void?]
              @defproc[(eseq-drop! [e eseq?] [i exact-nonnegative-integer?]
                                   [side (or/c 'front 'back) 'front]) void?])]{
 Truncate @racket[e] at index @racket[i].  @racket[eseq-take!] keeps the front
 part when @racket[side] is @racket['front] and the back part otherwise;
 @racket[eseq-drop!] keeps the other one.}

@defproc[(eseq-clear! [e eseq?]) void?]{Empties @racket[e].}

@defproc[(eseq-assign! [e1 eseq?] [e2 eseq?]) void?]{
 Moves the contents of @racket[e2] into @racket[e1] and empties @racket[e2].
 Does nothing if the two are the same sequence.}

@deftogether[(@defproc[(eseq->list [e eseq?]) list?]
              @defproc[(list->eseq [xs list?]) eseq?]
              @defproc[(eseq->vector [e eseq?]) vector?]
              @defproc[(eseq-for-each [e eseq?] [proc (-> any/c any)]) void?])]{
 Conversion and iteration.  Each of these takes @math{O(N)} time.  See
 @racket[in-eseq] below for
 iterating in a @racket[for] clause.}

@deftogether[(
@defproc[(eseq-fill! [e eseq?] [v any/c]
                     [start exact-nonnegative-integer? 0]
                     [end exact-nonnegative-integer? (eseq-length e)]) void?]
@defproc[(eseq-copy! [dst eseq?] [dst-start exact-nonnegative-integer?]
                     [src sek?]
                     [src-start exact-nonnegative-integer? 0]
                     [src-end exact-nonnegative-integer? (sek-length src)]) void?]
)]{

Change the elements of an @tech{ephemeral sequence} in place: @racket[eseq-fill!]
sets those from @racket[start] to @racket[end] to @racket[v], and
@racket[eseq-copy!] sets those starting at @racket[dst-start] to match the
elements of @racket[src] from @racket[src-start] to @racket[src-end].  They
take the same arguments in the same order as @racket[vector-fill!] and
@racket[vector-copy!], and @racket[eseq-copy!] stands to @racket[eseq-copy] as
@racket[vector-copy!] stands to @racket[vector-copy].

@racket[src] may be of either flavor; only the destination is modified.
@racket[eseq-copy!] handles the case where @racket[src] and @racket[dst] are
the same sequence and the ranges overlap.

Both go through writable segments, so they cost @math{O(size + K log_K N)}
time rather than one tree descent per element.

@examples[
#:eval the-eval
(define items (eseq 1 2 3 4 5))
(eseq-fill! items 'x 1 3)
items
(eseq-copy! items 0 (pseq 'a 'b))
items
]}

@subsection{Converting between the two flavors}

@defproc[(eseq-snapshot [e eseq?]) pseq?]{
 Returns a @tech{persistent sequence} with the current contents of
 @racket[e].
 @racket[e] remains usable and keeps its contents; later updates to it do not
 affect the snapshot.

 This operation takes @math{O(K log_K N)} time in the worst case: the two
 inner chunks are folded into the middle sequence first, and only then does the
 conversion install a fresh ownership identifier on @racket[e], which makes
 every chunk in the structure stop being recognizable as uniquely owned and so
 silently immutable.  The cost of re-acquiring ownership is paid later,
 and only for the chunks that are actually written.  Compare
 @racket[mutable-treelist-snapshot], which takes @math{O(N)} time.

 @examples[
 #:eval the-eval
 (define e (list->eseq '(1 2 3)))
 (define snap (eseq-snapshot e))
 (eseq-push-back! e 4)
 (eseq->list e)
 (code:comment "the snapshot does not see the push")
 (pseq->list snap)
 ]}

@defproc[(pseq-edit [s pseq?]) eseq?]{
 Returns an @tech{ephemeral sequence} with the contents of @racket[s], sharing
 its representation.  @racket[s] is unaffected by later updates to the result.

 This operation takes @math{O(1)} time: the front and back chunks are shared
 rather than copied, and a chunk is copied only on the first write to it.
 Compare @racket[treelist-copy], which takes @math{O(N)} time.

 @examples[
 #:eval the-eval
 (define s (pseq 1 2 3))
 (define e (pseq-edit s))
 (eseq-set! e 0 'changed)
 (eseq->list e)
 (pseq->list s)
 ]}

@defproc[(eseq-snapshot-and-clear! [e eseq?]) pseq?]{
 Takes the snapshot and empties @racket[e].  Because nothing is left sharing
 chunks with the result, later updates to @racket[e] never pay for
 copy-on-write; this is the cheaper operation when the old contents are not
 needed.}

@defproc[(eseq-copy [e eseq?] [#:mode mode (or/c 'share 'copy) 'share]) eseq?]{
 An independent ephemeral copy of @racket[e].  In @racket['share] mode the two
 sequences start out sharing everything and are separated lazily by whichever
 one writes first, which is @math{O(1)} now and makes the next update to
 either sequence more expensive; in @racket['copy] mode the elements are
 copied up front, which costs @math{O(N)} and leaves no latent cost.}

@section{Iterators}

An @deftech{iterator} is a cursor into a sequence.  Its position is an integer
in @math{[-1, N]}: the indices in @math{[0, N)} designate elements, and the two
extremes are @italic{sentinels}, one just before the sequence and one just
after.  An iterator that sits on a sentinel is @racket[sek-iter-finished?].

Moving one step costs @math{O(1)} as long as the iterator stays inside one
run of contiguous storage, which is the common case; crossing a chunk or a
level of the tree costs more, but happens only once every @math{K} elements.
A full traversal therefore costs @math{O(N)}, where repeated
@racket[pseq-ref] would cost @math{O(N log_K N)}.

Iterating an ephemeral sequence is checked: any update to the sequence
invalidates every iterator on it, and an invalidated iterator raises an
exception rather than quietly reading stale storage. @racket[sek-configure!]
turns the check off, after which an invalidated iterator is undefined.
Iterators on persistent sequences are never invalidated.

@defproc[(sek-iterator [s (or/c pseq? eseq?)]
                       [dir (or/c 'forward 'backward) 'forward]) sek-iter?]{
 An iterator on the first element of @racket[s], or on the last one if
 @racket[dir] is @racket['backward].  On an empty sequence the result is
 already finished.}

@defproc[(sek-iterator-at-sentinel [s (or/c pseq? eseq?)]
                                   [side (or/c 'front 'back) 'front]) sek-iter?]{
 An iterator on the sentinel just before (or just after) the sequence.}

@defproc[(sek-iter? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is an @tech{iterator}, @racket[#f]
 otherwise.}

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
 a sentinel; @racket[sek-iter-get*] returns @racket[#f] there.  @math{O(1)}.

 Throughout this section, a name ending in @tt{*} is the variant that returns
 @racket[#f] at a sentinel instead of raising -- which is usually what a
 traversal loop wants, since reaching a sentinel is how it ends.}

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
vector, a start index and a length. An iterator can hand out the whole run it
is sitting on, which lets a caller process @math{K} elements with a tight
vector loop instead of @math{K} iterator steps. @racket[sek-fold-left] and the
rest of the derived operations all work this way.

A segment is a view into the sequence, not a copy.  It is valid only as long
as the iterator that produced it is, and writing through one writes into the
sequence.

@deftogether[(@defproc[(sek-iter-segment [it sek-iter?]
                                         [dir (or/c 'forward 'backward) 'forward])
                       segment?]
              @defproc[(sek-iter-segment* [it sek-iter?]
                                          [dir (or/c 'forward 'backward) 'forward])
                       (or/c segment? #f)]
              @defproc[(sek-iter-segment-and-jump! [it sek-iter?]
                                                   [dir (or/c 'forward 'backward) 'forward])
                       segment?]
              @defproc[(sek-iter-segment-and-jump*! [it sek-iter?]
                                                    [dir (or/c 'forward 'backward) 'forward])
                       (or/c segment? #f)])]{
 The elements from the current position to the end of the run, in the given
 direction.  Note that a backward segment still lists its elements in
 sequence order; it is the elements at and before the cursor.
 @racket[sek-iter-segment-and-jump!] additionally moves the iterator past the
 segment, which is how a traversal advances run by run.}

@deftogether[(@defproc[(segment [v vector?] [start exact-nonnegative-integer?]
                                [len exact-nonnegative-integer?]) segment?]
              @defproc[(segment? [v any/c]) boolean?]
              @defproc[(segment-valid? [s any/c]) boolean?]
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
              @defproc[(sek-iter-set-and-move! [it sek-iter?] [v any/c]
                                               [dir (or/c 'forward 'backward) 'forward]) void?]
              @defproc[(sek-iter-writable-segment [it sek-iter?]
                                                  [dir (or/c 'forward 'backward) 'forward])
                       segment?]
              @defproc[(sek-iter-writable-segment* [it sek-iter?]
                                                   [dir (or/c 'forward 'backward) 'forward])
                       (or/c segment? #f)]
              @defproc[(sek-iter-writable-segment-and-jump! [it sek-iter?]
                                                            [dir (or/c 'forward 'backward) 'forward])
                       segment?]
              @defproc[(sek-iter-writable-segment-and-jump*! [it sek-iter?]
                                                             [dir (or/c 'forward 'backward) 'forward])
                       (or/c segment? #f)])]{
 Write at the iterator's position, or obtain a writable segment.  Both
 require an iterator on an ephemeral sequence, and both
 invalidate every @italic{other} iterator on that sequence.

 The first write into a chunk that is shared with some snapshot costs
 @math{O(K log_K N)}, because that write copies the chunk and rebuilds the
 iterator; after that, writes into the same chunk are @math{O(1)}.  A sweep
 that writes every element therefore costs @math{O(N + K log_K N)} rather than
 one tree descent per element.}

@section{Operations on either flavor}

The operations in this section accept a persistent or an ephemeral sequence.
Those that build a new sequence return the same flavor they were given,
collapsing the OCaml library's two parallel modules into one set of names.

@deftogether[(@defproc[(sek? [v any/c]) boolean?]
              @defproc[(sek-length [s sek?]) exact-nonnegative-integer?]
              @defproc[(sek-empty? [s sek?]) boolean?]
              @defproc[(sek-ref [s sek?] [i exact-nonnegative-integer?]) any/c]
              @defproc[(sek-first [s sek?]) any/c]
              @defproc[(sek-last [s sek?]) any/c])]{
 Basic accessors, dispatching on the flavor.}

@subsection{Traversal}

@deftogether[(@defform*[((in-sek s) (in-sek s dir))]
              @defform*[((in-pseq s) (in-pseq s dir))]
              @defform*[((in-eseq e) (in-eseq e dir))])]{
 Sequences over the elements, in @racket['forward] order by default. Written
 directly in a @racket[for] clause these expand to a loop over the sequence's
 own storage, so a step is a vector reference and an increment; used as
 ordinary values they fall back to a checked iterator. Either way, the loop
 detects an update to an ephemeral sequence rather than silently producing
 nonsense.}

@deftogether[(@defproc[(sek-for-each [s sek?] [proc (-> any/c any)]
                                     [dir (or/c 'forward 'backward) 'forward]) void?]
              @defproc[(sek-for-each/index [s sek?] [proc (-> exact-nonnegative-integer? any/c any)]
                                           [dir (or/c 'forward 'backward) 'forward]) void?]
              @defproc[(sek-segments-for-each [s sek?] [proc (-> segment? any)]
                                              [dir (or/c 'forward 'backward) 'forward]) void?]
              @defproc[(sek-segments-for-each2 [s1 sek?] [s2 sek?]
                                               [proc (-> segment? segment? any)]
                                               [dir (or/c 'forward 'backward) 'forward]) void?])]{
 Apply @racket[proc] to each element, to each index and element, or to each
 run of contiguous storage.  The last is the fastest way to sweep a sequence
 and is what the others are built on.}

@deftogether[(@defproc[(sek-fold-left [s sek?] [proc (-> any/c any/c any/c)]
                                      [init any/c]) any/c]
              @defproc[(sek-fold-right [s sek?] [proc (-> any/c any/c any/c)]
                                       [init any/c]) any/c])]{
 Fold from the left or from the right.  These operations take @math{O(N)}
 time.}

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
              @defproc[(sek-member? [s sek?] [v any/c]
                                    [same? (-> any/c any/c any/c) equal?])
                       boolean?]
              @defproc[(sek-memq? [s sek?] [v any/c]) boolean?])]{
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
 The usual list-shaped operations, each @math{O(N)} plus the cost of
 @racket[proc].  @racket[sek-append*] concatenates a sequence of sequences;
 given an ephemeral one it empties both it and its elements, as the reference
 library's @tt{flatten} does, because it hands over each sequence's
 representation rather than copying its elements.}

@deftogether[(@defproc[(sek-sub [s sek?] [start exact-nonnegative-integer?]
                                [size exact-nonnegative-integer?]) sek?]
              @defproc[(sek-take [s sek?] [n exact-nonnegative-integer?]) sek?]
              @defproc[(sek-drop [s sek?] [n exact-nonnegative-integer?]) sek?]
              @defproc[(sek-copy [s sek?] [#:mode mode (or/c 'share 'copy) 'share]) sek?])]{
 @racket[sek-sub] extracts a slice in @math{O(size + K)}, which beats
 splitting when the slice is short; @racket[sek-take] and @racket[sek-drop]
 split instead, in @math{O(K log_K N + log_K^2 N)}.  None of them modifies
 @racket[s].  @racket[sek-copy] is the identity on a persistent sequence.}

@deftogether[(
@defproc[(sek-take-right [s sek?] [n exact-nonnegative-integer?]) sek?]
@defproc[(sek-drop-right [s sek?] [n exact-nonnegative-integer?]) sek?]
)]{
 Produce a sequence like @racket[s] but with only the last @racket[n]
 elements, or without the last @racket[n] elements, respectively.  They cost
 what @racket[sek-take] and @racket[sek-drop] cost, and neither modifies
 @racket[s].

 @examples[
 #:eval the-eval
 (sek-take-right (pseq 1 2 3 4 5) 2)
 (sek-drop-right (pseq 1 2 3 4 5) 2)
 ]}

@deftogether[(
@defproc[(sek-insert [s sek?] [i exact-nonnegative-integer?] [v any/c]) sek?]
@defproc[(sek-delete [s sek?] [i exact-nonnegative-integer?]) sek?]
)]{
 Produce a sequence like @racket[s], except that @racket[v] is inserted before
 the element at @racket[i], or that the element at @racket[i] is removed.  If
 @racket[i] is @racket[(sek-length s)] then @racket[sek-insert] adds
 @racket[v] at the end.

 Each goes through a split and a concatenation rather than rebuilding the
 sequence, so each takes @math{O(K log_K N + log_K^2 N)} time.  Neither
 modifies @racket[s].

 @examples[
 #:eval the-eval
 (sek-insert (pseq 1 2 3) 1 'x)
 (sek-insert (pseq 1 2 3) 3 'x)
 (sek-delete (pseq 1 2 3) 1)
 ]}

@defproc[(sek-index-of [s sek?] [v any/c]
                       [same? (-> any/c any/c any/c) equal?])
         (or/c exact-nonnegative-integer? #f)]{
 Returns the index of the first element of @racket[s] that is @racket[same?]
 to @racket[v], or @racket[#f] if there is none.  @racket[same?] receives
 @racket[v] first and the element second.

 @examples[
 #:eval the-eval
 (sek-index-of (pseq 'a 'b 'c) 'b)
 (sek-index-of (pseq 'a 'b 'c) 'z)
 ]}

@subsection{Ordering}

@deftogether[(@defproc[(sek-sort [s sek?] [less? (-> any/c any/c any/c)]) sek?]
              @defproc[(sek-uniq [s sek?] [same? (-> any/c any/c any/c) equal?]) sek?]
              @defproc[(sek-merge [s1 sek?] [s2 sek?] [less? (-> any/c any/c any/c)]) sek?])]{
 A stable sort, which takes @math{O(N log N)} time; a pass that drops
 adjacent duplicates, and so drops every duplicate from a sorted sequence; and
 a stable merge of two sorted sequences.}

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

@subsection{Construction}

@deftogether[(@defproc[(build-pseq [n exact-nonnegative-integer?]
                                   [proc (-> exact-nonnegative-integer? any/c)]) pseq?]
              @defproc[(build-eseq [n exact-nonnegative-integer?]
                                   [proc (-> exact-nonnegative-integer? any/c)]) eseq?]
              @defproc[(make-pseq [n exact-nonnegative-integer?] [v any/c #f]) pseq?]
              @defproc[(sequence->pseq [s sequence?]
                                       [n (or/c exact-nonnegative-integer? #f) #f]) pseq?]
              @defproc[(sequence->eseq [s sequence?]
                                       [n (or/c exact-nonnegative-integer? #f) #f]) eseq?]
              @defform[(for/eseq (for-clause ...) body ...+)]
              @defform[(for*/eseq (for-clause ...) body ...+)]
              @defform[(for/pseq (for-clause ...) body ...+)]
              @defform[(for*/pseq (for-clause ...) body ...+)])]{
 Build a sequence of @racket[n] elements, or from the elements of any Racket
 @racket[sequence], or from the results of a comprehension, in
 @math{O(N + K)}.  See also @racket[make-eseq], which takes the same arguments
 as @racket[make-vector].}

@section{Configuration}

@defproc[(sek-configure! [#:leaf-capacity k0 (and/c exact-integer? (>=/c 2))]
                         [#:node-capacity k1 (and/c exact-integer? (>=/c 2))]
                         [#:short-threshold t exact-nonnegative-integer?]
                         [#:overwrite-empty-slots? overwrite? any/c]
                         [#:check-iterator-validity? check? any/c])
         void?]{
 Set the tunable parameters of the implementation.  An argument you do not
 supply keeps its current value.

 @racket[k0] and @racket[k1] are the chunk capacities used at the leaves and
 at internal nodes, and @racket[t] is the length below which a persistent
 sequence is represented by a plain vector.  The defaults are 128, 16
 and 32.

 @racket[overwrite?] controls whether the library overwrites a slot that
 becomes logically empty. Leaving it alone saves one write per pop but lets
 the garbage collector retain a value that the sequence no longer holds;
 overwriting is the default.

 @racket[check?] controls whether the library detects the use of an
 invalidated iterator at runtime. The check costs a comparison per iterator
 operation and a sign test per update, and is on by default; with it off,
 using an invalidated iterator is undefined rather than an error.

 Set the capacities and the threshold before building any sequences: a
 structure whose chunks were allocated under different settings will not
 satisfy the invariants that @racket[sek-validate-pseq] checks, and its
 density bounds no longer hold.  Small capacities are chiefly useful for
 testing, where they force deep trees.}

@section{Validation}

@deftogether[(@defproc[(sek-validate-pseq [s pseq?]) pseq?]
              @defproc[(sek-validate-eseq [e eseq?]) eseq?])]{
 Check the structural invariants of a sequence and return it, raising an
 exception describing the first violation found.  This is the paper's runtime
 validation function; the test suite calls it after every operation.  It costs
 @math{O(N)} and is meant for testing, not production use.}

@section{Implementation notes}

Every push, pop and indexed access goes through paths that use
@racketmodname[racket/unsafe/ops]. Each use rests on an invariant the library
maintains: it allocates every chunk's backing vector itself and never
impersonates one; it reduces every index into a chunk modulo the capacity, so
the index is in range; and it bounds heads, sizes and weights by a vector
length or a sequence length, so they are fixnums. The runtime validator checks
the first two after every operation in the test suite, and the conformance
harness runs the same operations against the reference implementation.

An ephemeral sequence does not allocate its front and back chunks until the
first push to that side. The paper gives the cost of creating one as @math{O(N
+ K)}, the @math{K} being those two arrays; deferring them makes creation
@math{O(1)} without making anything else slower, since the first push
allocates exactly the chunk it needs. Deferring them is worth doing when a
program makes many short-lived sequences -- though for that use a growable
vector is still the better tool, because a chunk of capacity @math{K} is a lot
of storage for a ten-element sequence.

@section{Differences from the paper}

This library follows the paper, and where the paper is silent, the authors'
OCaml library @hyperlink["https://gitlab.inria.fr/fpottier/sek/"]{Sek}.

A conformance harness checks agreement with the reference: it runs both
implementations on the same generated script and compares the traces -- the
result of every operation, and the full contents of a dozen sequences after
each one.  The @tt{conformance} directory holds that harness, the
operation-by-operation mapping between the two APIs, and a record of what it
has checked.  The remaining differences are
these.

@itemlist[

 @item{Sequences are parameterized by neither an element type nor a
       @tt{default} value.  Logically empty slots are filled with a private
       sentinel instead, which removes the @tt{default} argument that the
       OCaml library has to thread through every constructor.}

 @item{The two flavors are one set of names rather than two parallel modules:
       an operation that builds a sequence returns the same flavor it was
       given.}

 @item{@racket[sek-sort] is stable, so it covers @tt{stable_sort} too;
       @tt{sort} makes no such promise.}

 @item{The iterator supports the operations of the OCaml library's @tt{ITER}
       and @tt{ITER_EPHEMERAL} signatures.  @racket[sek-iter-reach!] reuses the
       cursor's position when the target lies in the run or the chunk it is
       already on, which is what makes a scan with short hops cheap, but
       descends from the root when the target is in a different chunk, where
       the reference can sometimes continue from the middle-sequence cursor.}

 @item{@racket[pseq-edit] and @racket[eseq-snapshot] share the front and back
       chunks instead of copying them, where the OCaml library's versions
       copy.  A
       chunk is copied on the first write to it, if there is one, which makes
       @racket[pseq-edit] take @math{O(1)} time rather than @math{O(K)}.  This
       is observationally identical and measurably better: a loop that
       snapshots after every push runs ten times faster, because the next push
       usually extends a chunk monotonically and copies nothing.}

 @item{This library supports a @racket[#:short-threshold] of 0; the reference
       rejects it, because it still builds a compact node for a two-element
       sequence and its own validator then refuses that node.}

 @item{This library unifies the paper's @tt{One} and @tt{Short} constructors
       for short persistent sequences into a single vector representation,
       which appears only at the top of the structure, as in the authors'
       implementation.}

 @item{@racket[eseq-snapshot] folds the two inner chunks into the middle
       sequence, as the OCaml library does, so it costs @math{O(K log_K N)} in
       the worst case rather than the @math{O(1)} the paper gives.}

 @item{As in the paper's implementation, monotonic in-place updates make the
       persistent flavor unsafe to share across threads without
       synchronization.}]

@bibliography[
 (bib-entry #:key "Chargueraud26"
            #:title "A Catenable, Splittable, Transient Sequence Data Structure"
            #:author "Arthur Charguéraud and François Pottier"
            #:location "International Conference on Functional Programming"
            #:url "https://doi.org/10.1145/3828706"
            #:date "2026")
 (bib-entry #:key "Stucki15"
            #:title "RRB Vector: A Practical General Purpose Immutable Sequence"
            #:author "Nicolas Stucki, Tiark Rompf, Vlad Ureche, and Phil Bagwell"
            #:location "International Conference on Functional Programming"
            #:url "https://dl.acm.org/doi/abs/10.1145/2784731.2784739"
            #:date "2015")
]

@close-eval[the-eval]
