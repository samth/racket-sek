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

A @deftech{sequence} is a finite ordered collection of elements that supports
random access, pushing and popping at either end, concatenation and
splitting.  Sequences come in two flavors: a @deftech{persistent sequence} is
immutable, so an operation on one produces a new sequence and leaves the
original intact, and an @deftech{ephemeral sequence} is updated in place.  The
library implements the data structure of Charguéraud and Pottier
@cite["Chargueraud26"].

Conversions between the two flavors are cheap: @racket[psek-edit] produces an
ephemeral sequence in constant time, and @racket[esek-snapshot] produces a
persistent sequence in logarithmic time, where snapshotting and editing other
data structures typically take time linear in the size of the data.  This
approach is called @deftech{transience}, following terminology originally
developed in Clojure.

@examples[
#:eval the-eval
(define p (list->psek '(1 2 3 4 5)))
(psek->list (psek-push-front p 0))
(code:comment "p itself is unchanged")
(psek->list p)
(code:comment "switch to in-place updates, in O(1) time")
(define e (psek-edit p))
(esek-push-back! e 6)
(esek-set! e 0 'a)
(code:comment "and back again")
(define q (esek-snapshot e))
(psek->list q)
(psek->list p)
]

A sequence stores its elements in @italic{chunks} of up to @math{K} elements.
Throughout this manual, @math{N} is the length of the sequence, @math{K} the
chunk capacity (128 at the leaves and 16 at internal nodes), and @math{T} the
length up to which a persistent sequence is held in a plain vector (32).
Unless otherwise specified, operations on a sequence of length @math{N} take
@math{O(log_K N)} time.  As with @tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{treelists}, the base of the
@math{log} is large enough that it is effectively constant-time for many
purposes: a sequence of up to a hundred million elements is at most six
levels deep.

Sequences of either flavor can be used as single-valued @racket[sequence]s
whose elements are the sequence's elements, they are @racket[serializable?],
and two sequences of the same flavor are @racket[equal?] when their elements
are.  A persistent sequence can also be used as a @tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{stream}.  Any number of threads may
use a persistent sequence at once, and an ephemeral sequence too as long as
none of them updates it; @secref["threads"] gives the rules.

@section{Persistent Sequences}

@defproc[(psek? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{persistent sequence},
@racket[#f] otherwise.}

@defproc[(psek [v any/c] ...) psek?]{

Returns a @tech{persistent sequence} with @racket[v]s as its elements in
order.

@examples[
#:eval the-eval
(psek 1 "a" 'apple)
]}

@deftogether[(
@defproc[(make-psek [n exact-nonnegative-integer?] [v any/c #f]) psek?]
@defproc[(build-psek [n exact-nonnegative-integer?]
                     [proc (-> exact-nonnegative-integer? any/c)]) psek?]
@defproc[(sequence->psek [s sequence?]
                         [n (or/c exact-nonnegative-integer? #f) #f]) psek?]
@defform[(for/psek (for-clause ...) body ...+)]
@defform[(for*/psek (for-clause ...) body ...+)]
)]{

Build a @tech{persistent sequence} of @racket[n] copies of @racket[v], of the
results of @racket[proc] on @racket[0] through @racket[(sub1 n)], of the
elements of any Racket @racket[sequence] (of which @racket[n], when given, is
the length), or of the results of a comprehension.  Each takes
@math{O(N + K)} time.

@examples[
#:eval the-eval
(build-psek 4 (lambda (i) (* i i)))
(for/psek ([c "abc"]) (char-upcase c))
]}

@deftogether[(
@defproc[(psek-empty? [s psek?]) boolean?]
@defthing[empty-psek (and/c psek? psek-empty?)]
)]{

A predicate and constant for a @tech{persistent sequence} of length 0.}

@defproc[(psek-length [s psek?]) exact-nonnegative-integer?]{

Returns the number of elements in @racket[s].  This operation takes
@math{O(1)} time.

@examples[
#:eval the-eval
(psek-length (psek 1 "a" 'apple))
]}

@deftogether[(
@defproc[(psek-add [s psek?] [v any/c]) psek?]
@defproc[(psek-cons [s psek?] [v any/c]) psek?]
@defproc[(psek-push-back [s psek?] [v any/c]) psek?]
@defproc[(psek-push-front [s psek?] [v any/c]) psek?]
)]{

Return a @tech{persistent sequence} with @racket[v] added at the end
(@racket[psek-add]) or at the front (@racket[psek-cons]).
@racket[psek-push-back] and @racket[psek-push-front] are aliases for these
operations, following the names in the paper.

These take @math{O(K log_K N)} time in the worst case.  Most take @math{O(1)}
time, or @math{O(K)} time if @racket[s] has already had an element added at
that end or is the result of removing one from it.

@examples[
#:eval the-eval
(define s (psek 1 2 3))
(psek-cons s 0)
(psek-add s 4)
s
]}

@deftogether[(@defproc[(psek-pop-front [s psek?]) (values any/c psek?)]
              @defproc[(psek-pop-back [s psek?]) (values any/c psek?)])]{

Return the element at the given end and the rest of the sequence, or raise
@racket[exn:fail:contract] if @racket[s] is empty.  These operations take
@math{O(log_K N)} time in the worst case, and most take @math{O(1)}; they take
@math{O(T)} time when the result has at most @math{T} elements.}

@deftogether[(@defproc[(psek-first [s psek?]) any/c]
              @defproc[(psek-last [s psek?]) any/c])]{

Shorthands for using @racket[psek-ref] to access the first or last element of
a @tech{persistent sequence}.}

@deftogether[(@defproc[(psek-ref [s psek?] [i exact-nonnegative-integer?]) any/c]
              @defproc[(psek-set [s psek?] [i exact-nonnegative-integer?]
                                 [v any/c]) psek?])]{

Returns the @racket[i]th element of @racket[s], or a sequence with that
element replaced by @racket[v].  The first element is position @racket[0], and
the last position is one less than @racket[(psek-length s)].

@racket[psek-ref] takes @math{O(K log_K N)} time in general, and
@math{O(log_K N)} time for a sequence built without concatenation.
@racket[psek-set] takes @math{O(K log_K N)} time.

@examples[
#:eval the-eval
(define s (list->psek '(a b c d)))
(psek-ref s 2)
(psek->list (psek-set s 2 'C))
(psek->list s)
]}

@defproc[(psek-append [s1 psek?] [s2 psek?]) psek?]{

Returns a @tech{persistent sequence} with the elements of @racket[s1] followed
by those of @racket[s2].  This operation takes
@math{O(K log_K N + log_K^2 N)} time.

@examples[
#:eval the-eval
(psek->list (psek-append (psek 1 2) (psek 3 4)))
]}

@defproc[(psek-split [s psek?] [i exact-nonnegative-integer?])
         (values psek? psek?)]{

Returns the first @racket[i] elements and the rest.  This operation takes
@math{O(K log_K N + log_K^2 N)} time.

@examples[
#:eval the-eval
(define-values (before after) (psek-split (list->psek '(a b c d e)) 2))
(psek->list before)
(psek->list after)
]}

@deftogether[(@defproc[(psek-take [s psek?] [i exact-nonnegative-integer?]) psek?]
              @defproc[(psek-drop [s psek?] [i exact-nonnegative-integer?]) psek?])]{

The two halves of @racket[psek-split] separately: the first @racket[i]
elements, or all but the first @racket[i].  Each takes the same time as
@racket[psek-split].

@examples[
#:eval the-eval
(define s (list->psek '(a b c d e)))
(psek->list (psek-take s 2))
(psek->list (psek-drop s 2))
]}

@deftogether[(@defproc[(psek->list [s psek?]) list?]
              @defproc[(list->psek [xs list?]) psek?]
              @defproc[(psek->vector [s psek?]) vector?]
              @defproc[(vector->psek [v vector?]) psek?]
              @defproc[(psek-for-each [s psek?] [proc (-> any/c any)]) void?]
              @defproc[(psek-map [s psek?] [proc (-> any/c any/c)]) psek?])]{

Conversion and iteration; see @racket[in-psek] for iterating in a
@racket[for] clause.  Each of these takes @math{O(N)} time.}

@defform*[((in-psek s) (in-psek s dir))]{

A @racket[sequence] over the elements of @racket[s], in @racket['forward]
order by default or @racket['backward].  It is fastest written directly in a
@racket[for] clause.  A full traversal takes @math{O(N)} time.}

@defproc[(psek-edit [s psek?]) esek?]{

Returns an @tech{ephemeral sequence} with the contents of @racket[s], which is
unaffected by later updates to the result.  This operation takes @math{O(1)}
time.

@examples[
#:eval the-eval
(define s (psek 1 2 3))
(define e (psek-edit s))
(esek-set! e 0 'changed)
(esek->list e)
(psek->list s)
]}

@section{Ephemeral Sequences}

An operation that changes an @tech{ephemeral sequence} modifies the sequence
it is given, where the corresponding operation on a @tech{persistent
sequence} returns a new one.  Several threads may read an ephemeral sequence
at once, but an update needs exclusive access; @secref["threads"] lists the
operations that count as updates.

@defproc[(esek? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an @tech{ephemeral sequence},
@racket[#f] otherwise.}

@defproc[(esek [v any/c] ...) esek?]{

Returns an @tech{ephemeral sequence} with @racket[v]s as its elements in
order.

@examples[
#:eval the-eval
(esek 1 "a" 'apple)
]}

@deftogether[(
@defproc[(make-esek [n exact-nonnegative-integer? 0] [v any/c #f]) esek?]
@defproc[(build-esek [n exact-nonnegative-integer?]
                     [proc (-> exact-nonnegative-integer? any/c)]) esek?]
@defproc[(sequence->esek [s sequence?]
                         [n (or/c exact-nonnegative-integer? #f) #f]) esek?]
@defform[(for/esek (for-clause ...) body ...+)]
@defform[(for*/esek (for-clause ...) body ...+)]
)]{

Build an @tech{ephemeral sequence} of @racket[n] copies of @racket[v], of the
results of @racket[proc] on @racket[0] through @racket[(sub1 n)], of the
elements of any Racket @racket[sequence] (of which @racket[n], when given, is
the length), or of the results of a comprehension.  @racket[make-esek] takes
the same arguments as @racket[make-vector].  Each takes @math{O(N + K)} time.

@examples[
#:eval the-eval
(make-esek 0)
(make-esek 3 'pear)
]}

@defproc[(esek-empty? [e esek?]) boolean?]{

Returns @racket[#t] if @racket[e] has no elements, @racket[#f] otherwise.
This operation takes @math{O(1)} time.}

@defproc[(esek-length [e esek?]) exact-nonnegative-integer?]{

Returns the number of elements in @racket[e].  This operation takes
@math{O(1)} time.

@examples[
#:eval the-eval
(esek-length (esek 1 "a" 'apple))
]}

@deftogether[(
@defproc[(esek-add! [e esek?] [v any/c]) void?]
@defproc[(esek-cons! [e esek?] [v any/c]) void?]
@defproc[(esek-push-back! [e esek?] [v any/c]) void?]
@defproc[(esek-push-front! [e esek?] [v any/c]) void?]
@defproc[(esek-pop-back! [e esek?]) any/c]
@defproc[(esek-pop-front! [e esek?]) any/c]
)]{

Adds @racket[v] at the end (@racket[esek-add!]) or the front
(@racket[esek-cons!]) of @racket[e], or removes and returns the element at one
of its ends, modifying @racket[e] in place.  @racket[esek-push-back!] and
@racket[esek-push-front!] are aliases for these operations, following the
names in the paper.

In the worst case a push takes @math{O(K log_K N)} time and a pop
@math{O(log_K N)}.  A series of pushes and pops takes amortized
@math{O(log_K N)} time per operation, and most take @math{O(1)}.

@examples[
#:eval the-eval
(define items (esek 1 2 3))
(esek-cons! items 0)
(esek-add! items 4)
items
(esek-pop-front! items)
(esek-pop-back! items)
items
]}

@deftogether[(
@defproc[(esek-ref [e esek?] [i exact-nonnegative-integer?]) any/c]
@defproc[(esek-set! [e esek?] [i exact-nonnegative-integer?] [v any/c]) void?]
)]{

Returns the @racket[i]th element of @racket[e], or replaces it with
@racket[v].  The first element is position @racket[0], and the last position
is one less than @racket[(esek-length e)].

@racket[esek-ref] takes @math{O(K log_K N)} time in general, and
@math{O(log_K N)} time for a sequence built without concatenation.
@racket[esek-set!] takes @math{O(K log_K N)} time.  For a sequence built
without concatenation it takes @math{O(log_K N)}, except that after a snapshot
or an edit the first update near each index costs the full
@math{O(K log_K N)}.

@examples[
#:eval the-eval
(define items (esek 1 "a" 'apple))
(esek-ref items 2)
(esek-set! items 2 'pear)
items
]}

@deftogether[(
@defproc[(esek-first [e esek?]) any/c]
@defproc[(esek-last [e esek?]) any/c]
)]{

Shorthands for using @racket[esek-ref] to access the first or last element of
an @tech{ephemeral sequence}.}

The six operations that follow rearrange ephemeral sequences in place, and
they @italic{consume} the sequences they are given, leaving each empty.  Use
@racket[sek-take], @racket[sek-drop] and @racket[sek-sub] when the input must
survive.

@defproc[(esek-append! [e esek?] [other (or/c esek? psek?)]
                       [side (or/c 'front 'back) 'back]) void?]{

Appends the contents of @racket[other] to @racket[e], in place, at the given
end.  This empties an ephemeral @racket[other] and leaves a persistent one
untouched.  The two sequences must be distinct.  This operation takes
@math{O(K log_K N + log_K^2 N)} time.}

@defproc[(esek-concat! [e1 esek?] [e2 esek?]) esek?]{

Returns a new sequence holding the concatenation, and empties both arguments,
which must be distinct.  This operation takes @math{O(K log_K N + log_K^2 N)}
time.}

@defproc[(esek-split! [e esek?] [i exact-nonnegative-integer?])
         (values esek? esek?)]{

Returns two new sequences holding the first @racket[i] elements and the rest,
and empties @racket[e].  This operation takes @math{O(K log_K N + log_K^2 N)}
time.}

@defproc[(esek-carve! [e esek?] [i exact-nonnegative-integer?]
                      [side (or/c 'front 'back) 'back]) esek?]{

Splits @racket[e] at @racket[i], keeping one part in @racket[e] and returning
the other: @racket['back] keeps the front part, @racket['front] keeps the back
part.  It saves an allocation over @racket[esek-split!] when one part is going
back into the same variable.  This operation takes
@math{O(K log_K N + log_K^2 N)} time.}

@deftogether[(@defproc[(esek-take! [e esek?] [i exact-nonnegative-integer?]
                                   [side (or/c 'front 'back) 'front]) void?]
              @defproc[(esek-drop! [e esek?] [i exact-nonnegative-integer?]
                                   [side (or/c 'front 'back) 'front]) void?])]{

Truncate @racket[e] at index @racket[i].  @racket[esek-take!] keeps the front
part when @racket[side] is @racket['front] and the back part otherwise;
@racket[esek-drop!] keeps the other one.  Each takes
@math{O(K log_K N + log_K^2 N)} time.}

@defproc[(esek-assign! [e1 esek?] [e2 esek?]) void?]{

Moves the contents of @racket[e2] into @racket[e1] and empties @racket[e2],
or does nothing if the two are the same sequence.  This operation takes
@math{O(K log_K N)} time.}

@defproc[(esek-clear! [e esek?]) void?]{

Empties @racket[e].  This operation takes @math{O(1)} time.}

@deftogether[(@defproc[(esek->list [e esek?]) list?]
              @defproc[(list->esek [xs list?]) esek?]
              @defproc[(esek->vector [e esek?]) vector?]
              @defproc[(esek-for-each [e esek?] [proc (-> any/c any)]) void?])]{

Conversion and iteration; see @racket[in-esek] for iterating in a
@racket[for] clause.  Each of these takes @math{O(N)} time.}

@defform*[((in-esek e) (in-esek e dir))]{

A @racket[sequence] over the elements of @racket[e], in @racket['forward]
order by default or @racket['backward].  It is fastest written directly in a
@racket[for] clause.  Updating @racket[e] during the loop is detected, as for
an @tech{iterator}.  A full traversal takes @math{O(N)} time.}

@deftogether[(
@defproc[(esek-fill! [e esek?] [v any/c]
                     [start exact-nonnegative-integer? 0]
                     [end exact-nonnegative-integer? (esek-length e)]) void?]
@defproc[(esek-copy! [dst esek?] [dst-start exact-nonnegative-integer?]
                     [src sek?]
                     [src-start exact-nonnegative-integer? 0]
                     [src-end exact-nonnegative-integer? (sek-length src)]) void?]
)]{

Change the elements of an @tech{ephemeral sequence} in place:
@racket[esek-fill!] sets those from @racket[start] to @racket[end] to
@racket[v], and @racket[esek-copy!] sets those starting at @racket[dst-start]
to match the elements of @racket[src] from @racket[src-start] to
@racket[src-end].  They take the same arguments in the same order as
@racket[vector-fill!] and @racket[vector-copy!].  @racket[src] may be of
either flavor, and only the destination is modified; @racket[esek-copy!]
handles the case where @racket[src] and @racket[dst] are the same sequence and
the ranges overlap.

For a sequence built without concatenation, both cost
@math{O(size + K log_K N)} time, even after a snapshot, and
@math{O((size + K) log_K N)} in general.

@examples[
#:eval the-eval
(define items (esek 1 2 3 4 5))
(esek-fill! items 'x 1 3)
items
(esek-copy! items 0 (psek 'a 'b))
items
]}

@defproc[(esek-snapshot [e esek?]) psek?]{

Returns a @tech{persistent sequence} with the current contents of @racket[e].
@racket[e] remains usable and keeps its contents; later updates to it do not
affect the snapshot.  This operation takes @math{O(K log_K N)} time in the
worst case, and later updates to @racket[e] may cost more than they otherwise
would.

@examples[
#:eval the-eval
(define e (list->esek '(1 2 3)))
(define snap (esek-snapshot e))
(esek-push-back! e 4)
(esek->list e)
(code:comment "the snapshot does not see the push")
(psek->list snap)
]}

@defproc[(esek-snapshot-and-clear! [e esek?]) psek?]{

Returns a snapshot of @racket[e] and empties @racket[e].  Unlike
@racket[esek-snapshot], it does not make later updates to @racket[e] more
expensive.  This operation takes @math{O(K log_K N)} time in the worst case.}

@defproc[(esek-copy [e esek?] [#:mode mode (or/c 'share 'copy) 'copy]) esek?]{

Returns an independent ephemeral copy of @racket[e].  In @racket['copy] mode,
the default, the copy takes @math{O(N)} time and leaves no later cost.  In
@racket['share] mode it takes @math{O(1)} time, but later updates to either
sequence, @racket[e] included, cost more than they otherwise would.}

@section{Operations on Either Flavor}

The operations in this section accept a persistent or an ephemeral sequence.
Those that build a new sequence return the same flavor they were given.

@deftogether[(@defproc[(sek? [v any/c]) boolean?]
              @defproc[(sek-length [s sek?]) exact-nonnegative-integer?]
              @defproc[(sek-empty? [s sek?]) boolean?]
              @defproc[(sek-ref [s sek?] [i exact-nonnegative-integer?]) any/c]
              @defproc[(sek-first [s sek?]) any/c]
              @defproc[(sek-last [s sek?]) any/c])]{

Basic accessors, dispatching on the flavor.  Each takes the same time as the
corresponding operation on that flavor.}

@defform*[((in-sek s) (in-sek s dir))]{

A @racket[sequence] over the elements of @racket[s], like @racket[in-psek] or
@racket[in-esek].  A full traversal takes @math{O(N)} time.}

@deftogether[(@defproc[(sek-for-each [s sek?] [proc (-> any/c any)]
                                     [dir (or/c 'forward 'backward) 'forward]) void?]
              @defproc[(sek-for-each/index [s sek?] [proc (-> exact-nonnegative-integer? any/c any)]
                                           [dir (or/c 'forward 'backward) 'forward]) void?])]{

Apply @racket[proc] to each element, or to each index and element, in the
given direction.  Each takes @math{O(N)} time plus the cost of the calls to
@racket[proc].}

@deftogether[(@defproc[(sek-fold-left [s sek?] [proc (-> any/c any/c any/c)]
                                      [init any/c]) any/c]
              @defproc[(sek-fold-right [s sek?] [proc (-> any/c any/c any/c)]
                                       [init any/c]) any/c])]{

Fold from the left or from the right.  Each takes @math{O(N)} time plus the
cost of the calls to @racket[proc].}

@deftogether[(@defproc[(sek->list [s sek?] [dir (or/c 'forward 'backward) 'forward]) list?]
              @defproc[(sek->vector [s sek?]) vector?])]{

Conversions.  Each takes @math{O(N)} time.}

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

Search operations, all of which stop as soon as they can.  @racket[sek-find]
returns @racket[#f] when nothing matches, so use @racket[sek-find-index] when
an element could itself be @racket[#f].  Each takes @math{O(N)} time in the
worst case.}

@defproc[(sek-index-of [s sek?] [v any/c]
                       [same? (-> any/c any/c any/c) equal?])
         (or/c exact-nonnegative-integer? #f)]{

Returns the index of the first element of @racket[s] that is @racket[same?] to
@racket[v], or @racket[#f] if there is none.  @racket[same?] receives
@racket[v] first and the element second.  This operation takes @math{O(N)}
time in the worst case.

@examples[
#:eval the-eval
(sek-index-of (psek 'a 'b 'c) 'b)
(sek-index-of (psek 'a 'b 'c) 'z)
]}

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

The usual list-shaped operations.  @racket[sek-append*] concatenates a
sequence of sequences; given an ephemeral one it empties both it and its
elements.  Each takes @math{O(N)} time plus the cost of the calls to
@racket[proc] or @racket[pred].}

@deftogether[(@defproc[(sek-sub [s sek?] [start exact-nonnegative-integer?]
                                [size exact-nonnegative-integer?]) sek?]
              @defproc[(sek-take [s sek?] [n exact-nonnegative-integer?]) sek?]
              @defproc[(sek-drop [s sek?] [n exact-nonnegative-integer?]) sek?]
              @defproc[(sek-copy [s sek?] [#:mode mode (or/c 'share 'copy) 'copy]) sek?])]{

The @racket[size] elements of @racket[s] starting at @racket[start], the first
@racket[n] elements, all but the first @racket[n], or a copy.
@racket[sek-copy] is the identity on a persistent sequence and
@racket[esek-copy] on an ephemeral one.  None of them modifies @racket[s].

@racket[sek-take] and @racket[sek-drop] take @math{O(K log_K N + log_K^2 N)}
time.  So does @racket[sek-sub] on a @tech{persistent sequence}; on an
@tech{ephemeral sequence} it copies the slice instead, in
@math{O(size + K log_K N)} time, which does not make later updates to
@racket[s] more expensive.}

@deftogether[(
@defproc[(sek-take-right [s sek?] [n exact-nonnegative-integer?]) sek?]
@defproc[(sek-drop-right [s sek?] [n exact-nonnegative-integer?]) sek?]
)]{

Produce a sequence like @racket[s] but with only the last @racket[n] elements,
or without the last @racket[n] elements, respectively.  Neither modifies
@racket[s], and each takes the same time as @racket[sek-take] and
@racket[sek-drop].

@examples[
#:eval the-eval
(sek-take-right (psek 1 2 3 4 5) 2)
(sek-drop-right (psek 1 2 3 4 5) 2)
]}

@deftogether[(
@defproc[(sek-insert [s sek?] [i exact-nonnegative-integer?] [v any/c]) sek?]
@defproc[(sek-delete [s sek?] [i exact-nonnegative-integer?]) sek?]
)]{

Produce a sequence like @racket[s], except that @racket[v] is inserted before
the element at @racket[i], or that the element at @racket[i] is removed.  If
@racket[i] is @racket[(sek-length s)] then @racket[sek-insert] adds
@racket[v] at the end.  Neither modifies @racket[s], and each takes
@math{O(K log_K N + log_K^2 N)} time.

@examples[
#:eval the-eval
(sek-insert (psek 1 2 3) 1 'x)
(sek-insert (psek 1 2 3) 3 'x)
(sek-delete (psek 1 2 3) 1)
]}

@deftogether[(@defproc[(sek-sort [s sek?] [less? (-> any/c any/c any/c)]) sek?]
              @defproc[(sek-uniq [s sek?] [same? (-> any/c any/c any/c) equal?]) sek?]
              @defproc[(sek-merge [s1 sek?] [s2 sek?] [less? (-> any/c any/c any/c)]) sek?])]{

A stable sort; a pass that drops adjacent duplicates, and so drops every
duplicate from a sorted sequence; and a stable merge of two sorted sequences.
@racket[sek-sort] takes @math{O(N log N)} time, and the other two
@math{O(N)}.}

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

Operations on two sequences at once.  They stop at the end of the shorter
sequence, except @racket[sek-equal?], which first compares lengths, and
@racket[sek-compare], which orders a proper prefix before the sequence that
extends it.  @racket[sek-zip] pairs elements with @racket[cons], and
@racket[sek-unzip] undoes it.  Each takes @math{O(N)} time plus the cost of
the calls to the procedure argument.}

@section{Iterators}

An @deftech{iterator} is a cursor into a sequence.  Its position is an integer
in @math{[-1, N]}: the indices in @math{[0, N)} designate elements, and the two
extremes are @italic{sentinels}, one just before the sequence and one just
after.  An iterator that sits on a sentinel is @racket[sek-iter-finished?].
Moving an iterator one step costs @math{O(1)} amortized, so a traversal by
iterator costs @math{O(N)}, where repeated @racket[sek-ref] pays for a lookup
at every element.

Any update to an ephemeral sequence invalidates every iterator on it, except
that @racket[sek-iter-set!] leaves the iterator it writes through usable.
Using an invalidated iterator raises an exception; make a new one instead.
Iterators on persistent sequences are never invalidated.

@defproc[(sek-iterator [s sek?] [i exact-integer? 0]) sek-iter?]{

Returns an iterator on the element of @racket[s] at index @racket[i], or on a
sentinel when @racket[i] is @racket[-1] or @racket[(sek-length s)].  Starting
at either end takes @math{O(1)} time, and starting anywhere else at most as
long as @racket[sek-ref].

@examples[
#:eval the-eval
(define s (psek 'a 'b 'c 'd))
(define it (sek-iterator s 1))
(sek-iter-get it)
(sek-iter-move! it)
(sek-iter-get it)
(define backward (sek-iterator s (sub1 (sek-length s))))
(let loop ([acc '()])
  (if (sek-iter-finished? backward)
      (reverse acc)
      (let ([x (sek-iter-get backward)])
        (sek-iter-move! backward 'backward)
        (loop (cons x acc)))))
]}

@defproc[(sek-iter? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an @tech{iterator}, @racket[#f]
otherwise.}

@deftogether[(@defproc[(sek-iter-index [it sek-iter?]) exact-integer?]
              @defproc[(sek-iter-finished? [it sek-iter?]) boolean?])]{

The iterator's current position, in @math{[-1, N]}, and whether that position
is a sentinel.  Each takes @math{O(1)} time.}

@defproc[(sek-iter-get [it sek-iter?]) any/c]{

Returns the element under the iterator, or raises an exception at a sentinel.
This operation takes @math{O(1)} time.}

@defproc[(sek-iter-move! [it sek-iter?]
                         [dir (or/c 'forward 'backward) 'forward]) void?]{

Moves the iterator one element in the given direction.  Moving off the far
sentinel raises an exception.  This operation takes @math{O(1)} amortized
time and @math{O(log_K N)} in the worst case.}

@defproc[(sek-iter-reach! [it sek-iter?] [i exact-integer?]) void?]{

Moves the iterator to index @racket[i], which may be @racket[-1] or the length
of the sequence.  Reaching either sentinel, or a position inside the run of
storage the iterator is on, takes @math{O(1)} time; any other move takes at
most as long as @racket[sek-ref].}

@defproc[(sek-iter-set! [it sek-iter?] [v any/c]) void?]{

Replaces the element under @racket[it], which must be an iterator on an
ephemeral sequence and not on a sentinel, with @racket[v].  It invalidates
every @italic{other} iterator on that sequence.  The first write into a chunk
that is shared with a snapshot takes @math{O(K log_K N)} time in the worst
case, and later writes into the same chunk @math{O(1)}; for a sequence built
without concatenation, a sweep that writes every element takes
@math{O(N + K log_K N)} time, even after a snapshot.}

@section[#:tag "threads"]{Threads}

These rules apply to Racket threads, whether or not they run in parallel, and
to futures.

Any number of threads may use a @tech{persistent sequence} at once, with any
operations, and without synchronization.

Any number of threads may use an @tech{ephemeral sequence} at once as long as
none of them updates it.  An update needs exclusive access: while it runs, no
other thread may use the sequence or an @tech{iterator} over it.  These
operations update an ephemeral sequence:

@itemlist[
 @item{every operation whose name begins with @racketidfont{esek-} and ends
       in @litchar{!}, on the sequence it modifies.
       @racket[esek-append!], @racket[esek-assign!] and @racket[esek-concat!]
       also update the ephemeral sequence that they empty; the source of
       @racket[esek-copy!] is only read;}
 @item{@racket[sek-iter-set!], on an iterator over the sequence; and}
 @item{@racket[sek-append*], which updates an ephemeral sequence it is given
       and every ephemeral sequence in it, since it empties them.}]

Every other operation only reads the sequences it is given.  In particular,
iterating over a sequence, @racket[esek-snapshot], @racket[esek-copy],
@racket[equal?], and the operations on either flavor that build a new
sequence are all reads.

The two flavors can share structure -- a snapshot shares with the ephemeral
sequence it was taken from, and @racket[psek-edit] returns an ephemeral
sequence that shares with its argument -- but the sharing needs no
synchronization: one thread may update an ephemeral sequence while others use
a persistent sequence that shares with it.

@racket[sek-iter-move!], @racket[sek-iter-reach!] and @racket[sek-iter-set!]
change the iterator, so one iterator may be used by only one thread at a time.
Each of several threads may have its own iterator over one sequence, subject
to the rules above.

A program that breaks these rules gets unspecified results: an operation may
return a wrong answer or raise @racket[exn:fail:sek], and the sequence may be
left inconsistent.  The program's memory is not corrupted, however; no use of
this library, correct or not, corrupts memory.

These rules assume that a sequence reaches another thread through an
operation that synchronizes the two threads, such as a channel, a semaphore,
or @racket[thread-wait].

@defstruct[(exn:fail:sek exn:fail) ()]{

Raised by an operation that finds the structure of a sequence inconsistent,
which only a program that breaks the rules above can cause.}

@section{Comparison with Treelists}

Racket's @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{treelists}
solve a similar problem---efficient, persistent data structures with fast
indexing, implemented using wide trees (RRB trees @cite["Stucki15"], in their
case).  Both structures support random
access, concatenation and splitting in @math{O(log N)} time, with a base large
enough that the logarithm is effectively a constant.

Sequences from this library are more efficient at the beginning and the end.
Most pushes and pops at either end take @math{O(1)} time, for either flavor
(see @racket[psek-add] for when a persistent push costs @math{O(K)}).  In the
worst case a push takes @math{O(K log_K N)} time and a pop @math{O(log_K N)},
and a series of pushes and pops on an @tech{ephemeral sequence} takes
amortized @math{O(log_K N)} time per operation.  The corresponding treelist
operations take @math{O(log N)} time.

Conversions are cheaper too.  @racket[treelist-copy] and
@racket[mutable-treelist-snapshot] each take @math{O(N)} time, where
@racket[psek-edit] takes @math{O(1)} and @racket[esek-snapshot]
@math{O(K log_K N)}.  Likewise @racket[mutable-treelist-append!] takes
@math{O(N)} time in the length of its second argument, where
@racket[esek-append!] takes @math{O(K log_K N + log_K^2 N)}.

Traversal is @math{O(N)} for both.

@;{
The settings and the validators are not part of the public interface; the
tests reach them through (submod sek config) and (submod sek validate).

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
 at internal nodes, and @racket[t] is the length up to which a persistent
 sequence is represented by a plain vector.  The defaults are 128, 16
 and 32.  A capacity must be less than @math{2^30}, or @math{2^14} on a
 platform whose fixnums are 30 bits wide.

 @racket[overwrite?] controls whether popping from an ephemeral sequence
 overwrites the slot it empties. Leaving it alone saves one write per pop but
 lets the garbage collector retain a value that the sequence no longer holds;
 overwriting is the default. Popping from a persistent sequence never
 overwrites, so the popped value stays reachable for as long as the result
 does.

 @racket[check?] controls whether the library detects the use of an
 invalidated iterator at runtime.  The check is on by default; with it off,
 using an invalidated iterator gives unspecified results rather than an
 error.

 Set the capacities and the threshold before building any sequences: a
 sequence built under other settings fails @racket[sek-validate-psek] and does
 not meet the bounds given here.  Small capacities are useful mainly for
 testing.  The settings apply to the whole program, so
 @racket[sek-configure!] must not run while another thread is using a
 sequence.}

@section{Validation}

@deftogether[(@defproc[(sek-validate-psek [s psek?]) psek?]
              @defproc[(sek-validate-esek [e esek?]) esek?])]{
 Check the structural invariants of a sequence and return it, raising an
 exception describing the first violation found.  This is the paper's runtime
 validation function.  It costs @math{O(N)} and is meant for testing, not
 production use.}
}

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
