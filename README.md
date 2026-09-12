# sek — catenable, splittable, transient sequences for Racket

A Racket implementation of

> Arthur Charguéraud and François Pottier.
> **A Catenable, Splittable, Transient Sequence Data Structure.**
> Proc. ACM Program. Lang. 10, ICFP, Article 308 (August 2026).
> <https://doi.org/10.1145/3828706>

A *transient* data structure is an ephemeral data structure, a persistent one,
and fast conversions between them. You take a persistent snapshot in constant
time when you need one, and keep the speed of in-place updates everywhere else.

```racket
(require sek)

(define p (list->pseq '(1 2 3 4 5)))
(pseq->list (pseq-push-front p 0))   ; '(0 1 2 3 4 5)
(pseq->list p)                       ; '(1 2 3 4 5) — unchanged

(define e (pseq-edit p))             ; O(1): switch to in-place updates
(eseq-push-back! e 6)
(eseq-set! e 0 'a)
(define q (eseq-snapshot e))         ; O(1): switch back
(pseq->list q)                       ; '(a 2 3 4 5 6)
(pseq->list p)                       ; '(1 2 3 4 5) — still unchanged
```

## What is here

| file | paper | contents |
| --- | --- | --- |
| `sek/config.rkt` | §4.1 | chunk capacities and the short-sequence threshold |
| `sek/array.rkt` | §2 | transient arrays: `parray` / `earray` |
| `sek/chunk.rkt` | §3.3, Fig. 11–13 | transient chunks: circular support + view + ownership id |
| `sek/ptree.rkt` | §3.1–3.2 | the Sek tree: push, pop, get, set, split, concat, merge |
| `sek/persistent.rkt` | §3.5 | `pseq`, including the compact representation of short sequences |
| `sek/ephemeral.rkt` | §3.6 | `eseq`, including the inner front/back chunks |
| `sek/iterate.rkt` | — | lazy left-to-right traversal, behind `prop:sequence` |
| `sek/segment.rkt` | `Segment` | runs of contiguous storage |
| `sek/iterator.rkt` | `Iterator` (§5.4) | first-class cursors, with invalidation checking |
| `sek/generic.rkt` | `Generic`, `PublicSignature` | the derived operation surface |
| `sek/check.rkt` | Appendix A, Fig. 19 | the runtime validation function |
| `bench/` | §4.3 | benchmarks against Racket's sequences and against the OCaml library |
| `sek/tests/` | §4.2 | randomized differential testing against list/vector references |
| `conformance/` | — | differential testing against the OCaml library itself |

The paper describes the data structure; segments, iterators, the derived
operations and the conformance harness follow the authors' OCaml library,
[Sek](https://gitlab.inria.fr/fpottier/sek/), where the paper is silent.

## The three ideas

**Ownership identifiers** (§2.4, from L'orange). Every chunk carries an id, and
every ephemeral sequence carries one. If they match, the chunk is not shared
with anybody and can be written in place. `eseq-snapshot` hands the sequence a
*fresh* id, so every chunk in it instantly stops being recognizable as owned —
which is why a snapshot costs nothing and yet is safe.

**Monotonic updates** (§3.3). A chunk is a view onto a partially-filled
circular buffer. Filling a slot that lies outside every view cannot be observed
by anyone, so a persistent push can often write in place instead of copying the
whole chunk.

**Views** (§3.3). A persistent pop just shrinks the view. The support is left
alone, so the versions that still point at it keep working.

Layered on top is the tree of §3.1: a level is a front chunk, a middle
sequence, and a back chunk, where the middle sequence is the same structure one
level down holding *chunks* of this level's items. The elements near the two
ends therefore live at the root, which is what makes push and pop at the ends
cheap; a density invariant on the middle sequences (any two adjacent chunks
hold more than `K` items together) keeps the depth logarithmic even after
adversarial sequences of splits and concatenations.

## Iterators and segments

Repeated `pseq-ref` costs O(n log_K n) to walk a sequence. An iterator walks it
in O(n), because stepping stays inside one run of contiguous storage most of
the time:

```racket
(define it (sek-iterator s 'forward))
(let loop ()
  (unless (sek-iter-finished? it)
    (displayln (sek-iter-get-and-move! it 'forward))
    (loop)))
```

An iterator can also hand out the whole run it is sitting on, as a *segment* —
a vector, a start index and a length. That turns a traversal into a sequence of
tight vector loops, and it is how everything in the next section is built:

```racket
(sek-segments-for-each s (lambda (sg) (for ([x (in-segment sg)]) ...)))
```

Segments are views into the sequence, not copies. On an ephemeral sequence they
can be written through (`sek-iter-writable-segment`), which is what makes
`sek-fill!` and `sek-blit!` cost O(size + K log_K n) instead of a tree descent
per element.

Any update to an ephemeral sequence invalidates every iterator on it, and using
an invalidated iterator raises rather than reading stale storage. The check is
a version-number comparison; `sek-configure!` can turn it off.

## Operations

Beyond the core, the library has the operation surface of the OCaml library's
`SEK` signature. These accept either flavour, and the ones that build a
sequence return the flavour they were given:

`sek-length` `sek-empty?` `sek-ref` `sek-first` `sek-last` `in-sek`
`sek-for-each` `sek-for-each/index` `sek-segments-for-each` `sek-fold-left`
`sek-fold-right` `sek->list` `sek->vector` `sek-find` `sek-find-index`
`sek-find-map` `sek-for-all?` `sek-exists?` `sek-member?` `sek-memq?`
`sek-map` `sek-map/index` `sek-filter` `sek-filter-map` `sek-partition`
`sek-reverse` `sek-append*` `sek-append-map` `sek-sort` `sek-uniq` `sek-merge`
`sek-sub` `sek-take` `sek-drop` `sek-copy` `sek-for-each2` `sek-fold-left2`
`sek-fold-right2` `sek-map2` `sek-zip` `sek-unzip` `sek-for-all2?`
`sek-exists2?` `sek-equal?` `sek-compare` `sek-fill!` `sek-blit!`
`sek-segments-for-each2` `build-pseq` `build-eseq` `make-pseq`
`sequence->pseq` `sequence->eseq`

Ephemeral sequences also have the reference library's in-place structural
operations, which consume the sequences they are given: `eseq-append!`
`eseq-concat!` `eseq-split!` `eseq-carve!` `eseq-take!` `eseq-drop!`
`eseq-assign!` `eseq-clear!`.

## Costs

`n` is the length, `K` the chunk capacity, `T` the short-sequence threshold,
and `N` a bound on the length an ephemeral sequence reaches.

| operation | persistent | ephemeral |
| --- | --- | --- |
| `ref` | O(K log_K n) | O(K log_K n) |
| `set` | O(K log_K n) | O(K log_K n), O(log_K n) once the path is owned |
| `pop` | O(log_K n) | O(log_K N) amortized |
| `push` | O(K log_K n) | O(log_K N) amortized |
| `append`, `split` | O(K log_K n + log²_K n) | — |
| `edit` | O(K) | — |
| `snapshot` | — | O(1) |

`ref` and `set` drop to O(log_K n) on *packed* chunks, which is every chunk of
a sequence built without concatenation.

## Testing

Following §4.2, the tests build long randomized scenarios over a pool of
sequences that descend from one another by `snapshot` and `edit`, compare every
version against a list-based reference implementation, and run the Appendix A
validator after every single step. That last part is what catches ownership
bugs: an in-place write to a chunk some snapshot still observes corrupts that
snapshot and nothing else notices.

```
raco test sek/
```

The suite runs at chunk capacities from 2 upwards, so the trees get deep and
the cascading cases in `push`, `pop`, `split` and `merge` are actually reached.

Every module of the library is compiled with `(#%declare #:unsafe)`, so nothing
here raises by accident: a struct accessor handed the wrong kind of value reads
whatever is at that offset rather than complaining. Every function a caller
outside the library can reach therefore checks its arguments with an explicit
`unless`, and `sek/tests/error-tests.rkt` is what holds that line — it calls the
public surface with wrong types and out-of-range indices across about 150 cases
and insists on an exception. It is worth running whenever an entry point is
added, because a missing check there is not a bad error message, it is a read of
arbitrary memory.

Separately, `conformance/` checks this library against the OCaml one directly:
a generated script of several hundred operations is run by both, and the two
traces — the result of every command plus the full contents of a dozen
sequences after it — must be identical.

```
cd conformance && ./build.sh && ./run.sh 1 8 700
```

Ten configurations of the tunable settings, eight seeds each, all match.

## Benchmarks

`bench/` runs the scenarios from the paper and from the OCaml library's own
benchmark suite — stack, queue, traversal, random access, hops, update,
construction, concat, split, filter, fill — plus transient scenarios, which the
reference's suite does not measure. Contenders are Racket's `treelist` and
`mutable-treelist`, `gvector` (the revised one from
[racket/data#34](https://github.com/racket/data/pull/34)), lists, a mutable box
holding a list, a hand-rolled growable `array` as the floor, and the OCaml
implementation itself. `bench/bm.rkt` is the benchmark from that gvector PR,
with sek rows added.

Every structure is measured on every operation it can perform at all, including
where that costs it a walk of the whole sequence — a cons list does have a back,
it is just Θ(n) away. A dash means only that the operation does not exist.
Rows that would otherwise be quadratic are measured as a bounded burst against
a sequence already at length n; the `burst-check` scenario runs both paths side
by side so the size of that substitution is visible rather than asserted.

`bench/external.rkt` adds the shapes that the *other* chunked-sequence
libraries measure, transcribed from their own suites: Scala's
`VectorBenchmark2` (the JMH suite behind the 2.13 `Vector` rewrite), immer's
`benchmark/vector` (including its transient `_move` and `_mut` variants), and
bifurcan's list benchmarks, which `clojure/core.rrb-vector` reuses for its
published numbers.

```
cd bench && ./run.sh                    # this library
./run.sh --external                     # the borrowed scenarios
./build-ocaml.sh && ./run.sh --ocaml    # the reference, same scenarios
racket -y bm.rkt                        # the gvector PR benchmark, plus sek
racket -y nqueens.rkt                   # the classic Scheme nqueens benchmark

./run.sh --all --json results.json && racket -y report.rkt results.json report.html
```

`bench/report.rkt` draws the recorded results as one static HTML page, charts
included; no scripts and no network.

`bench/README.md` has the tables and the analysis. The short version, at a
million elements, nanoseconds per operation:

| | eseq | treelist | mutable-treelist | gvector | list |
| --- | ---: | ---: | ---: | ---: | ---: |
| push/pop at the back | **5.5** | 45.2 | 51.8 | 28.5 | — |
| push/pop at the front | **5.6** | 205.3 | 209.0 | — | 2.5 |
| queue (back, front) | **5.5** | 69.6 | 75.4 | — | — |
| traversal, per element | 1.5 | 1.8 | 1.8 | 1.3 | 1.3 |
| `ref` at a random index | 39.1 | **8.7** | 13.5 | 10.9 | — |
| `set` at a random index | 35.0 | 203.3 | **16.2** | 10.4 | — |
| construction, per element | **2.3** | 47.5 | 51.6 | 17.7 | 40.7 |
| filter, per element | **3.2** | 16.3 | — | — | 4.7 |
| one snapshot | **24.9** | — | 1484152 | — | — |

The ends are flat in the length of the sequence and indifferent to which end
you use, which is the whole point; indexing is what the design gives up, and it
costs about 4× a treelist. Snapshots are the headline: `eseq-snapshot` does not
depend on the length of the sequence, while `mutable-treelist-snapshot`
copies — 1.5 ms at a million elements against 24.9 ns. In a round trip that
changes one element between snapshots, sek is four orders of magnitude ahead; a
treelist catches up only once tens of thousands of writes amortize its copy.

The borrowed suites agree, and sharpen two points. Scala's `vApprepend` —
alternate a push at each end — is the clearest win in the whole set: 5.8 ns
against a treelist's 146.5 at 10^5, and flat where the treelist's grows with
depth. And Scala's `vApplySequential` shows what indexing costs and what it
need not cost: an ascending walk through `pseq-ref` takes 16.4 ns against the
treelist's cached 5.9, but the same walk through `in-pseq` takes 1.57 — 3.7×
faster than the treelist, faster than an indexed read on a growable array, and
level with a raw vector. The answer to sequential access in this library is a
first-class iterator, and it is a better answer than a display; it is just not
spelled `ref`. Bulk element-wise work is 13× ahead of a treelist for the same
reason (`map` 3.5 ns an element against 46.9), and immer's `push_move`
reproduces its headline (5.4 ns per element against 47.5 for repeated
persistent `treelist-add`).

Slicing used to be the thing this design gives up — `slice`, `take` and `split`
all read several times a treelist, and `split` 4.6× the OCaml reference. That
turned out to be wrong, and reading the reference next to this code found seven
places where the port copied a chunk where the reference shares a view of it,
rescanned a weight it already knew, or built a half of a split it then threw
away. Closing them took `split` to 0.66× the reference and 0.74× a treelist, `take`
from 600 ns to 86, `slice` from 1073 to 257, and made persistent `set` twice as
fast as the reference. What remains is a two-sided slice, where the density
invariant is real work an RRB tree does not do, and that is the honest residue.
`bench/README.md` has the full account.

`bench/nqueens.rkt` runs the classic Scheme nqueens benchmark — 8 queens,
10000 repetitions — over each structure, both as the original pair-list
program with the operations swapped out and as a backtracking search over one
mutable stack. Pairs win it outright, as they should for a search that never
holds more than eight elements; among the sequence structures `pseq` is
fastest at 3.4 s against `treelist`'s 4.2 s, and the mutable ones pay 2–4×
more because every `cons` and `append` has to copy where a persistent
structure shares.

Against the OCaml implementation, all fifteen scenarios are faster than the
reference, on a runtime with a garbage collector against native code compiled
with flambda: `set` at a random index costs 0.36× what it costs there
ephemerally and 0.54× persistently, splitting 0.66×, indexing 0.69×,
construction 0.72×, persistent push/pop 0.63×, traversal by fold 0.64×,
`filter` 0.82×, concatenation 0.87×, ephemeral push/pop 0.94×. Snapshotting
after every push is 10× *faster* here, because this implementation shares the
end chunks where the reference copies them.

The last row to cross over was `filter`, and it did not cross by getting
faster. The scenario's predicate was written `(zero? (modulo x 3))` where the
reference writes `x mod 3 = 0` on an unboxed `int`, and on this runtime those
are not the same operation: the generic one costs about a nanosecond an element
more, a third of the scenario, and none of it is either sequence's doing. The
scenario now reports both spellings.

None of that came from writing different Racket. Four of those numbers used to
be embarrassing — `split` read 4.6×, `construction` 3.3×, `concat` 2.5×,
`filter` 1.7× — and in every case the port was doing something the reference
does not. It copied a chunk where the reference shares a view of it; it pushed
one element at a time where the reference fills a chunk; it called
contract-checked list functions on three-element lists; it paid three closure
calls an element where the loop could be the caller's own. The exception that
proves the rule is the one that *was* about Racket: every struct was paying a
walk of its type's ancestry vector on each field access, which `#:sealed`
collapses to one comparison, and the innermost modules had implicit checks that
`(#%declare #:unsafe)` removes — two lines of declaration, found by reading the
generated code, worth `pseq-ref` 35.6 ns to 22.2 and `eseq` push-back 8.8 to
4.7. A second reading of the generated code, this time of the machine code
rather than the intermediate form, found six more of the same kind: two calls
on the push path that inlining removes, a capacity read that was three
dependent loads, an ownership test that went through generic `eqv?` because an
id was a counter, an index guard that made the compiler re-test the same tag
three times, arithmetic on struct fields that carried a fixnum guard and an
overflow check apiece, and a fold that walked a segment as an offset plus a
counter.

Measuring `mutable-treelist` on the operations it had previously been left out
of turned up a bug in `racket/mutable-treelist`, since fixed: shortening one at
the front and then copying or snapshotting it raised `vector-length: contract
violation`, because `treelist-copy-for-mutable` assumed every node was a bare
vector and a `treelist-drop` leaves nodes carrying a size vector. `bench/README.md`
has the diagnosis; the fix and its regression test are in the Racket tree.

## Deviations from the paper

Agreement with the reference is not a claim, it is checked: `conformance/`
runs the OCaml library and this one on the same generated script and compares
the traces — the result of every operation and the full contents of a dozen
sequences after each one. See `conformance/README.md` for the harness, the
operation-by-operation mapping between the two APIs, and what has been
checked. The remaining differences:

* Sequences are parameterized by neither an element type nor a `default` value;
  logically empty slots get a private sentinel instead, which removes the
  `default` argument the OCaml library threads through every constructor.
* The two flavours are one set of names rather than two parallel modules: an
  operation that builds a sequence returns the flavour it was given.
* `sek-append*` builds a fresh result, where the OCaml `flatten` clears the
  sequence of sequences and every sequence in it.
* `sek-sort` is stable, so it covers `stable_sort` too.
* `sek-iter-reach!` reuses the cursor's position when the target is in the run
  or the chunk it is already on; beyond that it descends from the root, where
  the reference can also search from the current position inside the middle
  sequence.
* `One` and `Short` (§3.5) are unified into one vector representation, used
  only at the top of the structure, as in the authors' implementation.
* `pseq-edit` shares the front and back chunks instead of copying them, so it
  is O(1) rather than O(K); the copy happens on the first write, if any.
* `eseq-snapshot` folds the inner chunks into the middle sequence, as the
  OCaml library does, so it is O(K log_K n) in the worst case rather than the
  O(1) of Figure 16.
* A `#:short-threshold` of 0 works here and not there.
* As in the paper, monotonic in-place updates make the persistent flavour
  unsafe to share across threads without synchronization.
