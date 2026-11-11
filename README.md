# sek — catenable, splittable, transient sequences for Racket

A Racket implementation of

> Arthur Charguéraud and François Pottier.
> **A Catenable, Splittable, Transient Sequence Data Structure.**
> Proc. ACM Program. Lang. 10, ICFP, Article 308 (August 2026).
> <https://doi.org/10.1145/3828706>

The paper is open access, under CC BY 4.0:
[PDF](https://dl.acm.org/doi/pdf/10.1145/3828706).

A *transient* data structure is an ephemeral data structure, a persistent one,
and fast conversions between them. You take a persistent snapshot in
logarithmic time when you need one, and keep the speed of in-place updates
everywhere else.

```racket
(require sek)

(define p (list->psek '(1 2 3 4 5)))
(psek->list (psek-push-front p 0))   ; '(0 1 2 3 4 5)
(psek->list p)                       ; '(1 2 3 4 5) — unchanged

(define e (psek-edit p))             ; switch to in-place updates
(esek-push-back! e 6)
(esek-set! e 0 'a)
(define q (esek-snapshot e))         ; and back
(psek->list q)                       ; '(a 2 3 4 5 6)
(psek->list p)                       ; '(1 2 3 4 5) — still unchanged
```

## What is here

The library's interface is `sek-lib/sek/main.rkt`, and everything behind it is
in `sek-lib/sek/private/`.  Its documentation is in `sek-doc/`, and its tests,
benchmarks and conformance harness in `sek-test/tests/sek/`.

| file | paper | contents |
| --- | --- | --- |
| `sek-lib/sek/private/config.rkt` | §4.1 | chunk capacities and the short-sequence threshold |
| `sek-lib/sek/private/chunk.rkt` | §3.3, Fig. 11–13 | transient chunks: circular support + view + ownership id |
| `sek-lib/sek/private/ptree.rkt` | §3.1–3.2 | the Sek tree: push, pop, get, set, split, concat, merge |
| `sek-lib/sek/private/persistent.rkt` | §3.5 | `psek`, including the compact representation of short sequences |
| `sek-lib/sek/private/ephemeral.rkt` | §3.6 | `esek`, including the inner front/back chunks |
| `sek-lib/sek/private/iterate.rkt` | — | a lazy left-to-right reader, which `equal?` and `for` use only when `generic.rkt` is not loaded |
| `sek-lib/sek/private/segment.rkt` | `Segment` | runs of contiguous storage, used internally |
| `sek-lib/sek/private/iterator.rkt` | `Iterator` (§5.4) | first-class cursors, with invalidation checking |
| `sek-lib/sek/private/generic.rkt` | `Generic`, `PublicSignature` | the derived operation surface |
| `sek-lib/sek/private/check.rkt` | Appendix A, Fig. 19 | the runtime validation function |
| `sek-lib/sek/private/unchecked.rkt` | — | the only module with unchecked operations, and the checked and jitter builds |
| `sek-test/tests/sek/` | §4.2 | randomized differential testing against list/vector references |
| `sek-test/tests/sek/bench/` | §4.3 | benchmarks against Racket's sequences and against the OCaml library |
| `sek-test/tests/sek/conformance/` | — | differential testing against the OCaml library itself |
| `sek-test/tests/sek/stress/` | — | a fuzzer that races threads on shared sequences |
| `sek-doc/THREADS.md` | — | what threads may do with a sequence, and why we believe it is safe |

The paper describes the data structure; segments, iterators, the derived
operations and the conformance harness follow the authors' OCaml library,
[Sek](https://gitlab.inria.fr/fpottier/sek/), where the paper is silent.

## The three ideas

**Ownership identifiers** (§2.4, from L'orange). Every chunk carries an id, and
every ephemeral sequence carries one. If they match, the chunk is not shared
with anybody and can be written in place. `esek-snapshot` hands the sequence a
*fresh* id, so every chunk in it instantly stops being recognizable as owned —
which is why a snapshot need not copy the sequence and yet is safe.

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

## Iterators

Repeated `psek-ref` costs O(n log_K n) to walk a sequence. An iterator walks it
in O(n), because stepping stays inside one run of contiguous storage most of
the time:

```racket
(define it (sek-iterator s))          ; at index 0; -1 and the length are sentinels
(let loop ()
  (unless (sek-iter-finished? it)
    (displayln (sek-iter-get it))
    (sek-iter-move! it)
    (loop)))
```

The public interface is eight names: `sek-iterator`, `sek-iter?`,
`sek-iter-index`, `sek-iter-finished?`, `sek-iter-get`, `sek-iter-move!`,
`sek-iter-reach!` and `sek-iter-set!`.  Internally an iterator can also hand
out the whole run it is sitting on, as a *segment* — a vector, a start index
and a length — and that is how `in-sek`, the folds and everything in the next
section are built: a traversal becomes a series of tight vector loops.
Writable segments are what make `esek-fill!`, `esek-copy!` and `esek-map!` cost
O(size + K log_K n) instead of a tree descent per element.  Segments are not
exported.

Any update to an ephemeral sequence invalidates every iterator on it, and using
an invalidated iterator raises rather than reading stale storage.  The check is
a version-number comparison.

## Operations

Beyond the core, the library has the operation surface of the OCaml library's
`SEK` signature. These accept either flavor, and the ones that build a
sequence return the flavor they were given:

`sek-length` `sek-empty?` `sek-ref` `sek-first` `sek-last` `in-sek`
`sek-for-each` `sek-for-each/index` `sek-fold-left` `sek-fold-right`
`sek->list` `sek->vector` `sek-find` `sek-find-index` `sek-find-map`
`sek-for-all?` `sek-exists?` `sek-member?` `sek-memq?` `sek-index-of`
`sek-map` `sek-map/index` `sek-filter` `sek-filter-map` `sek-partition`
`sek-reverse` `sek-append*` `sek-append-map` `sek-sort` `sek-uniq` `sek-merge`
`sek-sub` `sek-take` `sek-drop` `sek-take-right` `sek-drop-right`
`sek-insert` `sek-delete` `sek-copy` `sek-for-each2` `sek-fold-left2`
`sek-fold-right2` `sek-map2` `sek-zip` `sek-unzip` `sek-for-all2?`
`sek-exists2?` `sek-equal?` `sek<?`

Each flavor has its constructors (`make-psek`, `build-psek`,
`sequence->psek`, `for/psek`, `for*/psek` and the same for `esek`), and
ephemeral sequences have `esek-fill!`, `esek-copy!`, `esek-map!` and
`esek-map/index!`.

Ephemeral sequences also have the reference library's in-place structural
operations, which consume the sequences they are given: `esek-append!`
`esek-concat!` `esek-split!` `esek-carve!` `esek-take!` `esek-drop!`
`esek-assign!` `esek-clear!`.

## Costs

`n` is the length, `K` the chunk capacity, `T` the short-sequence threshold,
and `N` a bound on the length an ephemeral sequence reaches.

| operation | persistent | ephemeral |
| --- | --- | --- |
| `ref` | O(K log_K n) | O(K log_K n) |
| `set` | O(K log_K n) | O(K log_K n), O(log_K n) once the path is owned |
| `pop` | O(log_K n) | O(log_K N) amortized |
| `push` | O(K log_K n) | O(log_K N) amortized |
| `append`, `split` | O(K log_K n + log²_K n) | O(K log_K n + log²_K n) |
| `edit` | O(1), O(K) at length ≤ T | — |
| `snapshot` | — | O(K log_K n) |

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
raco test sek-test/tests/sek/
```

The suite runs at chunk capacities from 2 upwards, so the trees get deep and
the cascading cases in `push`, `pop`, `split` and `merge` are actually reached.

The accessors of the library's own structures do not check their argument (see
`sek/private/unchecked`), so nothing here raises by accident: an accessor
handed the wrong kind of value reads whatever is at that offset rather than
complaining.  Every function a caller outside the library can reach therefore
checks its arguments with an explicit `unless`, and
`sek-test/tests/sek/error-tests.rkt` is what holds that line — it calls the
public surface with wrong types and out-of-range indices across about 150 cases
and insists on an exception. It is worth running whenever an entry point is
added, because a missing check there is not a bad error message, it is a read of
arbitrary memory.

Separately, `sek-test/tests/sek/conformance/` checks this library against the
OCaml one directly: a generated script of several hundred operations is run by
both, and the two traces — the result of every command plus the full contents
of a dozen sequences after it — must be identical.

```
cd sek-test/tests/sek/conformance && ./build.sh && ./run-all.sh 1 8 700
```

`run-all.sh` runs the seeds at ten configurations of the tunable settings.

## Benchmarks

`sek-test/tests/sek/bench/` runs the scenarios from the paper and from the
OCaml library's benchmark suite, scenarios for snapshots and transience, the
shapes that Scala's, immer's and bifurcan's suites measure, and several whole
programs.  The contenders are Racket's `treelist` and `mutable-treelist`,
`gvector`, lists, a hand-written growable `array` as the floor, and the OCaml
library itself.

```
cd sek-test/tests/sek/bench && ./run.sh # this library
./run.sh --external                     # the borrowed scenarios
./run.sh --workloads                    # whole programs
./build-ocaml.sh && ./run.sh --ocaml    # the reference, same scenarios
```

The README there describes each program and has the full results.  At a
million elements, on one core of a Xeon Platinum 8268 with Racket CS 9.3.0.8,
in nanoseconds per operation unless a unit is given:

| | esek | psek | treelist | mutable-treelist | gvector | list |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| push and pop at the back | 19.6 | 30.5 | 105 | 122 | 114 | 41 ms |
| push and pop at the front | 18.0 | 28.3 | 513 | 529 | 2.6 ms | 7.4 |
| queue: push back, pop front | 17.3 | 29.0 | 169 | 187 | 1.3 ms | 18 ms |
| traversal, per element | 5.7 | 4.5 | 6.8 | 6.8 | 9.9 | 4.4 |
| `ref` at a random index | 90.5 | 86.7 | 37.3 | 51.4 | 85.1 | 0.9 ms |
| `set` at a random index | 96.0 | 966 | 416 | 51.6 | 56.9 | 22 ms |
| construction, per element | 7.7 | 8.0 | 110 | 123 | 124 | 82.1 |
| `map`, per element | 11.2 | 10.3 | 107 | 39.8 | 89.1 | 24.8 |
| append two halves | 1180 | 801 | 1978 | 3.2 ms | 60 ms | 12 ms |
| split at a random index | 422 | 234 | 447 | 15 ms | 54 ms | 9.4 ms |
| one change, then a snapshot | 70.2 | — | — | 3.4 ms | — | — |

Pushing and popping cost the same at either end and at every length.
Indexing is what the design gives up: a random `ref` costs about 2.4 times a
treelist's.  A sequential read should go through `in-sek` or an iterator,
which costs 3.3 ns an element where an ascending walk by `psek-ref` costs
41.5 and one by `treelist-ref` 23.8.  `esek-snapshot` costs the same at every
length, where `mutable-treelist-snapshot` copies the sequence.

Against the OCaml library on the same machine, this library takes between
0.12 and 0.9 times as long on fourteen of nineteen scenarios, among them
snapshots, `ref`, `set`, split, concat, construction and every persistent
push and pop.  It takes 1.03 to 1.19 times as long on ephemeral push and pop
and on traversal through an iterator, and 1.6 times as long on `filter`, where
the predicate's division dominates.

## Deviations from the paper

This library follows the paper, and where the paper is silent, the authors'
OCaml library. Agreement with the reference is not a claim, it is checked:
`sek-test/tests/sek/conformance/` runs the OCaml library and this one on the
same generated script and compares the traces — the result of every operation
and the full contents of a dozen sequences after each one. See the README there
for the harness, the operation-by-operation mapping between the two APIs, and
the configurations it runs at. The remaining differences:

* Sequences are parameterized by neither an element type nor a `default` value;
  logically empty slots get a private sentinel instead, which removes the
  `default` argument the OCaml library threads through every constructor.
* The two flavors are one set of names rather than two parallel modules: an
  operation that builds a sequence returns the flavor it was given.
* `sek-sort` is stable, so it covers `stable_sort` too; `sort` there makes no
  such promise.  Given an ephemeral sequence, `sek-sort` returns a new one,
  where `Ephemeral.sort` sorts in place.
* `sek<?` stands in for `compare`: it takes a less-than procedure, as
  `sek-sort` does, and returns a boolean, where `compare` takes and returns
  integer comparisons.
* `sek-sub` on a persistent sequence always shares the sequence's chunks,
  where the OCaml library copies a slice of at most `T` elements.
* `sek-iter-reach!` reuses the cursor's position when the target is in the run
  or the chunk it is already on; beyond that it descends from the root, where
  the reference can also search from the current position inside the middle
  sequence.
* `One` and `Short` (§3.5) are unified into one vector representation, used
  only at the top of the structure, as in the authors' implementation.
* `psek-edit` shares the front and back chunks instead of copying them, so it
  is O(1) rather than O(K), except on a sequence of at most `T` elements; the
  copy happens on the first write, if any.  `esek-copy` in `'share` mode is
  likewise O(1).
* `esek-snapshot` folds the inner chunks into the snapshot's middle sequence,
  so it is O(K log_K n) in the worst case rather than the O(1) of Figure 16.
  The OCaml library folds them into the sequence's own middle sequence, and so
  does creating an iterator there; here neither changes the sequence, so that
  several threads may read one ephemeral sequence at once.
* The settings — the chunk capacities and the short-sequence threshold — are
  global, where the OCaml library fixes them when its functor `Make` is
  applied, and the chunk capacity takes one value for the leaves and one for
  every level above them, where the OCaml library takes a function of the
  depth.  They are not part of the public interface: the tests change them
  through `(submod sek config)`.  A short-sequence threshold of 0 works here
  and not there.
* A persistent sequence may be shared between threads.  A monotonic in-place
  update claims its slot with a compare-and-swap on the support's head and
  size, which share one field for the purpose; the OCaml library writes the
  slot without synchronization, so there two threads that extend one
  sequence at once can overwrite each other's element.
