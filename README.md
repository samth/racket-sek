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
| `sek/bench.rkt` | §4.3 | the push/pop benchmark |
| `sek/tests/` | §4.2 | randomized differential testing against list/vector references |

The paper describes the data structure; the last three rows follow the
authors' OCaml library, [Sek](https://gitlab.inria.fr/fpottier/sek/), where the
paper is silent.

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
`build-pseq` `build-eseq` `make-pseq`

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

## Benchmark

`sek/bench.rkt` runs the scenario of §4.3 — repeat `n` pushes followed by `n`
pops until 2 million pushes have happened — against Racket's `gvector`, a
mutable box holding a list, and an immutable list. On one machine
(Racket CS 9.3, ns per push/pop pair):

| n | eseq | gvector | box of list | pseq | immutable list |
| --- | --- | --- | --- | --- | --- |
| 10 | 18.5 | 38.6 | 5.4 | 21.6 | 2.5 |
| 1 000 | 18.5 | 37.8 | 4.1 | 25.3 | 2.1 |
| 100 000 | 19.1 | 37.6 | 4.1 | 25.7 | 2.3 |
| 1 000 000 | 19.3 | 55.3 | 8.5 | 27.7 | 5.2 |

The shape matches the paper's Figures 17 and 18: Sek beats a growable vector by
about 2x and stays flat as the sequence grows, while structures that pay for
locality (`gvector`) or allocation (lists) degrade at a million elements.
Racket's lists remain the fastest way to use a sequence *as a stack* — but they
are only a stack.

The same file also measures what the iterators and segments buy. Summing a
million-element persistent sequence:

| how | ns per element |
| --- | --- |
| `sek-fold-left` (segments) | 2.1 |
| iterator, one element at a time | 11.3 |
| `pseq-ref` at each index | 48.0 |
| Racket vector | 0.6 |
| Racket list | 1.2 |

Handing out whole runs of storage rather than stepping per element is worth
5x, and worth 23x over indexing — which is the whole reason the OCaml library
has iterators.

## Deviations from the paper

* Sequences are parameterized by neither an element type nor a `default` value;
  logically empty slots get a private sentinel instead, which removes the
  `default` argument the OCaml library threads through every constructor.
* The two flavours are one set of names rather than two parallel modules: an
  operation that builds a sequence returns the flavour it was given.
* `eseq-append!` leaves its second argument alone and `eseq-split` leaves its
  argument alone, where the OCaml versions clear them. Both go through
  `eseq-snapshot` and `pseq-edit` underneath.
* `sek-iter-reach!` always descends from the root; the OCaml version can start
  from the iterator's current position when the target is nearby. Jumps that
  stay inside one segment are still O(1).
* `One` and `Short` (§3.5) are unified into one vector representation, used
  only at the top of the structure, as in the authors' implementation.
* `pseq-edit` shares the front and back chunks instead of copying them, so it
  is O(1) rather than O(K); the copy happens on the first write, if any.
* `eseq-snapshot` folds the inner chunks into the middle sequence, as the
  OCaml library does, so it is O(K log_K n) in the worst case rather than the
  O(1) of Figure 16.
* As in the paper, monotonic in-place updates make the persistent flavour
  unsafe to share across threads without synchronization.
