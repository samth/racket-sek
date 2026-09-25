# Conformance with the reference implementation

This directory checks this library against the authors' OCaml library,
[Sek](https://gitlab.inria.fr/fpottier/sek/) — not by reading it, but by
running both on the same input and comparing.

```sh
./build.sh          # clone and build the OCaml reference (needs ocamlfind)
./run.sh 1 8 700    # seeds 1..8, 700 generated commands each
```

`gen.rkt` generates a random script over six ephemeral and six persistent
slots, maintaining a list model so that structural commands get in-range
arguments — and deliberately out-of-range ones for the operations whose
failure both sides report. `driver.ml` and `driver.rkt` execute the script and
print a trace: the result of each command, followed by the full contents of
all twelve slots. The traces must be byte-identical.

Both drivers read the tree shape from the environment, so one script can be
replayed at several capacities:

```sh
SEK_LEAF=2 SEK_NODE=2 SEK_THRESHOLD=2 ./run.sh 1 8 700
```

## What has been checked

Ten configurations — `(leaf, node, threshold)` of (128,16,32), (2,2,2),
(3,2,2), (4,4,4), (5,3,5), (8,4,6), (16,8,12), (32,4,8), plus
`overwrite_empty_slots = false` and `check_iterator_validity = false` — each at
eight seeds of ~820 commands. Every trace matched.

The scripts cover push, pop, peek, get, set, length, clear, assign, copy in
both modes, concat, append, split, carve, take, drop, sub, fill, blit,
snapshot, snapshot-and-clear, edit, iteration forward and backward, iteration
through a first-class iterator, `reach` to arbitrary indices including the
sentinels, writing through an iterator, fold in both directions, map, mapi,
filter, filter_map, rev, uniq, sort, merge, partition, zip, find, for_all,
exists, mem, equal, compare, and the runtime validators.

Note that the reference does not support `threshold = 0`: it still builds a
`Short` node for a two-element sequence, which its own validator rejects
(`ShortPersistentSequence.ml`, the assertion `2 <= n && n <= threshold`). This
library handles `#:short-threshold 0` — it simply never uses the compact
representation — so that configuration is tested on the Racket side only.

## Operation mapping

The OCaml library is two parallel modules, `Ephemeral` and `Persistent`, with
the same names in each. Here they are one set of names: an operation that
builds a sequence returns the same flavor it was given, so `sek-map` covers
both `Ephemeral.map` and `Persistent.map`.

| OCaml | this library |
| --- | --- |
| `create d` / `make d n v` / `init d n f` | `make-eseq`, `(make-eseq n v)`, `build-eseq`; `empty-pseq`, `make-pseq`, `build-pseq` |
| `default s` | — (no default value; see below) |
| `length` / `is_empty` | `sek-length`, `eseq-length`, `pseq-length` / `sek-empty?` … |
| `clear` | `eseq-clear!` |
| `copy ~mode` | `eseq-copy #:mode` |
| `assign` | `eseq-assign!` |
| `push side` | `eseq-push-front!` / `eseq-push-back!`, `pseq-push-front` / `pseq-push-back` |
| `pop side` / `pop_opt` | `eseq-pop-front!` … / guard with `sek-empty?` |
| `peek side` / `peek_opt` | `eseq-first` / `eseq-last`, `pseq-first` / `pseq-last` / guard with `sek-empty?` |
| `get` / `set` | `sek-ref`, `eseq-set!`, `pseq-set` |
| `concat` | `eseq-concat!`, `pseq-append` |
| `append side` | `eseq-append!` |
| `split` | `eseq-split!`, `pseq-split` |
| `carve side` | `eseq-carve!` |
| `take side` / `drop side` | `eseq-take!` / `eseq-drop!` (in place); `sek-take` / `sek-drop` (functional) |
| `sub` | `sek-sub` |
| `iter dir` / `iteri dir` | `sek-for-each` / `sek-for-each/index` |
| `iter_segments dir` | `sek-segments-for-each` |
| `fold_left` / `fold_right` | `sek-fold-left` / `sek-fold-right` |
| `to_list` / `to_array` / `to_seq dir` | `sek->list` / `sek->vector` / `in-sek` |
| `of_list` / `of_array` / `of_seq` | `list->pseq`, `list->eseq`, `vector->pseq`, `sequence->pseq`, `sequence->eseq` |
| `of_list_segment` / `of_array_segment` / `of_seq_segment` | `(sequence->pseq s n)`, `(sequence->eseq s n)` |
| `find dir` / `find_opt` / `find_map` | `sek-find` (returns `#f`), `sek-find-map`; `sek-find-index` when an element may be `#f` |
| `for_all` / `exists` / `mem` / `memq` | `sek-for-all?` / `sek-exists?` / `sek-member?` / `sek-memq?` |
| `map` / `mapi` / `rev` | `sek-map` / `sek-map/index` / `sek-reverse` |
| `zip` / `unzip` | `sek-zip` / `sek-unzip` |
| `filter` / `filter_map` / `partition` | `sek-filter` / `sek-filter-map` / `sek-partition` |
| `flatten` / `flatten_map` | `sek-append*` (consumes an ephemeral argument, as `flatten` does) / `sek-append-map` |
| `iter2 dir` / `iter2_segments` | `sek-for-each2` / `sek-segments-for-each2` |
| `fold_left2` / `fold_right2` / `map2` | `sek-fold-left2` / `sek-fold-right2` / `sek-map2` |
| `for_all2` / `exists2` | `sek-for-all2?` / `sek-exists2?` |
| `equal` / `compare` | `sek-equal?` / `sek-compare` |
| `sort` / `stable_sort` / `uniq` / `merge` | `sek-sort` (stable) / `sek-uniq` / `sek-merge` |
| `fill` / `blit` | `sek-fill!` / `sek-blit!` |
| `format` / `check` | printed by `write`; `sek-validate-pseq` / `sek-validate-eseq` |
| `snapshot` / `snapshot_and_clear` / `edit` | `eseq-snapshot` / `eseq-snapshot-and-clear!` / `pseq-edit` |
| `front` / `back` / `forward` / `backward` | the symbols `'front` `'back` `'forward` `'backward` |

### Iterators

| OCaml | this library |
| --- | --- |
| `Iter.create dir` | `sek-iterator`; `sek-iterator-at-sentinel` also exists |
| `Iter.reset dir` / `copy` | `sek-iter-reset!` / `sek-iter-copy` |
| `sequence` / `length` / `index` / `finished` | `sek-iter-sequence` / `-length` / `-index` / `-finished?` |
| `get` / `get_opt` | `sek-iter-get` / `sek-iter-get*` |
| `move dir` / `jump dir` / `reach` | `sek-iter-move!` / `sek-iter-jump!` / `sek-iter-reach!` |
| `get_and_move dir` / `get_and_move_opt` | `sek-iter-get-and-move!` / `sek-iter-get-and-move*!` |
| `get_segment dir` / `get_segment_opt` | `sek-iter-segment` / `sek-iter-segment*` |
| `get_segment_and_jump dir` / `…_opt` | `sek-iter-segment-and-jump!` / `sek-iter-segment-and-jump*!` |
| `is_valid` / `check` | `sek-iter-valid?` / `sek-iter-check` |
| `set` / `set_and_move dir` | `sek-iter-set!` / `sek-iter-set-and-move!` |
| `get_writable_segment dir` / `…_opt` | `sek-iter-writable-segment` / `sek-iter-writable-segment*` |
| `get_writable_segment_and_jump dir` / `…_opt` | `sek-iter-writable-segment-and-jump!` / `…*!` |
| `Segment.is_valid` / `is_empty` / `iter` / `iter2` | `segment-valid?` / `segment-empty?` / `segment-for-each` / `segment-for-each2` |

### Deliberate differences

* **No `default` value.** Every OCaml constructor takes one, because the
  library must initialize array slots without knowing the element type. Here
  a private sentinel does that job, so the argument does not appear.
* **`sort` is stable**, so it also serves as `stable_sort`.
* **`sek-sub` on a persistent sequence always shares.** The OCaml version
  copies a slice of at most T elements and shares a longer one; splitting is
  faster here at every size, from 16 elements (0.002 ms against 0.003) to half
  a million (0.002 ms against 2.1), and `pseq-take` normalizes a short result
  into the compact vector anyway. Same result either way.
* **`pseq-edit` and `eseq-snapshot` do not copy the front and back chunks**,
  where the OCaml versions do. Observationally identical, and measurably
  better: a loop that snapshots after every push runs ten times faster this
  way (see `bench/README.md`), because the next push usually extends a chunk
  monotonically and copies nothing.
* **`threshold = 0`** is supported here and not there, as described above.
