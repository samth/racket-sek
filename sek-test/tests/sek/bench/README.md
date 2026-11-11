# Benchmarks

```sh
./run.sh                     # this library, one scenario per process
./run.sh --external          # the scenarios borrowed from other libraries
./run.sh --workloads         # whole programs rather than single operations
./run.sh --all               # all three
./run.sh --quick             # smaller sizes
racket -y main.rkt stack     # a single scenario

./build-ocaml.sh             # build the OCaml reference (needs ocamlfind)
./run.sh --ocaml             # the same scenarios, run against it

racket -y bm.rkt             # the benchmark from racket/data PR #34, plus sek
racket -y nqueens.rkt        # the classic Scheme nqueens benchmark
racket -y scaling.rkt        # one operation at four sizes, with a fitted slope

./run.sh --all --json results.json               # record every table
racket -y nqueens.rkt --json results.json        # append
racket -y report.rkt results.json report.html    # draw it
```

The results files and the report are outputs, and git ignores them.

## What each program measures

* `main.rkt` follows the benchmarks that accompany the OCaml library (stack,
  queue, traversal, random access, hops, update, construction, concat, split,
  filter, fill) and Figures 17 and 18 of the paper, and adds scenarios for
  snapshots and transience.
* `external.rkt` transcribes the shapes that other chunked-sequence libraries
  measure: Scala's `VectorBenchmark2`, immer's `benchmark/vector`, and
  bifurcan's list benchmarks, which `clojure/core.rrb-vector` reuses.
* `workloads.rkt` runs whole programs: scripts of mixed operations over a
  sequence whose size changes as the script runs.  Every implementation runs
  the same script, generated from a fixed seed, and the answers are compared.
* `scaling.rkt` runs one operation at 1k, 10k, 100k and 1M elements and fits
  a slope on log-log axes: about 0 for constant time, 0.2 for log n at the
  bases these structures use, 1 for an operation that touches every element.
* `bm.rkt` is the benchmark from the gvector pull request, with sek rows
  added, and `nqueens.rkt` the classic Scheme benchmark over each structure.
  Both report totals in milliseconds, as their originals do.
* `bench.ml` is `main.rkt`'s scenarios against the OCaml library.
* `report.rkt` turns the recorded JSON into one static HTML page with SVG
  charts.

The contenders are `esek` and `psek`, Racket's `treelist` and
`mutable-treelist`, `gvector`, lists, a mutable box holding a list, and
`array`, a hand-written growable array with no argument checks, which marks
the floor.  A plain `vector` row appears where one applies.

## How to read the tables

Every number in `main.rkt` and `external.rkt` is nanoseconds per operation,
the best of three trials after a calibration run, so numbers compare down a
column.

Every structure is measured on every operation it can perform, including the
ones that cost it a walk of the whole sequence.  A dash means the operation
does not exist.  Two devices keep the linear rows affordable:

* **A capped operation count.**  A scenario that does 100,000 reads does
  fewer for a structure whose `ref` walks, spread over the whole index range.
* **A bounded burst.**  "Push n, then pop n" is quadratic for a structure
  whose push is linear, so those rows build to length n once and then push
  and pop a bounded number of times.  The `burst-check` scenario runs both
  forms on every structure that can afford both.

A destructive operation needs an instance of its own, so the ephemeral rows of
`concat`, `split` and the slicing scenarios pay for a copy: `esek-copy` in
share mode, which is O(1), and a full copy for the others.

In `workloads.rkt`, `n/a` marks an implementation whose linear operations
would make the script quadratic at that size.

## Running them

* Build the reference in its release configuration, as `build-ocaml.sh` does.
  With assertions on, it validates the whole sequence inside every operation.
  `conformance/build.sh` deliberately builds it the other way.
* `run.sh` gives each scenario a fresh process, so that one scenario's garbage
  is not charged to the next.
* The `split-parts`, `take-drop` and `slice` rows need a Racket newer than
  9.3.0.2, where `mutable-treelist` can be copied after it has been shortened
  at the front.
* Pin the process to an idle core.  Timings from different machines differ by
  more than most of the differences below: integer division, which the
  `filter` predicate uses, is several times slower on the machine these
  results come from than on a recent desktop.

## Results

One core of a Xeon Platinum 8268 (2.9 GHz), Racket CS 9.3.0.8 with the
`treelist` and `gvector` it ships, OCaml 5.4.0 `ocamlopt` without flambda, and
the default settings (leaf capacity 128, node capacity 16, threshold 32).

### Single operations

At a million elements, nanoseconds per operation unless a unit is given:

| | esek | psek | treelist | mutable-treelist | gvector | list | array |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| push and pop at the back | 19.6 | 30.5 | 105 | 122 | 114 | 41 ms | 16.1 |
| push and pop at the front | 18.0 | 28.3 | 513 | 529 | 2.6 ms | 7.4 | 2.6 ms |
| queue: push back, pop front | 17.3 | 29.0 | 169 | 187 | 1.3 ms | 18 ms | 1.3 ms |
| traversal, per element | 5.7 | 4.5 | 6.8 | 6.8 | 9.9 | 4.4 | 4.3 |
| `ref` at a random index | 90.5 | 86.7 | 37.3 | 51.4 | 85.1 | 0.9 ms | 14.2 |
| `set` at a random index | 96.0 | 966 | 416 | 51.6 | 56.9 | 22 ms | 10.2 |
| construction, per element | 7.7 | 8.0 | 110 | 123 | 124 | 82.1 | 18.3 |
| `map`, per element | 11.2 | 10.3 | 107 | 39.8 | 89.1 | 24.8 | 17.6 |
| `filter`, per input element | 18.2 | 17.3 | 44.4 | 29.8 | 45.8 | 19.5 | 24.1 |
| append two halves | 246 | 798 | 1919 | 2.9 ms | 61 ms | 12 ms | 15 ms |
| split at a random index | 422 | 234 | 447 | 15 ms | 54 ms | 9.4 ms | 19 ms |
| slice, on a 10% grid | 692 | 428 | 650 | 21 ms | 60 ms | 21 ms | 26 ms |
| one change, then a snapshot | 70.2 | — | — | 3.4 ms | — | — | — |

Pushing and popping cost the same at either end and at every length.
Indexing costs about 2.4 times a treelist's and 6 times a growable array's.
A sequential read should use `in-sek` or an iterator instead: an ascending
walk costs 41.5 ns an element through `psek-ref`, 23.8 through
`treelist-ref`, and 3.3 through an iterator.

A snapshot costs the same at every length, where `mutable-treelist-snapshot`
copies:

```
snapshot: 100000 pushes, keeping a snapshot every m of them, ns per push
                                 m=1        m=10      m=1000    m=100000
esek                           332.8       52.60       26.41       25.39
mutable-treelist                   -     181042.       579.3       108.8
psek (persistent)              24.98       25.03       24.99       24.88
treelist (persistent)          84.11       84.06       84.00       84.24

transient: edit, m in-place updates, snapshot, over 1000000 elements, ns per update
                                 m=1        m=10      m=1000    m=100000
sek edit/snapshot              437.8       284.3       385.8       191.0
treelist copy/snapshot      8962173.     888364.       9019.       179.2
sek persistent set             516.5       323.4       459.6       978.9
treelist persistent set        201.1       139.9       213.9       406.6
```

The chunk capacities trade reads against persistent writes: larger chunks
make a shallower tree, and a persistent write copies a chunk at each level.

```
capacities over 1000000 elements: leaf/node capacity, ns per operation
                              128/16       64/16       32/16       16/16         8/8      256/32
psek-ref                       85.04       113.4       135.5       159.8       237.7       66.60
psek-set                       945.8       789.9       758.6       725.1       958.8       1361.
esek push/pop                  14.95       15.69       16.59       20.02       32.06       16.10
traversal                       4.72        5.67        7.63       11.00       18.92        4.22
```

### Whole programs

Milliseconds for the whole workload; numbers compare across a table, not
between tables.

```
editor: edit a document of lines: jump, insert, delete, redraw a window
                                    1k lines   20k lines  400k lines
                            esek        3.57        5.28       10.63
                            psek        2.80        4.66        9.96
                        treelist        5.90        9.05       51.24
                mutable-treelist       10.47       99.30         n/a
                         gvector       70.83       1420.         n/a
                            list       27.03       596.9         n/a
                           array        9.38       171.5         n/a

undo: edit with an undo history: checkpoint often, occasionally restore
                                    1k lines   20k lines  400k lines
                            esek        2.11        2.83        4.13
                            psek        1.78        2.53        4.08
                        treelist        5.00        8.28       44.79
                mutable-treelist        9.78       96.27         n/a
                         gvector       66.18       1212.         n/a
                            list       14.01       291.3         n/a
                           array        9.95       164.8         n/a

queue: feed a work queue in bursts, drain it, snapshot the backlog
                                 2000 rounds
                            esek       16.95
                            psek       25.29
                        treelist       89.94
                mutable-treelist       107.2
                         gvector       3315.
                            list       7733.
                           array       3399.

log: append a log, compact it by dropping, filtering and re-joining
                                  1k batches 50k batches
                            esek        0.67       30.10
                            psek        0.61       29.03
                        treelist        1.33       73.08
                mutable-treelist        1.13       55.63
                         gvector        7.11         n/a
                            list        1.81         n/a
                           array        1.24         n/a

scan: build, then walk at a stride, then fold
                                         10k          1M
                            esek        2.87       58.94
                            psek        2.60       56.21
                        treelist        2.93       135.1
                mutable-treelist        4.34       158.5
                         gvector        7.88       199.3
                            list       557.7         n/a
                           array        0.97       23.53
```

### Scaling

The fitted slopes from `scaling.rkt`, over 1k to 1M elements:

| | esek | psek | treelist | mutable-treelist | gvector | array |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `ref` | 0.04 | 0.04 | 0.02 | 0.01 | 0.00 | 0.01 |
| `set` | 0.04 | 0.04 | 0.10 | 0.01 | 0.00 | 0.00 |
| push, then pop | 0.00 | −0.03 | 0.08 | 0.07 | 0.00 | 0.00 |
| split, then append | 0.07 | 0.09 | 0.15 | 1.21 | 1.14 | 1.19 |
| checkpoint | 0.01 | 0.01 | 0.01 | 1.17 | 1.25 | 1.14 |

### Against the OCaml library

The same scenarios at a million elements, in nanoseconds.  The ratio is
Racket ÷ OCaml, so a ratio above one means this library is slower.

| scenario | Racket | OCaml | ratio |
| --- | ---: | ---: | ---: |
| a snapshot after every push, per push | 333 | 2783 | 0.12 |
| one change, then a snapshot | 70.2 | 234 | 0.30 |
| `set` at a random index, ephemeral | 96.0 | 259 | 0.37 |
| split | 234 | 447 | 0.52 |
| `ref` at a random index, ephemeral | 90.5 | 168 | 0.54 |
| `ref` at a random index, persistent | 86.7 | 158 | 0.55 |
| `set` at a random index, persistent | 966 | 1587 | 0.61 |
| push and pop at the front, persistent | 28.3 | 44.9 | 0.63 |
| push and pop at the back, persistent | 30.5 | 43.9 | 0.69 |
| traversal by fold, per element | 4.45 | 6.40 | 0.70 |
| queue, persistent | 29.0 | 39.8 | 0.73 |
| construction, ephemeral, per element | 7.67 | 8.91 | 0.86 |
| construction, persistent, per element | 7.97 | 8.98 | 0.89 |
| concat | 798 | 890 | 0.90 |
| queue, ephemeral | 17.3 | 16.8 | 1.03 |
| push and pop at the front, ephemeral | 18.0 | 16.6 | 1.08 |
| traversal through an iterator, per element | 14.2 | 12.0 | 1.18 |
| push and pop at the back, ephemeral | 19.6 | 16.4 | 1.19 |
| `filter`, per input element | 17.3 | 10.7 | 1.61 |

Snapshots are cheaper here because this library shares the front and back
chunks where the reference copies them.  The `filter` row mostly measures the
predicate: `fxremainder` divides, and `ocamlopt` compiles `x mod 3` to a
multiplication.

### nqueens

Eight queens, 10,000 repetitions, in seconds.

| | pairs | vector | psek | treelist | mutable-treelist | esek | gvector |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| the classic program, operations swapped | 1.7 | 5.6 | 11.6 | 13.0 | 23.1 | 28.3 | 124.7 |

| | vector | box of list | box of psek | esek | box of treelist | mutable-treelist | gvector |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| backtracking over one mutable stack | 5.1 | 5.6 | 11.5 | 13.0 | 13.3 | 25.9 | 40.5 |

The classic program never holds more than eight elements and copies on every
`cons` and `append`, so pairs win it and the mutable structures pay most.
