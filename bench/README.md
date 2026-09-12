# Benchmarks

```sh
./run.sh                     # this library, one scenario per process
./run.sh --external          # the scenarios borrowed from other libraries
./run.sh --all               # both
./run.sh --quick             # smaller sizes
racket -y main.rkt stack     # a single scenario

./build-ocaml.sh             # build the OCaml reference (needs ocamlfind)
./run.sh --ocaml             # the same scenarios, run against it

racket -y bm.rkt             # the benchmark from racket/data PR #34, plus sek
racket -y nqueens.rkt        # the classic Scheme nqueens benchmark

./run.sh --all --json results.json     # record every table
racket -y nqueens.rkt --json results.json    # append
racket -y report.rkt results.json report.html    # draw it
```

`main.rkt` follows the benchmarks that accompany the OCaml library — stack,
reach, iteration, traversal, construction, fill, split — and Figures 17 and 18
of the paper, and adds scenarios for transience, which the reference's own
suite does not measure (it uses `snapshot` and `edit` only to set sequences
up). `external.rkt` is the shapes that the *other* chunked-sequence libraries
measure, transcribed from their suites; `bm.rkt` is the benchmark from the
gvector PR, unmodified except for the added sek rows. In both cases the
comparison is in their terms rather than ours.

`report.rkt` turns the recorded JSON into one static HTML page — the charts are
SVG generated at write time, so it needs no scripts and no network.

Every number in `main.rkt` and `external.rkt` is nanoseconds per operation, the
best of three trials after a calibration run, so numbers are comparable down a
column. `bm.rkt` and `nqueens.rkt` report their own totals in milliseconds, as
upstream does.

Five things worth knowing if you re-run this:

* The reference must be compiled in its **release** configuration. With
  assertions on it runs its own O(n) validator inside every operation and
  looks about thirty times slower than it is. `build-ocaml.sh` handles this;
  `conformance/build.sh` deliberately does the opposite.
* Each scenario runs in a fresh process. Otherwise one scenario's garbage is
  charged to the next, and the heap never shrinks.
* The `gvector` measured here is the revised one from
  [racket/data#34](https://github.com/racket/data/pull/34) (`samth:gvector-fast`,
  commit 778009f, "Optimize gvector with unsafe ops and memory-safe
  synchronization"), which is what is installed on this machine.
* The `treelist` measured here is likewise not the shipped one: it carries the
  `treelist-copy-for-mutable` fix described at the end of this file, and on top
  of that the changes in "And the treelist" below, which take `treelist-ref`
  from 6.35 ns to 4.53. Both are unmerged. Against the shipped treelist this
  library's margins on indexing and traversal are correspondingly larger, which
  is the wrong way to report them.
* **Every structure is measured on every operation it can perform at all**, and
  the table below says how the ones that cost a walk are kept affordable.
* `split-parts`, `take-drop` and `slice` need a Racket newer than 9.3.0.2.
  Shortening a mutable treelist at the front and then copying or snapshotting
  it used to raise `vector-length: contract violation`; that was a bug in
  `treelist-copy-for-mutable`, fixed in `racket/collects/racket/treelist.rkt`
  with a regression test in `racket-test-core`'s `treelist.rktl`. See
  "A Racket bug this turned up" at the end.

Numbers below are from one machine: Racket CS 9.3, OCaml 5.4.0 (flambda `-O3`),
default settings (leaf capacity 128, node capacity 16, threshold 32).

## Filling in every cell

A dash used to mean "this structure has no efficient way to do that", which is
a judgement rather than a measurement, and it left whole rows blank. Now a dash
means only that the operation does not exist at all — a gvector has no
snapshot, a persistent sequence has no in-place fill. Everything else is
measured, including the cases that cost a walk of the whole sequence: a cons
list does have a back, it is just Θ(n) away, and the number says so.

Two mechanisms keep those rows from taking forever. Each contender names its
Θ(n) operations, and:

* **A capped operation count.** A scenario that does 100 000 reads does
  proportionally fewer for a structure whose `ref` walks, spread over the whole
  index range so that a shortened sweep still reaches the far end. `measure`
  reports per operation, so the number stays comparable down the column.
* **A bounded burst.** "Repeat n pushes then n pops" is quadratic for a
  structure whose push is Θ(n) and simply will not finish — growing a cons list
  to 10^5 elements one `append` at a time costs about a minute per round. Those
  rows build to length n once and then push and pop a bounded number of times,
  which estimates the same per-operation cost.

The `burst-check` scenario runs both paths against every structure that can
afford either, so the size of that substitution is on the page rather than
asserted. They agree within a few percent for sek, gvector and a plain array;
for a treelist the burst reads about 30% low, because the full loop also pays
to build and discard the whole sequence once a round and the burst does not.

Two contenders were added at the same time: **`array`**, a hand-rolled growable
array — the floor that everything else is paying over — and a plain fixed
**`vector`** row where one applies.

`array` is the same shape as a `gvector`: a vector and a count, doubling on
demand, with the same memory-safety invariant under concurrent access. What it
does not have is everything else that makes a gvector a library data structure
— an argument check on every operation and a range check on `ref`, impersonator
support, the shrink pass `gvector-remove!` runs on every removal, and a dict /
`equal+hash` / serialization surface. The gap between the two rows is what that
costs:

| ns, n = 10^5 | gvector | array |
| --- | ---: | ---: |
| `ref` at random indices | 7.56 | **2.96** |
| `set` at random indices | 10.50 | **3.31** |
| push and pop at the back | 20.38 | **4.77** |
| construction, per element | 5.78 | **5.06** |
| traversal, per element | 1.31 | **1.27** |

Most of the push/pop gap is the shrink pass: `gvector-remove-last!` goes
through `gvector-remove!`, so every pop runs `trim!`'s capacity computation and
its CAS even when nothing shrinks. Push-only, in `sync-cost` below, a gvector
costs 10.7 rather than 20.4. Most of the `ref` gap is the two argument checks.

Two things about the floor took getting right, and both are worth recording
because each moved the number by more than the thing being measured.

**The growth path has to be a separate function.** With `make-vector` inlined
beside the store, `arr-push-back!` measured 8.7 ns per element against
gvector's 5.9; split out, it measures 5.1. The common case is a bounds test, a
store and an increment, and burying that next to an allocation hides it.

**Traversal has to hoist both fields.** An index loop calling `arr-ref` re-reads
`vec` and `n` per element and costs 1.40 ns; reading them once and handing the
bounds to `in-vector` costs 1.27, the same as a gvector and within 2.5× of a
raw vector sweep. `in-arr` is that loop as a sequence form, expanded in place
by `for`.

### What thread safety costs

`array` maintains `n ≤ (vector-length vec)` at every observable point, which is
what lets its `unsafe-vector*-ref` stay in bounds while another thread is
growing the array. Three rules keep it:

* a writer stores into the vector and only then raises `n`;
* `arr-ensure!` installs a larger vector with `unsafe-struct*-cas!`, retrying
  if another thread got there first;
* a writer captures `n` once and asks for room for *that* `n`, never a re-read
  one — and `arr-ensure!` consequently never reads `n` at all.

The third rule is the subtle one, and `bench/array-tests.rkt` found it the hard
way. `n` is written as an absolute value computed from a stale read, so a
concurrent writer can lower it; by itself that only loses an update. But a
growth path that re-read `n` could see the lowered value, conclude no growth
was needed, and then store at the `n` its caller had already captured — past
the end of the vector, through an unsafe write. Under futures the test violated
the invariant in 13 of 20 rounds before the fix and 0 of 20 after.

Like gvector, this is memory safety and not atomicity: two threads pushing at
once can still lose an update, because no slot is reserved. Actual mutual
exclusion needs a lock, and `sync-cost` prices all three:

| ns per push, n = 10^5 | |
| --- | ---: |
| unsynchronised | 7.24 |
| memory-safe (`array`) | **6.97** |
| gvector | 10.74 |
| locked (semaphore) | 16.77 |

| ns per `ref` | |
| --- | ---: |
| memory-safe (`array`) | **1.62** |
| gvector | 7.33 |
| locked (semaphore) | 14.25 |

The invariant is free — it is discipline about the order of two writes, not
extra work, and on the reading side it costs nothing at all, because readers do
not maintain it. A lock is not free: 2.4× on a push and 9× on a read.

A destructive operation needs an instance of its own, so the ephemeral rows in
`concat`, `split`, `slice`, `take-drop`, `split-parts` and `bulk-append` copy
first and are charged for the copy. That is the honest figure: it is what using
a mutable sequence in a persistent way actually costs. `eseq-copy` is O(1),
`mutable-treelist-copy` about 0.5 ns an element, a gvector's about 1.4.

## Against other Racket sequences

Contenders: `treelist` and `mutable-treelist` from `racket/treelist` (RRB
trees, the closest analogue), `gvector`, immutable lists, a mutable box holding
a list, and a hand-rolled growable `array`.

### The ends

`push` and `pop`, repeating "n pushes then n pops" until two million pushes
have happened, so n is the peak length.

```
stack: push-back / pop-back                     front stack: push-front / pop-front
              1000    100000   1000000                        1000    100000   1000000
eseq         10.13     10.24     10.93          eseq           9.76      9.82     10.49
pseq         12.67     12.89     14.08          pseq          12.60     12.86     14.09
treelist     21.70     38.74     45.51          treelist      37.09     163.8     204.2
mutable-tl   26.82     43.88     51.48          mutable-tl    43.07     167.5     206.4
gvector      19.05     19.10     27.43          gvector           -         -         -
                                                list           1.02      1.14      2.50

queue: push-back / pop-front
              1000    100000   1000000
eseq         10.24     10.16     10.88
pseq         12.80     13.04     14.23
treelist     28.87     58.21     70.28
mutable-tl   34.46     62.65     75.10
```

This is what the structure is for. `eseq` is flat in n and does not care which
end you use; a treelist costs 4× more at the back, 20× more at the front, and
degrades as the sequence grows. A gvector is fine at the back and has no front.
A Racket list is unbeatable as a front stack — and is only a front stack.

### Traversal, and what segments buy

```
traversal: for-each over the whole sequence, ns per element
                 100     10000   1000000
eseq            1.60      1.46      1.54
pseq            1.53      1.47      1.49
treelist        1.81      1.72      1.78
gvector         1.41      1.22      1.25
list            1.28      1.14      1.32
  pseq via fold 1.26      1.22      1.27
  pseq via iter  3.78      3.65      3.67
  vector        0.58      0.50      0.51
```

Sweeping a sek sequence costs about what sweeping a treelist costs. That is
entirely down to segments: the same traversal driven one element at a time
through the iterator costs 11.1 ns, almost 6× more. `sek-fold-left` and the
`for` clause forms of `in-sek`, `in-pseq` and `in-eseq` all take the segment
path.

### Random access

```
random access: ref at random indices        hops: ref at a fixed stride, n = 10^6
                 100     10000   1000000                  +1        +8       +64     +4096
eseq            7.61     25.19     59.84    eseq       33.01     32.95     36.29     56.01
pseq            8.07     24.91     60.15    pseq       33.02     33.42     36.68     57.10
treelist        4.24      4.88     10.71    treelist    5.34      5.82      7.07     10.28
mutable-tl      8.34      8.86     16.33    mutable-tl  9.21      9.51     10.21     15.68
gvector         7.24      7.27     10.82    gvector     7.15      7.17      7.30     12.24
  vector        0.76      0.85      2.09    pseq iter   9.59     11.79     26.72     74.29
```

This is the operation the design gives up, and the measurement says so: `ref`
is about 6× a treelist's. The reason is structural rather than incidental —
reaching an element means descending the middle spine to the level that holds
it and then descending the chunk hierarchy back to depth 0, roughly twice the
pointer-chasing of an RRB tree, which indexes in one descent. Profiling shows
the time spread evenly across those steps, with no hotspot to remove; the
OCaml reference measures 55.97 ns for the same lookup, so this is the data
structure, not the port.

What the structure offers instead is an iterator that remembers where it is.
Scanning with hops of one costs 9.6 ns rather than 33, because the cursor
stays inside the chunk it is already on.

### Updating

```
update: set at random indices              fill: overwrite k consecutive elements of 10^6
                 100     10000   1000000                        10      1000    100000
eseq           10.36     31.27     68.06    eseq (sek-fill!)  10.22      2.45      2.44
pseq           119.2     218.0     650.5    eseq (set! loop)  21.79     20.96     36.42
treelist       16.40     32.70     188.5    mutable-treelist   8.80      7.89      7.82
mutable-tl      7.54      8.00     16.19    gvector           10.03      9.05      8.97
gvector         9.63      9.62      9.68    vector             1.04      0.63      0.63
```

Scattered persistent writes are the other thing the design gives up: `pseq-set`
copies a chunk per level, and with 128-element leaves that is a lot of copying.
Writes with any locality are a different story — going through writable
segments, `sek-fill!` overwrites at 2.4 ns per element, 15× faster than the
same range written one `eseq-set!` at a time, and three times faster than a
mutable treelist or a gvector.

### Building, concatenating, splitting

```
construction: n elements from scratch      concat / split at n = 10^6, ns per operation
                 100     10000   1000000                concat     split
eseq            2.68      1.66      2.33    pseq         294.1     101.1
pseq            2.61      1.66      2.30    treelist     799.9     137.6
treelist       14.00     32.20     47.88    list       5929737   7421195
mutable-tl     17.24     35.42     52.82
gvector         5.96      4.64     21.50
list            1.90      2.03     42.62
```

Construction is flat for sek at about 2 ns an element and gets 3.4× worse for a
treelist as the sequence grows — the same effect as the push benchmark — so at
a million elements sek builds 21× faster than a treelist and 18× faster than
consing a list. Concatenation is 2.7× a treelist's; splitting is now 1.4×
*faster* than it, where it used to be 5× behind.

### Filtering, the paper's motivating example

§5.4 gives filtering a persistent sequence as the case for iterators: read
through an iterator on the source, write into an ephemeral destination, and
the whole thing is O(n + K). That is what `sek-filter` does.

```
filter: keep one element in three, ns per input element
                 100     10000   1000000
pseq            5.19      3.89      4.08
eseq            5.29      3.91      4.10
treelist        6.13     12.71     17.75
  list          4.21      3.79      5.21
  vector        4.67      4.57      7.92
```

Flat in n, and 4.4× faster than `treelist-filter` at a million elements --
where filtering a plain Racket list costs 5.21 ns an element and a vector
7.92, because both of those have to grow a result the size of the input while
this one appends chunk by chunk.

## Transience

The reference's benchmark suite does not measure transience; these scenarios
are ours. Racket's `mutable-treelist` is the honest comparison, because it has
the same pair of conversions: `treelist-copy` is its `edit` and
`mutable-treelist-snapshot` its `snapshot`.

```
one change plus one snapshot of an n-element sequence, ns
                     100     10000   1000000
eseq               154.7     209.3     209.2
mutable-treelist   62.00      3970   1714328
```

`eseq-snapshot` does not depend on the length of the sequence.
`mutable-treelist-snapshot` is linear in it: at a million elements one snapshot
costs 1.7 milliseconds, about 8000× more. That is the difference between the
ownership-identifier scheme of §2.4, where a snapshot just hands the sequence a
fresh identity, and copying.

The round trip — edit a persistent sequence, update it in place, snapshot the
result — puts a price on both halves:

```
edit, m in-place updates, snapshot, over 10^6 elements, ns per update
                            m=1      m=10    m=1000  m=100000
sek edit/snapshot         352.5     252.0     270.6     121.6
treelist copy/snapshot  3793970    372090      3943     62.55
sek persistent set        318.2     296.1     380.4     605.1
treelist persistent set   77.64     67.20     62.79     192.1
```

The two structures are mirror images. Sek's conversions are free and its
in-place writes are relatively expensive; a treelist's conversions cost O(n)
each and its writes are cheap. So sek wins outright when a few elements change
between snapshots — by four orders of magnitude at m = 1 — and a treelist wins
once several tens of thousands of writes amortize the copy. For a treelist the
transient path is not even worth taking below m ≈ 1000: its persistent `set` is
faster than copying in and out.

The same thing shows up in a loop:

```
100000 pushes, keeping a snapshot every m of them, ns per push
                       m=1      m=10    m=1000  m=100000
eseq                 127.6     19.61      9.81      9.52
mutable-treelist         -         -     271.8     44.19
pseq (persistent)    11.83     11.83     11.87     11.89
treelist (persistent) 38.16    38.24     38.32     38.12
```

Snapshotting every thousandth push costs an ephemeral sek sequence essentially
nothing (9.81 versus 9.52 ns per push). The mutable-treelist columns for m = 1
and m = 10 are missing because keeping that many copies of a growing sequence
needs more than ten gigabytes.

### Tuning

The chunk capacities are a real dial, and it trades exactly what the paper says
it trades:

```
capacities over 10^6 elements, ns per operation
                    128/16     64/16     32/16     16/16       8/8    256/32
pseq-ref             59.61     72.65     87.84     108.6     191.0     49.61
pseq-set             615.9     558.4     539.1     590.7     766.4     858.6
eseq push/pop        10.17     10.73     11.14     12.38     17.17     10.32
traversal             1.78      2.15      2.95      4.18      7.35      1.55
```

Bigger chunks mean a shallower tree, so reads, pushes and traversal all get
faster; but a persistent write copies a chunk per level, so `set` is worst at
both ends and best around 32-element leaves. The shipped default of 128/16 is
the paper's, and it is the right call unless an application does scattered
persistent writes.

## Borrowed from other libraries

The scenarios above are the ones the paper and the authors' OCaml suite chose.
`external.rkt` adds the ones that the *other* chunked-sequence libraries chose,
transcribed from their own benchmark suites:

* **Scala** — `VectorBenchmark2.scala` from `scala/scala`, the JMH suite Stefan
  Zeiger wrote for the 2.13 `Vector` rewrite ([scala/scala#8534](https://github.com/scala/scala/pull/8534),
  "radix-balanced finger tree vectors"). Contributes `apply-sequential`,
  `update-sequential`, `apprepend`, `ends`, `slice`, `bulk-append`, `map`,
  `filter-ratio`.
* **immer** — `benchmark/vector/` from [arximboldi/immer](https://github.com/arximboldi/immer),
  behind Juan Pedro Bolívar Puente's "Persistence for the Masses: RRB-Vectors in
  a Systems Language" (ICFP 2017). Its `_move` and `_mut` variants are its
  transients, which is why its suite is worth borrowing here at all.
  Contributes `take-drop` and `push-move`.
* **bifurcan** — `benchmark_test.clj` from [lacuna/bifurcan](https://github.com/lacuna/bifurcan),
  Zach Tellman's cross-library comparison, which
  [clojure/core.rrb-vector](https://github.com/clojure/core.rrb-vector/blob/master/doc/benchmarks/benchmarks.md)
  reuses for its own published numbers. Contributes `split-parts`.

Two of Scala's are deliberately left out. `vBadApplySequential` differs from
`vApplySequential` only in reading a field rather than a local, which is a JIT
question with no Racket counterpart, and `nvSliding` measures an API none of the
Racket structures have.

These were run on the smaller size ladder (`--quick`, up to 10^5 rather than
10^6): the machine had about 2 GB free at the time, and a full ladder OOMs.
The trends across the three sizes are the point, and they are already clear.
Nanoseconds per operation at n = 10^5:

| | eseq | pseq | treelist | mutable-treelist | gvector | array |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `apply-sequential`, per lookup | 17.5 | 15.1 | 6.0 | 10.0 | 7.5 | **2.5** |
| `update-sequential`, per set | 24.5 | 130.9 | 48.9 | 9.5 | 10.7 | **3.2** |
| `apprepend`, per push | **6.4** | 9.3 | 149.3 | 157.9 | 62113 | 61987 |
| `peek`, per first+last pair | 22.1 | 11.4 | 11.2 | 24.9 | 21.1 | **8.1** |
| `tail`, per pop | **6.4** | 9.5 | 78.1 | 87.9 | 69884 | 67142 |
| `slice`, per slice | 342.8 | **180.9** | 190.2 | 242687 | 408674 | 245213 |
| `map`, per element | 8.9 | 8.8 | 39.4 | 6.5 | **5.0** | 5.2 |
| `filter` keeping all, per element | 8.8 | 8.8 | 39.5 | 6.3 | 5.2 | **5.0** |
| `take-lin`, per step | 92.6 | 54.4 | **28.6** | 4820 | 81126 | 8760 |
| `push_move`, per element | **4.8** | 5.9 | 39.9 | 44.6 | 5.4 | 7.0 |
| `split-parts`, per element | 1.97 | 1.95 | 1.97 | 7.6 | 10.1 | 6.2 |

The `array` column is the floor described above, and reading down it is the
quickest way to see which of these operations are inherently about the shape of
the data structure and which are not. Indexing, peeking and mapping are all
things a flat array does several times faster than any tree, and the numbers
say by how much. `apprepend`, `tail` and `slice` are the ones where the array
is catastrophic — Θ(n) per operation — and where the trees are earning their
keep.

Six things come out of this.

**`apprepend` is the clearest win in the whole suite, and it is Scala's own
benchmark.** Alternating a push at each end costs sek a flat 10.3 ns and a
treelist 151.1 ns — 15× — and the gap is entirely a function of length:

| `apprepend`, ns per push | 10 | 1000 | 100000 |
| --- | ---: | ---: | ---: |
| eseq | 16.32 | **10.20** | **10.28** |
| pseq | 13.87 | 12.75 | 12.99 |
| treelist | **6.67** | 30.75 | 151.1 |
| mutable-treelist | 10.51 | 35.53 | 159.6 |
| array | 67.49 | 709.0 | 62441 |
| gvector | 94.79 | 745.8 | 62642 |
| list | 223.9 | 3189 | 665062 |

An RRB tree pays for a prepend what it pays for an append, and both go up with
depth. A sequence with a chunk at each end does not care which end it is
growing, and never leaves the chunk for K pushes out of K. This is the same
property Figures 17 and 18 measure separately; Scala's benchmark measures it in
one program, which makes it harder to miss.

**Sequential indexing is where sek's iterators earn their keep.** Scala
separates `vApplySequential` from `vApplyRandom` because a tree with a cached
display answers an ascending walk from cache. sek caches nothing on `ref`:

| `apply-sequential`, ns per lookup | 100 | 10000 | 100000 |
| --- | ---: | ---: | ---: |
| eseq | 7.74 | 22.37 | 27.69 |
| pseq | 8.42 | 22.23 | 27.51 |
| treelist | 4.91 | 5.41 | 6.14 |
| gvector | 7.54 | 7.55 | 7.54 |
| array | 2.64 | 2.51 | **2.51** |
| list | 29.30 | 3979 | 42000 |
| vector | 1.66 | 1.59 | 1.59 |
| pseq via iterator | **1.83** | **1.77** | 2.71 |

Read through `ref`, sek is 4.5× behind a treelist. Read through `in-pseq`, the
same walk is 2.7 ns — 2.3× faster than the treelist's cached `ref`, faster than
a growable array's indexed read, and within 1.7× of a raw vector. The library's
answer to sequential access is a first-class iterator, and it is a better
answer than a display; it is just not spelled `ref`.

**Bulk element-wise work is 4× ahead of a treelist, for the same reason.**
`map` and `filter` hand out segments, so their inner loop touches a raw vector:

| ns per input element, n = 10^5 | 100% kept | 50% | 0% |
| --- | ---: | ---: | ---: |
| pseq | 10.39 | 6.60 | 2.08 |
| eseq | 10.37 | 6.55 | 2.10 |
| treelist | 39.76 | 19.85 | **1.74** |
| mutable-treelist | 6.37 | 4.93 | 2.58 |
| gvector | 5.27 | 3.26 | 1.54 |
| array | 5.13 | 3.40 | 1.17 |
| list | **4.62** | **2.81** | 1.07 |
| vector | 7.22 | 3.92 | 1.08 |

Scala measures three filter ratios because they separate two costs. At 100% and
50%, where the output has to be built, sek is 4× ahead of a treelist. At 0%,
where nothing is built, the treelist wins: all that is left is the traversal,
and its traversal is slightly cheaper. The flat structures beat both, which is
the honest shape of this operation — filtering is a traversal and an append,
and neither is what a chunked tree is for.

**Slicing was once what this design gives up, and it turned out not to be.**
`slice`, `take-lin` and `drop-lin` used to read 5×, 16× and 7× a treelist, and
the obvious reading was that a chunked sequence pays for splitting. It was not
that: it was seven places where this port copied where the reference shares,
rescanned what it already knew, or built a half it then discarded. Closing them
took `slice` from 1073 ns to 257, `take` from 600 to 86, and `split` from 4.6×
the OCaml reference to 1.07×. "Closing the split and concat gaps" below is the
account. What is left is 1.3× a treelist on a two-sided slice and 2.7× on a
repeated one-sided `take` — the density invariant is real work that an RRB tree
does not do, and that is the honest residue.

The ephemeral rows in `take-drop` are immer's `_mut` variants, and their shape
is still worth reading: `eseq` is slower than `pseq` because an O(1) copy
still leaves the destructive split the same work to do, while
`mutable-treelist` is 60× worse than `treelist` because its copy is Θ(n) and
there are only ten steps to amortise it over.

**And `push_move` reproduces immer's headline.** Building through a transient
and freezing at the end costs 8.7 ns per element against 40.2 for repeated
persistent `treelist-add` — 4.6× — and it also beats sek's own persistent
`pseq-push-back` (12.6) and a hand-rolled growable array (7.1, once you count
the array's own growth). That is the comparison Clojure spells
`(persistent! (reduce conj! (transient []) xs))`, and it holds here.

**`tail` is the one where filling the table changed the answer.** Popping the
front until empty used to be a persistent-only row, and `pseq` won it at 13.4
ns against a treelist's 79.6. With the ephemeral structures measured too —
each popping a copy of its own, and charged for making it — `eseq` wins at
10.4, because `eseq-copy` is O(1) and the pops are what the front chunk is for.
A `mutable-treelist` pays 91.1 for the same loop and a growable array 69663,
since every pop moves the whole array down one.

One oddity worth recording, and it is fixable: `peek` on an `eseq` costs 40.5 ns
against 16.2 for a `pseq`, where every other operation has the two within a few
percent. Timed apart, at n = 10^5:

| | first | last |
| --- | ---: | ---: |
| `pseq` | 8.21 | 9.25 |
| `eseq` | 23.06 | 18.44 |

`pseq-first` reads the representation directly — a vector for a short sequence,
`pt-ref` at 0 otherwise. `eseq-first` calls `eseq-empty?` (which computes the
length) and then `eseq-ref e 0`, which walks front, inner front, middle, inner
back, back like any other index; `eseq-last` computes the length a second time
on top of that. `eseq-ref e 0` alone is 17.56 ns and `eseq-length` is 6.76.
Nothing depends on this, but the ephemeral ends could read their front and back
chunks directly the way the persistent ones do.

## The gvector PR benchmark

`bm.rkt` is the benchmark from racket/data#34 with sek rows added. It varies
the shape differently from `main.rkt` — M sequences of length N, so the short-N
rows are dominated by the cost of *creating* a sequence. Times are milliseconds
for the whole M×N workload.

```
1000000 of length 10                     100 of length 100000
+ for/gvector          39                + for/gvector          44
+ for/mut-treelist     34                + for/mut-treelist     37
+ for/eseq            116                + for/eseq             85
+ mut-treelist-add!    90                + mut-treelist-add!   425
+ treelist-add         60                + treelist-add        388
+ pseq-push-back      137                + pseq-push-back      120
- treelist-rest        77                - treelist-rest       778
- eseq-pop-front!     144                - eseq-pop-front!      91
! gvector-set!/fx     105                ! gvector-set!/fx      92
! mut-tree-set!/fx     78                ! mut-tree-set!/fx     82
! eseq-set!/fx        108                ! eseq-set!/fx        347
! eseq-fill!           86                ! eseq-fill!           25
^ in-gvector           12                ^ in-gvector            3
^ in-mut-treelist      29                ^ in-mut-treelist      19
^ in-sek               66                ^ in-sek               11
^ gvector-ref          84                ^ gvector-ref          71
^ eseq-ref             74                ^ eseq-ref            280
```

The two columns tell opposite stories, and both are fair.

At **length 10** sek loses at everything that touches a fresh sequence:
`for/eseq` is 3× `for/gvector`. A chunk of capacity 128 is a poor container
for ten elements, and a million of them is a million such chunks. This is
Figure 16's "creating an ephemeral sequence costs O(n + K)" showing up as a
constant factor; the fix within the design would be a smaller K, which the
capacity table above prices.

At **length 100000** it inverts: `for/eseq` is now half of `mut-treelist-add!`
and a fifth of `treelist-add`, popping from the front is 8× faster than
`treelist-rest`, and `in-sek` beats `in-mut-treelist`. Only indexing still goes
the other way.

`eseq-fill!` is the standout in both columns: writing a whole range through
writable segments is 3–4× a per-element `set!` loop on any of the contenders.

## nqueens

`nqueens.rkt` is the classic Scheme benchmark — the one in
`racket-benchmarks/tests/racket/benchmarks/common/nqueens.sch`, which counts
the 92 solutions to the 8-queens problem and repeats that 10000 times.

```sh
racket -y nqueens.rkt          # 10000 repetitions
racket -y nqueens.rkt 200      # fewer
```

The program is written against pairs, and everything it does — `null?`, `car`,
`cdr`, `cons`, `append` — every sequence type here can do, so the first table
is that program with the pair operations swapped out, one variant per
structure. Each variant is generated by a macro, so it compiles to direct
calls rather than dispatching through a table of closures.

```
the classic program with the pair operations swapped out
pairs                        413 ms
vector                      1967 ms
pseq                        3487 ms
treelist                    4376 ms
mutable-treelist            8225 ms
gvector                    10791 ms
eseq                       16295 ms
```

Pairs win by a factor of eight, and should: the search never holds more than
eight elements, `cons` is two words, `cdr` is free, and appending two lists of
four is nothing. Nothing in this benchmark asks for anything a pair list is
bad at — which is also why an immutable `vector` comes second, at 1967 ms.
Copying on every `cons` is Θ(n), but n is at most eight, and eight words is
cheaper than any tree's indirection.

Among the sequence structures, `pseq` is the fastest — 20% ahead of
`treelist`. Both are persistent, so the translation is direct: the search
keeps three sequences alive across two recursive calls, and `cons` and
`append` leave their arguments alone for free.

The mutable structures cannot do that. Every `cons` and `append` has to copy,
and that is what the bottom three rows measure — 2× to 4× the persistent ones.
It is the honest cost of using a mutable sequence for an algorithm that wants
sharing, and the reason a library like this one has both flavours: on this
program you would reach for `pseq`, and `eseq` is the wrong tool.

The same problem written the way a mutable structure wants it — one stack of
placed rows, pushed when a queen goes down and popped on the way back out —
puts the mutable structures on their own ground:

```
the same problem as a backtracking search over one mutable stack
vector                      1365 ms
box-of-list                 1534 ms
box-of-treelist             4199 ms
box-of-pseq                 4641 ms
eseq                        8024 ms
mutable-treelist            8179 ms
gvector                     9463 ms
```

Here `eseq` is level with `mutable-treelist` and ahead of `gvector`. All three
are about six times a bare vector, because a stack that never exceeds eight
entries has no use for any of their machinery: the vector version is a
`vector-set!` and an integer, and everything else is paying for growth it will
never need.

The two `box-of-` rows are a persistent sequence held in a box and pushed and
popped by replacement, which is what you would write if the search also had to
keep snapshots of the placed rows. Both land at about 4.2–4.6 s, between the
pair list and the mutable structures — the allocation per push is real, but so
is not having to copy anything, and at eight elements that trade is close to
even.

Writing this benchmark turned up one fixable thing. `eseq-copy` went through
`eseq-snapshot` and `pseq-edit`, which for a short sequence converted the tree
to a vector and the vector back into a chunk. It now hands both sequences a
fresh identity and shares everything, leaving the copying to whichever one
writes first — the same reasoning as `snapshot`, and O(1) rather than
O(n + K). That is a 3× improvement on the `eseq` row above.

## Against the OCaml reference

Both implementations running the same scenarios at the same sizes, at
n = 10^6. The ratio is Racket ÷ OCaml, so above one means this library is
slower.

| scenario | Racket | OCaml | ratio |
| --- | ---: | ---: | ---: |
| `set` at a random index, ephemeral | 34.3 | 98.3 | **0.35** |
| `set` at a random index, persistent | 435.5 | 811.5 | **0.54** |
| push/pop at the back, persistent | 9.4 | 14.8 | **0.63** |
| traversal by fold, per element | 1.3 | 2.0 | **0.64** |
| split | 101.1 | 152.2 | **0.66** |
| `ref` at a random index, persistent | 39.1 | 55.1 | **0.71** |
| `ref` at a random index, ephemeral | 40.1 | 56.3 | **0.71** |
| construction, persistent, per element | 2.3 | 3.2 | **0.73** |
| construction, ephemeral, per element | 2.3 | 3.2 | **0.74** |
| traversal through an iterator, per element | 3.7 | 4.8 | **0.77** |
| concat | 294.1 | 324.6 | **0.91** |
| push/pop at the back, ephemeral | 5.5 | 5.8 | **0.94** |
| queue push/pop, ephemeral | 5.6 | 5.9 | **0.94** |
| push/pop at the front, ephemeral | 5.7 | 5.9 | **0.95** |
| `filter`, per input element | 4.1 | 3.8 | 1.07 |

Fourteen of these fifteen are faster than the reference, on a runtime with a
garbage collector against native code compiled with flambda, and the one that
is not is `filter` at 1.07× -- of whose 4.1 ns about 1.7 is the benchmark
predicate's own `(zero? (modulo x 3))`, generic arithmetic the reference does
not pay for its `x mod 3`. Ephemeral `set` costs a third of what it costs
there, persistent `set` half, splitting and indexing and construction about a
third less.

Every row of this table has moved, and none of the movement was about Racket
being Racket. `split` once read 4.6× and `construction` 3.3× and `concat`
2.5×; the sections below are the accounts, and the pattern in all of them is
the same. The port did something the reference does not — copied a chunk where
the reference shares a view of it, rescanned a weight it already knew, built
half of a split it then threw away, pushed one element at a time where the
reference fills a chunk, called a contract-checked list function on a
three-element list — and the fix was to stop doing it, not to write different
Racket.

Two entries stand out.

**Snapshotting after every push is nine times faster here** (129 versus
1188 ns). The OCaml `snapshot` performs a shallow copy, duplicating the front
and back chunks each time; this implementation shares them and lets the next
write pay for a copy if there is one. In a snapshot-heavy loop, where the next
write usually extends a chunk monotonically and so copies nothing, sharing wins
outright — and it is no longer the worse tradeoff for a snapshot in isolation
either: one change plus one snapshot of a million-element sequence is 25.4 ns
here against 75.5 there.

**Indexing is the operation this design gives up**, and the reference agrees:
39.1 ns here against 55.1 there, both of them well behind a treelist's 9.1. It
is slow because of how the structure is shaped, not because of how it was
ported. The answer to sequential access is the iterator, which costs 3.7 ns a
step.

## Closing the split and concat gaps

`split` was once 4.6× the reference and `concat` 2.5×, and the first
explanation on offer — that a chunked sequence gives up slicing — turned out to
be wrong. Reading `ShareableSequence.ml` and `ShareableChunk.cppo.ml` next to
this code found seven differences, none of them about Racket. Measured at
n = 10^5, persistent:

The columns are before and after *that* change; `concat` moved again later,
in "The list plumbing in merge" below.

| | before | after | treelist | OCaml |
| --- | ---: | ---: | ---: | ---: |
| `split` | 591 | **153** | 121 | 144 |
| `take` | 600 | **86** | 31 | — |
| `drop` | 600 | **82** | 67 | — |
| `slice` (two splits) | 1073 | **257** | 190 | — |
| `concat` | 598 | **520** | 1090 | 262 |
| `concat`, n = 100 | 104 | **24** | 40 | 12 |
| `set` at a random index | 325 | **158** | 58 | 282 |

**A persistent split copies no chunk at all.** `ShareableChunk.three_way_split`
branches on ownership: a *shared* chunk yields `share`, a new view onto the same
support, where only a *uniquely owned* one calls `sub` and copies. A persistent
sequence owns nothing — `PersistentSequence.split` passes `Owner.none`, and
`is_uniquely_owned o1 o2 = o1 = o2 && o2 <> none` is then always false — so the
copying branch never runs. `chunk-sub` here copied unconditionally, even though
the `(support, head, size)` representation it needed was already in place. This
was 69% of split.

**Weights are arithmetic, not a scan.** `chunk-item-at` returns the offset `j`
of the atomic index within item `q`, so the prefix weighs `i - j` and the
suffix is `chunk-weight - w1 - wq`: the same two subtractions the reference gets
from `reach` and `weight2`. The old code re-walked the copied items with a
`chunk-ref` each, 16% of split.

**`take`, `drop` and `get` are separate functions there.** `ShareableSequence`
specialises `three_way_split` three ways, each building only what is asked for.
`sek-take` here ran a full split and discarded half of it.

**The split element need not be pushed back.** Both implementations put the
item at the split point on the right by pushing it onto the front afterwards
(`SSeq.push Front s2 x`), which copies a chunk to prepend one element — 62% of
split once the above landed. But the right half's leading chunk is a view
starting at item q+1 of the chunk that was split, so item q is the slot
immediately before it: starting the view at q puts the element where it belongs
for nothing. Only sound where the item is not itself subdivided, so it is
enabled only in the outermost call. This one goes beyond the reference.

**`concat` allocated an empty chunk where the reference swaps one.** When a
level's front chunk is empty the back takes its place; the displaced chunk is
already empty and already the right capacity, so it can stand in on the other
side, which is what `eject` does. Allocating a fresh K-slot vector instead was
most of what concatenating two short sequences cost: 104 → 29 ns. `merge-levels`
had the same bug.

**`fuse-chunks` used `filter` and `reverse` on a list of at most four chunks**,
which was 30% of `concat` at scale. It now skips empty chunks as it goes and
builds its answer in order.

**A write that changes nothing should not copy.** `set_shared` checks
`delta = 0 && x == get p i` and returns the chunk untouched; `pt-set` here now
propagates the same test up the spine. That one had a sting in it — see below.

**A chunk copy is one pass, not two.** `make-vector` with a filler writes every
slot and the blit then writes them again. `EphemeralChunk.sub` documents the
choice and takes the single-pass `Array.copy` whenever it may. Measured on a
58-slot vector: 63 ns for fill-then-blit, 21 ns for a straight copy.

### What the identity fast path broke

`ensure-owned!` in the iterator forced copy-on-write by **writing the current
element back to itself**, relying on `set` always copying. The new fast path
turned that into a no-op, so the iterator handed out a still-shared vector and
`sek-blit!` wrote into its own snapshot; `generic-tests.rkt` caught it on the
overlapping-blit case. The reference cannot use that idiom either, having the
same fast path, so the fix is to say what is meant: `chunk-own` / `pt-own` /
`eseq-own-at!` take ownership explicitly.

## What the generated code said

`PLT_LINKLET_SHOW_CP0=1 raco make` dumps each module after Chez's source
optimiser, and the `disassemble` package prints the machine code for a
procedure. Both were worth reading.

**Every struct was paying for a type check it did not need.** In the cp0 output,
each `#:authentic` field read still came out as

```racket
(if (unsafe-struct? t struct:lvl) (unsafe-struct*-ref t 1) (lvl-front t))
```

— five of them per level in `pt-ref`. The disassembly showed what that costs:
the predicate is not a comparison but a walk of the struct type's *ancestry
vector*, three dependent loads and a depth test:

```
(mov rdx (mem64+ rsi #x1))                     ; the record type
(mov r11 (mem64+ rdx #x9))                     ; its ancestry vector
(cmp (mem64+ r11 #x1) rdx) (jl ...)            ; compare depth
(shr rdx #x1)
(cmp (mem64+ r11 #x1 (* rdx #x1)) r8) (jnz ...)
```

Declaring the struct `#:sealed` — which only forbids subtyping, and which
`racket/treelist` does — reduces the whole thing to one comparison:

```
(cmp (mem64+ rsi #x1) rdi) (jnz ...)
```

Thirteen structs across seven files. On its own that is 11% off an `eseq`
push-back.

**Every module is compiled in unsafe mode, and checks its arguments itself.**
`(#%declare #:unsafe)` removes the implicit checks that survive cp0 — worth
1.4× on indexing and 1.7× on push-back — but it also means a struct accessor no
longer raises when handed the wrong kind of value: it reads whatever is at that
offset. So the checking that used to happen incidentally, as a side effect of a
safe accessor failing, is now deliberate. Fifty-seven functions across six
modules gained an explicit `unless`, behind six one-line macros (`check-pseq`,
`check-eseq`, `check-iter`, `check-segment`, `check-parray`, `check-earray`)
joining the `check-sek` that `generic.rkt` already had. With the structs sealed
each is a single pointer comparison, about 0.35 ns — visible on
`eseq-push-back!`, which went from 4.69 ns to 4.86, and swamped everywhere else
by what unsafe mode buys: `eseq-ref` went from 26.8 to 22.9 in the same change.

`sek/tests/error-tests.rkt` is what holds that line. It calls the public surface
with wrong types and out-of-range indices across about 150 cases and insists on
an exception, because without the checks those are not failures but reads of
arbitrary memory. It found a hole the moment it was written: `earray-length`
had no check, and quietly read a field off whatever it was handed. The library
now has more error coverage than it did when it was compiled safely, because
the checks are deliberate rather than incidental.

Two modules stay safe: `check.rkt`, the Appendix A validator, whose whole job is
to be suspicious of the structures it walks, and `config.rkt`.

**And one thing the generated code made look worse than it is.** `chunk-item-at`
returns two values, which cp0 renders as a `call-with-values` around an
arity-checking `case-lambda` — alarming to read on the hottest path in the
library. Measured, returning two values costs 1.18 ns where packing the same
two numbers into one fixnum and unpacking them costs 2.19. Chez handles it;
the "optimisation" would have been a pessimisation, and the only way to know
was to measure rather than to read.

| ns | before | after | OCaml | treelist |
| --- | ---: | ---: | ---: | ---: |
| `pseq-ref` | 35.6 | **22.2** | 29.9 | 6.6 |
| `eseq` push-back | 8.8 | **4.7** | 4.7 | — |
| `apply-sequential` | 27.5 | **15.1** | — | 6.0 |
| traversal | 1.78 | **1.67** | 1.9 | 1.77 |

### Where the settings live

`config.rkt` holds its settings in `set!`-ed module-level variables, and the
disassembly of `eseq-push-back!` showed what that was costing: before doing any
work it pushed a frame and jumped, to call `(check-iterator-validity?)` and read
one of them. A cross-module call, per push, to fetch a boolean.

`begin-encourage-inline` on the eight accessors removes the call. What it
cannot remove is the two dependent loads underneath, and it is worth being
precise about why, because the obvious fixes do not help. Five ways of holding
a setting that is chosen at run time and read constantly, each read from
another module, 40 million reads each:

| | ns |
| --- | ---: |
| `set!`-ed module variable *(what config.rkt does)* | **0.457** |
| vector in an unassigned variable | 0.484 |
| mutable struct field | 0.489 |
| box in an unassigned variable | 0.519 |
| compile-time constant | 0.412 |

The current representation is the fastest of the four mutable ones, and the
whole spread is 0.08 ns. The generated code says why — they are the same code:

```
set!-ed:  (mov rcx (mem64+ r15 #xb))     vector:  (mov rcx (mem64+ r15 #xb))
          (mov rcx (mem64+ rcx #x9))              (add rbp (mem64+ rcx #x9))
          (add rbp rcx)
```

Racket already represents an assigned module-level variable as a box, so
"use a box instead" is not a change of representation; a one-element vector and
a mutable struct field have the same shape too. Only a constant is cheaper, and
that would give up runtime configuration.

So the way to stop paying for a setting is to stop reading it.
`check-iterator-validity?` was read on every push, pop and set, and did not need
to be: the version protocol already carries the same information in the sign of
the version, and only iterator *creation* ever makes it positive. Moving the
test there leaves a mutation testing the sign of a field it has already loaded.
The setting is still honoured — with checking off the version never goes
positive, so nothing is ever invalidated and `eseq-iterator-valid?` answers `#t`
— and `SEK_CHECKITER=0` conformance confirms it.

`overwrite-empty-slots?` is the other hot read, on every owned chunk pop. It
stays: it governs whether a vacated slot is cleared, which is a real
garbage-collection semantic with nowhere natural to cache it, and it is a
well-predicted branch on a value that is in L1 after the first read.

### Building a chunk at a time

Reading `EphemeralSequence.init` in the reference turned up an algorithmic
difference rather than a compilation one. It builds through
`create_by_segments`, filling each chunk with `EChunk.init`; `build-eseq` here
pushed elements one at a time, and a push tests whether the back chunk is full,
invalidates the iterators and consults the ownership id once per element.

`pseq-build` now assembles the tree directly: the first chunk is the front, the
last is the back, and the rest are pushed into the middle. Every chunk but the
last is full, so the density invariant holds by construction. For the
operations that do not know the length in advance — `map`, `filter`, `reverse`,
`append-map`, `for/eseq`, `sequence->eseq` — a `pseq-builder` collects into a
chunk-sized buffer and emits whole chunks, holding one back so that the last
full chunk can become the level's back.

| ns per element | before | after | OCaml |
| --- | ---: | ---: | ---: |
| `build-eseq` | 5.30 | **3.18** | 2.7 |
| `filter` | 4.32 | **3.62** | 3.2 |

That left construction still going through `chunk-of-vector`, which is two
allocations and three passes over the data: build the elements into one vector,
allocate a capacity-sized support, copy between them. `chunk-build` fills the
support directly, in one allocation and one pass, and `chunk-of-fresh-vector`
lets the builder hand its buffer over instead of copying it — the buffer is
replaced on the next line and never read again. On a capacity-128 chunk that is
104 ns against 254.

| ns per element | before | after | OCaml |
| --- | ---: | ---: | ---: |
| `build-eseq` | 3.18 | **1.96** | 2.7 |

`perf` agrees on why: 126.0 instructions and 17.0 cycles per element before,
63.4 and 8.8 after.

### What the vector primitives are worth

Racket has fused vector operations that match these shapes exactly, and
`gvector`'s own `grow-vec` already uses one. Three were worth testing; the
results split two ways.

| capacity 128 | ns |
| --- | ---: |
| `make-vector` then `vector-copy!` (grow) | 157.7 |
| **`vector*-extend`** (grow) | **69.1** |
| `vector-copy` then `vector-set!` (copy-on-write of one slot) | 38.1 |
| **`unsafe-vector*-set/copy`** (copy-on-write of one slot) | **36.4** |
| **`make-vector` then a fill loop** (a full chunk from a closure) | **108.0** |
| `build-vector` (a full chunk from a closure) | 182.4 |

`vector*-extend` is worth 2.3×, and the growable array the benchmark uses as a
contender now grows with it. `unsafe-vector*-set/copy` is a smaller win, 4%,
but copy-on-write of one slot is *precisely* what it is for and it is now what
`chunk-set` does when the chunk is aligned.

`build-vector` went the other way, and overturned a design: it looks like the
obvious way to write the fill in `chunk-build` and it is 1.75× *slower* than a
hand-written loop, because it is a generic library function where the loop
compiles to a store per iteration.

One idea tested on its own and rejected: a chunk's support has to start out
filled with the empty marker, which `make-vector` does with a fill loop, so
keeping one pre-filled template around and copying it should write the same
words through a copy rather than a fill. It measures 104.3 against 108.0 at
capacity 128 and 16.6 against 16.4 at capacity 16 — a wash, and not worth a
piece of global mutable state. The fill is not where the time goes.

### Where `filter` was really spending its time

`filter` was the last scenario meaningfully slower than the reference, and both
implementations have the same shape — iterate, test, push — so the difference
had to be in one of the three parts. Measuring them separately, on 100000
elements:

| ns per input element | |
| --- | ---: |
| traversal alone | 1.73 |
| traversal + the benchmark's `(zero? (modulo x 3))` | 3.60 |
| traversal + the same test in fixnum arithmetic | 2.57 |
| the whole of `filter` | 4.70 |

Two things fall out of that. The first is that **the predicate cost 1.7 ns of
the 4.7** — more than the traversal — and about 1.0 ns of that is generic
arithmetic dispatch that the reference does not pay, since OCaml's `x mod 3`
on an `int` is a machine instruction. That is a property of the benchmark's
predicate, not of either sequence; the scenario keeps `modulo`, because that is
what a Racket program actually writes, but a reader comparing the two columns
should know that a fifth of the Racket number is arithmetic.

The second is that the remaining 1.1 ns went on **closure calls, not on
building**. Written with `sek-for-each` and `build-from`, a per-element
operation costs three of them: the segment loop calls the traversal's
procedure, that calls the caller's, and the caller calls `emit`. The
`build-by-segments` macro collapses all three — the segment loop *is* the
caller's loop, and `pseq-builder-add!` was split so that its hot half (a store
and a bounds test) is small enough for the inliner to copy across the module
boundary, with the chunk-full case left behind a call. An element the predicate
drops now costs the test and nothing else.

| ns per input element | before | after | OCaml |
| --- | ---: | ---: | ---: |
| `filter`, `modulo` predicate | 4.70 | **3.97** | 3.1 |
| `filter`, fixnum predicate | 3.24 | **2.72** | 3.1 |

`filter` is now 0.37 ns above bare traversal-plus-predicate, so the building
half is very nearly free. `map` and `filter-map` are built the same way.

The builder had one bug, and the Appendix A validator caught it rather than any
test of the result: the case where nothing has been emitted yet returned the
buffer as a compact vector without testing the short threshold, which may be
*below* the chunk capacity. At leaf 8 and threshold 6 a seven-element result
came back compact when it had to be a tree — `compact sequence of length 7
exceeds the threshold 6`. It only appears at capacities the conformance sweep
runs and the defaults do not.

### The list plumbing in merge

`concat` was the last scenario meaningfully above the reference, and the shape
of the gap said it was not a constant factor: the cost was not monotonic in n.
It measured 326 ns at n = 10^4 and 373 ns at n = 10^6 -- *cheaper* on a
sequence a hundred times longer. Walking n across the range found a step:

| n | 4000 | 8000 | **10000** | 16000 | 32000 | 64000 |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| before | 143 | 167 | **326** | 290 | 279 | 247 |
| after | 102 | 134 | **184** | 196 | 188 | 224 |

The step sits where the middle sequence gains a level. A level holds a front
chunk and a back chunk of node capacity 16, so a middle of up to 32 chunks --
32 x 128 = 4096 elements a side, n = 8192 -- needs no middle of its own, and
one more chunk than that adds a whole level of recursion to `merge`.

Profiling a concatenation at that size said where the extra level went, and it
was not the chunk copying. `last` and `drop-right` together were **36% of
`pt-merge`**. `merge` threads a list of leftover chunks down the spine, never
more than a handful of them, and the code split that list with the obvious
library functions -- which are contract-checked, and walk the list once to
validate and again to do the work, and then a third time for the other half of
the same split. `split-last` returns both halves in one pass, and `snoc`
replaces `(append L (list x))`. Nothing about the algorithm changed.

Re-profiling afterwards puts 87% of a concatenation in `fuse-chunks`, which is
the density invariant being repaired -- real work, already down to a
`vector-copy!` per chunk, and what the operation is for.

### What the disassembly said next

A second pass over the operations that were still at or above the reference,
this time reading what `disassemble` produced rather than what cp0 did.

**`eseq-push-back!` was making two calls.** It pushed a six-word frame and
jumped, once to ask `chunk-full?` whether two fields were equal and once to
push -- because `chunk.rkt` had no `begin-encourage-inline` anywhere in it.
With the small accessors and the push dispatchers inlined there is nothing left
to call, so Chez also drops the frame and the stack-overflow check and the
procedure becomes a leaf.

**`chunk-capacity` was a three-deep pointer chase.** It read the length off the
support's vector: chunk to support to data to the vector header, then a shift
and a mask to get the length out of the header word -- on every push, every pop
and every wrap. A support now carries its capacity, which it can because its
data is allocated with it and never replaced. `support-data`, `support-cap`,
`chunk-support` and `chunk-id` became immutable at the same time, for a related
reason: nothing assigns them, and saying so lets the compiler treat the loads
as pure and share them across the stores to `head` and `size`.

**An ownership id was a counter.** So `chunk-owned?` compared with `eqv?`, and
`eqv?` on two values that might be bignums is a pointer test, three tag tests
and a call to the generic procedure. A counter *can* reach a bignum, so the
compiler is right to be careful; an id is now a record, and the comparison is
`eq?`, which is one instruction.

**`pseq-ref` examined the tag of its index three times.** Nothing carries "is
an exact nonnegative integer" forward as "is a fixnum", so each comparison
re-tested it. Guarding on `fixnum?` instead also fixed something worse:
everything below indexes with `unsafe-fx` operations, which are undefined on a
bignum, and the wider guard let one through. It happened to raise anyway, on
the accident that a heap address compares larger than any length. The
disassembly went from 401 lines to 262.

**`pt-ref` carried five fixnum-tag guards and three overflow checks** around
three comparisons and two subtractions, because a weight read out of a struct
field is just a value. The same held in `chunk-item-at`, `chunk-ref-atomic`,
`pt-set`, `chunk-set-atomic`, `chunk-own-atomic`, `chunk-set`, the persistent
pushes, `chunk-blit!` and the version protocol.

**The folds walked a segment as an offset plus a counter**, paying a generic
addition per element, where they can walk the support indices directly.

| ns | before | after |
| --- | ---: | ---: |
| `eseq` push-back/pop-back | 4.24 | **3.48** |
| `eseq` push-front/pop-front | 4.42 | **3.63** |
| `pseq-ref` at random | 20.44 | **16.98** |
| `eseq-ref` at random | 20.51 | **17.42** |
| `eseq-set!` at random | 26.08 | **22.85** |
| traversal by fold, per element | 1.57 | **1.21** |
| `filter`, per input element | 3.97 | **3.68** |

### The array was dividing

The §2 transient array descends one level by splitting an index into a child
position and an index within that child, and it did that with `quotient` and
`remainder` -- two integer divisions per level, each tens of cycles. The spans
are products of the capacities and both defaults are powers of two, so the
usual case is a shift and a mask. `max-item-weight-shift` already tabulates the
exponent for exactly this and `chunk-item-at` already used it; `array.rkt` was
the one still dividing. The division stays for capacities that are not powers
of two.

| ns | before | after |
| --- | ---: | ---: |
| `earray-ref` at random | 17.98 | **8.50** |
| `parray-ref` at random | 18.24 | **8.39** |
| `earray-ref` ascending | 16.93 | **7.13** |
| `earray-set!` at random | 20.26 | **11.65** |
| `parray-set` at random | 104.62 | 101.56 |

`parray-set` barely moves because it is dominated by copying a node per level,
which is what a persistent write is.

### And the treelist

The same reading applied to `racket/treelist`, since it is the structure this
one is measured against. `treelist-ref` pushed a frame to call
`check-treelist-index` before doing any work -- that procedure has optional
arguments and three ways to raise, so it cannot be inlined, though the case it
is asked about is two comparisons -- and then pushed another to call
`treelist-node-for`, which repeats the `impersonator?` test its caller has
already done and hands back two values. And `radix`, which runs once per level
of every descent, masked with `bitwise-and` where both operands are fixnums.
`mutable-treelist-ref` did the whole check sequence twice, once itself and
again inside `treelist-ref`.

| ns | before | after |
| --- | ---: | ---: |
| `treelist-ref` at random | 6.35 | **4.53** |
| `treelist-ref` ascending | 6.49 | **3.74** |
| `mutable-treelist-ref` at random | 10.25 | **7.72** |
| `mutable-treelist-set!` at random | 10.09 | **8.75** |

The treelist rows in the tables above are measured against that treelist, from
branch `treelist-faster-ref` in a fork of Racket, on top of the
`treelist-copy-for-mutable` fix described at the end of this file. Both are
unmerged.

One change there was tried and rejected: `vector*-add-right` is
`vector*-append` with a freshly allocated one-element vector, and
`vector*-extend` computes the same thing in one primitive and one allocation --
but it measures slower, 3.41 ns against 2.28 appending to a 4-slot node, 7.13
against 5.02 at 16 and 11.15 against 8.98 at 31.

### Would stencil vectors help?

No, for three separate reasons. A Chez stencil vector holds at most **58 slots**
(26 on a 32-bit build), and the default leaf capacity is 128, so they could not
back the chunks that hold the data. They are *slower* at the one operation they
would be used for — `unsafe-stencil-vector-update` fuses allocate, copy and
substitute into one primitive, which is exactly the copy-on-write step in
`chunk-set`, and it measures 10.1 ns against `vector-copy` plus `vector-set!`'s
7.0 at 16 slots, 18.2 against 11.5 at 32, and 36.1 against 20.9 at 58. And the
representation is wrong: a stencil vector's mask is a *set* of occupied slots,
which is what makes it right for a HAMT node, where children are sparse. A
chunk is a contiguous ring with a `(head, size)` view that has to push and pop
at both ends in constant time and share one mutable support between several
views. Its mask would always be a run of ones — strictly less information than
`(head, size)` — and indexing would need a popcount where `wrap+` does now.

## A Racket bug this turned up

Measuring `mutable-treelist` on the operations it had previously been excluded
from ran straight into a bug in `racket/mutable-treelist`, as of Racket 9.3.0.2:

```racket
(require racket/mutable-treelist racket/list)
(define a (list->mutable-treelist (build-list 200 values)))
(mutable-treelist-drop! a 12)
(mutable-treelist-copy a)       ; vector-length: contract violation
(mutable-treelist-snapshot a)   ; the same
```

The treelist itself is fine after the drop — `ref`, `length` and iteration all
give the right answers — so nothing detects the damage until you try to copy it.

A treelist node is either a bare vector, when the subtree below it is leftwise
dense, or a `(cons children sizes)` pair when it is not. `treelist-drop` leaves
size vectors behind on the nodes it rebuilds, which is exactly what they are
for. `treelist-copy-for-mutable`, which is what both `mutable-treelist-copy`
and `mutable-treelist-snapshot` call to give the copy private leaf vectors,
walked the tree assuming every node was a bare vector:

```racket
(for/vector #:length (vector-length n) ([e (in-vector n)])
  (copy-node e (fx- height 1)))
```

so it handed a pair to `vector-length`. Every other node walk in the file goes
through `node-leftwise-dense?` / `node-children` / `node-sizes`; this was the
one that did not. The failure needs a tree with interior nodes, which is why
the existing tests — all on four-element treelists — never saw it.

The fix reaches the children through `node-children` and puts the size vector
back with `Node`, which returns the children vector unchanged when there are no
sizes, so the leftwise-dense case still allocates nothing extra:

```racket
(define children (node-children n))
(Node (for/vector #:length (vector*-length children) ([e (in-vector children)])
        (copy-node e (fx- height 1)))
      (node-sizes n))
```

Size vectors are never mutated — `treelist-set!` writes only into leaf vectors
— so they can be shared with the original rather than copied.

The regression test added to `treelist.rktl` is the failure itself, in the
style of the mutable-treelist tests beside it: drop from the front of a
200-element treelist, then copy it and snapshot it, then check that writing to
the copy leaves the original alone. Without the fix it aborts inside
`mutable-treelist-copy`.

Verified against `racket-test-core`'s `treelist.rktl` (1237 tests), `data-test`'s
`treelist-coverage.rkt` (500 randomised model-correspondence tests), and a
throwaway randomised check of copy and snapshot against a list model over
`drop!`/`take!`/`take-right!`/`drop-right!`/`sublist!`/`append!`/`prepend!`/
`insert!`/`delete!`/`reverse!` — 200 trials of 25 operations, which reproduces
the crash without the fix and passes with it. (`treelist-props.rkt` and
`mutable-treelist-props.rkt` do not compile in this checkout — `check-guided-property`
is undefined anywhere in the tree — which has nothing to do with this change.)
