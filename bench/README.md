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
eseq            2.19      1.89      1.95
pseq            2.04      1.88      1.94
treelist        1.81      1.74      1.79
gvector         1.41      1.32      1.34
list            1.26      1.10      1.27
  pseq via fold 1.87      1.72      1.77
  pseq via iter 11.21     11.03     11.06
  vector        0.59      0.50      0.51
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
eseq            8.94      8.68      9.75    pseq         827.9     705.4
pseq            9.58      8.71      9.62    treelist     788.8     129.3
treelist       13.57     31.85     46.83    list       5590487   4377206
mutable-tl     16.78     34.61     51.67
gvector         5.89      4.60     20.38
list            1.88      2.00     41.16
```

Construction is flat for sek and gets 5× worse for a treelist as the sequence
grows — the same effect as the push benchmark. Concatenation is a wash with
treelist; splitting is treelist's win by 5×.

### Filtering, the paper's motivating example

§5.4 gives filtering a persistent sequence as the case for iterators: read
through an iterator on the source, write into an ephemeral destination, and
the whole thing is O(n + K). That is what `sek-filter` does.

```
filter: keep one element in three, ns per input element
                 100     10000   1000000
pseq            7.42      6.48      6.68
eseq            7.03      6.53      6.75
treelist        5.90     12.33     17.29
  list          3.99      3.61      4.97
  vector        4.55      4.49      7.53
```

Flat in n, and 2.6× faster than `treelist-filter` at a million elements.

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

| | eseq | pseq | treelist | mutable-treelist | gvector |
| --- | ---: | ---: | ---: | ---: | ---: |
| `apply-sequential`, per lookup | 27.5 | 27.3 | **5.8** | 9.9 | 7.5 |
| `update-sequential`, per set | 30.4 | 237.1 | 42.0 | **9.4** | 10.3 |
| `apprepend`, per push | **9.7** | 12.9 | 160.7 | 155.6 | — |
| `peek`, per first+last pair | 40.8 | 16.2 | **9.7** | 23.5 | 21.3 |
| `tail`, per persistent pop | — | **13.4** | 79.6 | — | — |
| `slice`, per slice | — | 1020 | **191.0** | — | — |
| `map`, per element | 10.3 | **10.1** | 39.6 | — | — |
| `filter` keeping all, per element | 10.3 | **10.2** | 39.2 | — | — |
| `take-lin`, per step | 521.5 | 466.4 | **79.0** | — | — |
| `push_move`, per element | **8.7** | 12.4 | 39.8 | 44.5 | 5.1 |
| `split-parts`, per element | — | 2.00 | **1.97** | — | — |

Five things come out of this.

**`apprepend` is the clearest win in the whole suite, and it is Scala's own
benchmark.** Alternating a push at each end costs sek a flat 9.7 ns and a
treelist 160.7 ns — 17× — and the gap is entirely a function of length:

| `apprepend`, ns per push | 10 | 1000 | 100000 |
| --- | ---: | ---: | ---: |
| eseq | 15.40 | **9.58** | **9.72** |
| pseq | 13.66 | 12.72 | 12.86 |
| treelist | **6.01** | 28.74 | 160.7 |
| mutable-treelist | 9.42 | 33.63 | 155.6 |

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
| eseq | 7.49 | 22.23 | 27.45 |
| pseq | 8.31 | 21.96 | 27.26 |
| treelist | 4.55 | 5.06 | 5.78 |
| gvector | 7.65 | 7.45 | 7.45 |
| vector | 1.56 | 1.42 | **1.42** |
| pseq via iterator | **1.83** | **1.78** | 2.73 |

Read through `ref`, sek is 4.7× behind a treelist. Read through `in-pseq`, the
same walk is 2.7 ns — twice as fast as the treelist's cached `ref`, and within
2× of a raw vector. The library's answer to sequential access is a first-class
iterator, and it is a better answer than a display; it is just not spelled
`ref`.

**Bulk element-wise work is 4× ahead, for the same reason.** `map` and `filter`
hand out segments, so their inner loop touches a raw vector:

| ns per input element, n = 10^5 | 100% kept | 50% | 0% |
| --- | ---: | ---: | ---: |
| pseq | 10.23 | 6.49 | 2.09 |
| eseq | 10.29 | 6.54 | 2.11 |
| treelist | 39.16 | 19.67 | **1.77** |
| list | **4.35** | **2.71** | 1.12 |
| vector | 6.91 | 3.86 | 1.08 |

Scala measures three filter ratios because they separate two costs. At 100% and
50%, where the output has to be built, sek is 3–4× ahead of a treelist. At 0%,
where nothing is built, the treelist wins: all that is left is the traversal,
and its traversal is slightly cheaper.

**Slicing is what this design gives up.** `slice`, `take-lin` and `drop-lin`
all say the same thing — a treelist splits 5–6× faster, and the gap grows with
n where the treelist's is nearly flat. This agrees with the OCaml comparison
further down, where `split` is the one operation 4.6× off the reference. The
`eseq (transient)` rows in `take-drop` are immer's `_mut` variants; they are
slower than the persistent ones here, because an O(1) `eseq-copy` still leaves
the destructive split to do the same work.

**And `push_move` reproduces immer's headline.** Building through a transient
and freezing at the end costs 8.7 ns per element against 39.8 for repeated
persistent `treelist-add` — 4.6× — and it also beats sek's own persistent
`pseq-push-back` (12.4). That is the comparison Clojure spells
`(persistent! (reduce conj! (transient []) xs))`, and it holds here.

One oddity worth recording, and it is fixable: `peek` on an `eseq` costs 40.8 ns
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
the classic program, with the pair operations swapped out
pairs                        413 ms
pseq                        3423 ms
treelist                    4187 ms
mutable-treelist            7903 ms
gvector                    10686 ms
eseq                       15850 ms
```

Pairs win by a factor of eight, and should: the search never holds more than
eight elements, `cons` is two words, `cdr` is free, and appending two lists of
four is nothing. Nothing in this benchmark asks for anything a pair list is
bad at.

Among the sequence structures, `pseq` is the fastest — 22% ahead of
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
vector                      1363 ms
box-of-list                 1608 ms
eseq                        7957 ms
mutable-treelist            8033 ms
gvector                     9588 ms
```

Here `eseq` is level with `mutable-treelist` and ahead of `gvector`. All three
are about six times a bare vector, because a stack that never exceeds eight
entries has no use for any of their machinery: the vector version is a
`vector-set!` and an integer, and everything else is paying for growth it will
never need.

Writing this benchmark turned up one fixable thing. `eseq-copy` went through
`eseq-snapshot` and `pseq-edit`, which for a short sequence converted the tree
to a vector and the vector back into a chunk. It now hands both sequences a
fresh identity and shares everything, leaving the copying to whichever one
writes first — the same reasoning as `snapshot`, and O(1) rather than
O(n + K). That is a 3× improvement on the `eseq` row above.

## Against the OCaml reference

Both implementations running the same scenarios, at n = 10^6 (or the natural
size for the scenario). The ratio is Racket ÷ OCaml, so above one means this
library is slower.

| scenario | Racket | OCaml | ratio |
| --- | ---: | ---: | ---: |
| stack push/pop, ephemeral | 10.93 | 5.72 | 1.9 |
| stack push/pop, persistent | 14.08 | 14.81 | **0.95** |
| queue push/pop, ephemeral | 10.88 | 5.95 | 1.8 |
| traversal, persistent | 1.94 | 1.98 | **0.98** |
| traversal via iterator | 11.06 | 4.72 | 2.3 |
| random access, persistent | 60.15 | 55.97 | 1.07 |
| hops of one, via iterator | 9.59 | 6.00 | 1.6 |
| set at random indices, persistent | 650.5 | 767.8 | **0.85** |
| set at random indices, ephemeral | 68.06 | 97.10 | **0.70** |
| construction | 9.62 | 3.23 | 3.0 |
| concat | 827.9 | 328.0 | 2.5 |
| split | 705.4 | 153.9 | 4.6 |
| filter | 6.68 | 3.84 | 1.7 |
| edit, one update, snapshot | 352.5 | 249.1 | 1.4 |
| one change plus one snapshot | 209.2 | 74.83 | 2.8 |
| snapshot after every push | 127.6 | 1141 | **0.11** |
| fill, 10^5 elements | 2.44 | 0.51 | 4.8 |

The persistent operations are at parity or better — pushing, popping,
traversing and indexing a persistent sequence costs what it costs in native
OCaml, and both flavours of `set` are faster here. What costs more is
everything dominated by allocation: construction 3×, `concat` 2.5×, `fill` 5×.
That is the shape one expects from Racket's allocator against native code with
flambda.

Two entries stand out.

**Snapshotting after every push is nine times faster here** (128 versus
1141 ns). The OCaml `snapshot` performs a shallow copy, duplicating the front
and back chunks each time; this implementation shares them and lets the next
write pay for a copy if there is one. In a snapshot-heavy loop, where the next
write usually extends a chunk monotonically and so copies nothing, sharing wins
outright. It is the worse tradeoff for a single snapshot in isolation (2.8×
slower), and the better one whenever snapshots come in a stream.

**`split` is 4.6× slower**, and the gap grows with n where the reference's is
flat. Both are within the O(K log_K n + log²_K n) bound; the constant is worse
here.

Finally, the OCaml comparison is what settles the random-access question above:
at 55.97 ns for the reference against 60.15 here, indexing is slow because of
how the structure is shaped, not because of how it was ported.

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

Verified against `racket-test-core`'s `treelist.rktl` (1237 tests, passing, and
failing before the fix with the regression test added), `data-test`'s
`treelist-coverage.rkt` (500 randomised model-correspondence tests), and a
randomised check of copy and snapshot against a list model over
`drop!`/`take!`/`take-right!`/`drop-right!`/`sublist!`/`append!`/`prepend!`/
`insert!`/`delete!`/`reverse!` — 200 trials of 25 operations, which reproduces
the crash without the fix and passes with it.
