# Benchmarks

```sh
./run.sh                     # this library, one scenario per process
./run.sh --quick             # smaller sizes
racket -y main.rkt stack     # a single scenario

./build-ocaml.sh             # build the OCaml reference (needs ocamlfind)
./run.sh --ocaml             # the same scenarios, run against it

racket -y bm.rkt             # the benchmark from racket/data PR #34, plus sek
```

`main.rkt` follows the benchmarks that accompany the OCaml library — stack,
reach, iteration, traversal, construction, fill, split — and Figures 17 and 18
of the paper, and adds scenarios for transience, which the reference's own
suite does not measure (it uses `snapshot` and `edit` only to set sequences
up). `bm.rkt` is the benchmark from the gvector PR, unmodified except for the
added sek rows, so the comparison is in its terms rather than ours.

Every number in `main.rkt` is nanoseconds per operation, the best of three
trials after a calibration run, so numbers are comparable down a column.
`bm.rkt` reports its own totals in milliseconds, as upstream does.

Three things worth knowing if you re-run this:

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

Numbers below are from one machine: Racket CS 9.3, OCaml 5.4.0 (flambda `-O3`),
default settings (leaf capacity 128, node capacity 16, threshold 32).

## Against other Racket sequences

Contenders: `treelist` and `mutable-treelist` from `racket/treelist` (RRB
trees, the closest analogue), `gvector`, immutable lists, and a mutable box
holding a list. A dash means the structure has no constant-time way to do it.

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
