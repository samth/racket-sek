# Benchmarks

```sh
./run.sh                     # this library, one scenario per process
./run.sh --quick             # smaller sizes
racket -y main.rkt stack     # a single scenario

./build-ocaml.sh             # build the OCaml reference (needs ocamlfind)
./run.sh --ocaml             # the same scenarios, run against it
```

The scenarios follow the benchmarks that accompany the OCaml library — stack,
reach, iteration, traversal, construction, fill, split — and Figures 17 and 18
of the paper. Every number is nanoseconds per operation, the best of three
trials after a calibration run, so numbers are comparable down a column.

Two things worth knowing if you re-run this:

* The reference must be compiled in its **release** configuration. With
  assertions on it runs its own O(n) validator inside every operation and
  looks about thirty times slower than it is. `build-ocaml.sh` handles this;
  `conformance/build.sh` deliberately does the opposite.
* Each scenario runs in a fresh process. Otherwise one scenario's garbage is
  charged to the next, and the heap never shrinks.

Numbers below are from one machine: Racket CS 9.3, OCaml 5.4.0 (flambda `-O3`),
default settings (leaf capacity 128, node capacity 16, threshold 32).

## Against other Racket sequences

Contenders: `treelist` and `mutable-treelist` from `racket/treelist` (RRB
trees, the closest analogue), `gvector` from `data/gvector`, immutable lists,
and a mutable box holding a list. A dash means the structure has no
constant-time way to do it.

### The ends

`push` and `pop`, repeating "n pushes then n pops" until two million pushes
have happened, so n is the peak length.

```
stack: push-back / pop-back                     front stack: push-front / pop-front
              1000    100000   1000000                        1000    100000   1000000
eseq         10.72     10.82     11.47          eseq          10.84     10.84     11.52
pseq         12.86     13.06     14.39          pseq          13.11     13.33     14.62
treelist     21.70     38.66     45.55          treelist      38.21     161.4     203.8
mutable-tl   26.71     44.20     51.22          mutable-tl    43.88     168.0     208.8
gvector      19.02     19.36     28.38          gvector           -         -         -
                                                list           1.03      1.15      2.44

queue: push-back / pop-front
              1000    100000   1000000
eseq         10.71     10.62     11.36
pseq         13.13     13.44     14.69
treelist     28.82     57.24     69.34
mutable-tl   34.15     62.66     74.75
```

This is what the structure is for. `eseq` is flat in n and does not care which
end you use; a treelist costs 4× more at the back, 18× more at the front, and
degrades as the sequence grows. A gvector is fine at the back and has no front.
A Racket list is unbeatable as a front stack — and is only a front stack.

### Traversal, and what segments buy

```
traversal: for-each over the whole sequence, ns per element
                 100     10000   1000000
eseq            2.22      1.92      1.96
pseq            2.06      1.91      1.93
treelist        1.81      1.74      1.78
gvector         1.49      1.42      1.31
list            1.32      1.19      1.38
  pseq via fold 1.87      1.72      1.75
  pseq via iter 11.51     11.32     11.30
  vector        0.60      0.48      0.49
```

Sweeping a sek sequence costs about what sweeping a treelist costs, and about
40% more than a list. That is entirely down to segments: the same traversal
driven one element at a time through the iterator costs 11.3 ns, almost 6×
more. `sek-fold-left` and friends all take the segment path.

### Random access

```
random access: ref at random indices        hops: ref at a fixed stride, n = 10^6
                 100     10000   1000000                  +1        +8       +64     +4096
eseq           17.82     28.37     63.35    eseq       36.11     36.36     40.42     60.21
pseq            8.67     26.34     61.65    pseq       34.64     35.12     39.18     59.26
treelist        4.24      4.97     10.63    treelist    5.45      5.85      6.60     10.20
mutable-tl      8.65      8.97     16.34    mutable-tl  9.08      9.84      9.87     15.61
gvector         7.28      7.32     10.95    gvector     7.13      7.17      7.27     12.28
  vector        0.80      0.83      2.09    pseq iter   9.73     11.92     26.67     74.76
```

This is the operation the design gives up, and the measurement says so: `ref`
is about 6× a treelist's. The reason is structural rather than incidental —
reaching an element means descending the middle spine to the level that holds
it and then descending the chunk hierarchy back to depth 0, roughly twice the
pointer-chasing of an RRB tree, which indexes in one descent. Profiling shows
the time spread evenly across those steps, with no hotspot to remove; the
OCaml reference measures 54.77 ns for the same lookup, so this is the data
structure, not the port.

What the structure does offer instead is an iterator that remembers where it
is. Scanning with hops of one costs 9.7 ns rather than 35, because the cursor
stays inside the chunk it is already on.

### Updating

```
update: set at random indices              fill: overwrite k consecutive elements of 10^6
                 100     10000   1000000                        10      1000    100000
eseq           20.27     33.78     72.04    eseq (sek-fill!)  10.80      2.49      2.50
pseq           114.5     214.5     617.1    eseq (set! loop)  23.90     23.35     40.61
treelist       16.37     31.80     190.2    mutable-treelist   8.89      7.98      7.91
mutable-tl      8.22      8.32     16.64    gvector            9.91      9.02      9.03
gvector         9.22      9.29      9.35    vector             0.90      0.64      0.64
```

Scattered writes are the other thing the design gives up: a persistent `set`
copies a chunk per level, and with 128-element leaves that is a lot of copying.
Writes with any locality are a different story — going through writable
segments, `sek-fill!` overwrites at 2.5 ns per element, 16× faster than the
same range written one `eseq-set!` at a time, and three times faster than a
mutable treelist or a gvector.

### Building, concatenating, splitting

```
construction: n elements from scratch      concat / split at n = 10^6, ns per operation
                 100     10000   1000000                concat     split
eseq           10.04      9.29     10.41    pseq         816.2     703.1
pseq           11.46      9.34     10.29    treelist     755.6     130.8
treelist       13.63     32.13     48.10    list       5442636   4497195
mutable-tl     16.15     35.16     52.24
gvector         5.87      4.62     18.21
list            1.94      2.06     46.43
```

Construction is flat for sek and gets 5× worse for a treelist as the sequence
grows — the same effect as the push benchmark. Concatenation is a wash with
treelist; splitting is treelist's win by 5×.

### Snapshots

This is the point of the whole exercise, so it gets its own table. Racket's
`mutable-treelist` has a `snapshot` operation too, which makes it the honest
comparison.

```
one change plus one snapshot of an n-element sequence, ns
                     100     10000   1000000
eseq               133.2     207.6     209.1
mutable-treelist   73.32      3993   1417371
```

`eseq-snapshot` does not depend on the length of the sequence.
`mutable-treelist-snapshot` is linear in it: at a million elements one snapshot
costs 1.4 milliseconds, about 6800× more. That is the difference between the
ownership-identifier scheme of §2.4, where a snapshot just hands the sequence a
fresh identity, and copying.

It shows up as soon as snapshots are taken in a loop:

```
100000 pushes, keeping a snapshot every m of them, ns per push
                       m=1      m=10    m=1000  m=100000
eseq                 126.3     19.99     10.68     10.49
mutable-treelist         -         -     277.3     45.10
pseq (persistent)    12.51     12.36     12.48     12.37
treelist (persistent) 38.91    38.78     38.77     38.83
```

Snapshotting every thousandth push costs an ephemeral sek sequence essentially
nothing (10.68 versus 10.49 ns per push). The mutable-treelist columns for
m = 1 and m = 10 are missing because keeping that many copies of a growing
sequence needs more than ten gigabytes.

### Tuning

The chunk capacities are a real dial, and it trades exactly what the paper says
it trades:

```
capacities over 10^6 elements, ns per operation
                    128/16     64/16     32/16     16/16       8/8    256/32
pseq-ref             61.24     74.20     90.52     110.7     185.4     52.32
pseq-set             600.3     551.7     531.0     591.8     764.4     851.2
eseq push/pop        10.40     10.95     11.54     13.09     17.29     10.50
traversal             1.74      2.09      2.88      4.02      7.20      1.53
```

Bigger chunks mean a shallower tree, so reads, pushes and traversal all get
faster; but a persistent write copies a chunk per level, so `set` is worst at
both ends and best around 32-element leaves. The shipped default of 128/16 is
the paper's, and it is the right call unless an application does scattered
persistent writes.

## Against the OCaml reference

Both implementations running the same scenarios, at n = 10^6 (or the natural
size for the scenario). The ratio is Racket ÷ OCaml, so above one means this
library is slower.

| scenario | Racket | OCaml | ratio |
| --- | ---: | ---: | ---: |
| stack push/pop, ephemeral | 11.47 | 5.70 | 2.0 |
| stack push/pop, persistent | 14.39 | 14.74 | **0.98** |
| queue push/pop, ephemeral | 11.36 | 5.74 | 2.0 |
| traversal, persistent | 1.93 | 1.95 | **0.99** |
| traversal via iterator | 11.30 | 4.63 | 2.4 |
| random access, persistent | 61.65 | 54.77 | 1.13 |
| hops of one, via iterator | 9.73 | 5.97 | 1.6 |
| set at random indices, persistent | 617.1 | 808.3 | **0.76** |
| set at random indices, ephemeral | 72.04 | 100.3 | **0.72** |
| construction | 10.29 | 3.10 | 3.3 |
| concat | 816.2 | 334.1 | 2.4 |
| split | 703.1 | 153.1 | 4.6 |
| one change plus one snapshot | 209.1 | 75.5 | 2.8 |
| snapshot after every push | 126.3 | 1211 | **0.10** |
| fill, 10^5 elements | 2.50 | 0.51 | 4.9 |

The persistent operations are at parity or better — pushing, popping,
traversing and indexing a persistent sequence costs what it costs in native
OCaml, and persistent `set` is faster here. What costs more is everything
dominated by in-place mutation and allocation: ephemeral push and pop are 2×,
construction 3×, `fill` 5×. That is the shape one expects from Racket's write
barrier and allocator against native code with flambda.

Two entries stand out.

**Snapshotting after every push is ten times faster here** (126 versus 1211 ns).
The OCaml `snapshot` performs a shallow copy, duplicating the front and back
chunks each time; this implementation shares them and lets the next write pay
for a copy if there is one. In a snapshot-heavy loop, where the next write
usually extends a chunk monotonically and so copies nothing, sharing wins
outright. That was a documented deviation; it turns out to be the better
tradeoff for this workload, and the worse one for a single snapshot in
isolation (2.8× slower).

**`split` is 4.6× slower**, and the gap grows with n where the reference's is
flat. Both are within the O(K log_K n + log²_K n) bound; the constant is worse
here.

Finally, the OCaml comparison is what settles the random-access question above:
at 54.77 ns for the reference against 61.65 here, indexing is slow because of
how the structure is shaped, not because of how it was ported.
