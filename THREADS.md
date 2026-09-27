# Threads and memory safety

This note explains the commits that change what sek promises when threads
share its sequences, and why we believe the new promises hold.

| Commit | Change |
|---|---|
| `bcd56bd` | Compile safe, and skip checks only where nothing can defeat them |
| `ae37c96` | Let persistent sequences be shared between threads |
| `de633d2` | Check the run a segment is built from |
| `acc3780` | Let several threads read an ephemeral sequence at once |
| `5ae61e2` | Let the next update adopt a snapshot's fold of the inner chunks |
| `b4d0b02` | Win back what safe compilation cost the element loops and the push path |
| `ede8a0c` | Say precisely what threads may do with a sequence |
| `9def697` | Build sek with every unchecked operation checked, or with races widened |
| `16a40e1` | Add a fuzzer that races threads on shared sequences |
| `1c55da4` | Keep an iterator from reading at a sentinel after a race |
| `8b556ee` | Keep a pop finite when a race leaves a chunk's size negative |

## What changed for a user

| Use | Before | After |
|---|---|---|
| A persistent sequence, used by any number of threads, including pushes | Two threads extending sequences that share storage could get each other's elements, and could leave chunks malformed | Safe, with no synchronization |
| An ephemeral sequence read by several threads at once, including iterating it, snapshotting it and copying it | The readers could corrupt the sequence and crash the process | Safe, with no synchronization |
| An ephemeral sequence updated by one thread while others use it | Could crash the process | Still unsupported: an update needs exclusive access. A program that breaks this gets wrong results or an `exn:fail:sek`, but memory is never corrupted |
| A segment built by a caller with a run that does not fit its vector | Read past the end of the vector | Raises `exn:fail:contract` |

The documentation's "Threads" section states the contract precisely.  In
short: any number of threads may use a persistent sequence at once; any
number may use an ephemeral sequence at once as long as none of them updates
it; an update -- an `eseq-...!` operation, a write through an iterator or a
segment, or `sek-append*` of an ephemeral sequence -- needs exclusive access;
sharing between the flavors, as between a snapshot and the sequence it came
from, needs no synchronization; one iterator belongs to one thread at a time;
and a program that breaks these rules gets unspecified results, but no use of
the library corrupts memory.

## Why the old code was unsafe

Three separate things went wrong.

**The library's memory safety rested on its invariants.**  Every module was
compiled with `(#%declare #:unsafe)`, which makes struct accessors, vector
references and fixnum arithmetic skip their checks.  That is sound as long as
the invariants hold, because the invariants keep every index inside its vector
and every field access on a value of the right kind.  But two threads updating
one ephemeral sequence break the invariants in the middle of an operation, and
then an index computed from a chunk's head and size can land outside the
vector, or a read of an empty slot can hand a symbol to an accessor that
expects a chunk.  Fuzzing crashed the process this way ("invalid memory
reference").

**A persistent push writes into shared storage.**  The paper's monotonic
update extends a chunk in place when the chunk's view ends where its
support's occupied region ends: the new slot lies outside every view, so no
sequence can see the write.  The push tested that condition, then wrote the
slot, then advanced the support's size -- three separate steps.  Two threads
pushing onto sequences that share a support could both pass the test and both
write the slot, and one of them then returned a sequence ending in the other's
element.  This needs no ephemeral sequence at all: two threads that push onto
the same persistent sequence race.

**Some reads wrote.**  Creating an iterator -- which every `in-sek` loop,
fold and `sek-for-each` does -- moved the inner chunks of an ephemeral
sequence into its middle sequence, and so did `eseq-snapshot`.  Two threads
that only read one ephemeral sequence could therefore race on those writes.

Separately from threads, the exported `segment` constructor accepted any start
and length, and `segment-for-each` reads a segment without checking each
index, so `(segment-for-each (segment (vector 1 2) 0 5) println)` printed
three of the library's internal objects after `1` and `2`.

## The changes

### Compile safe; skip checks only where nothing can defeat them (`bcd56bd`)

No module is compiled in unsafe mode any more.  Two kinds of operation are
still unchecked, because no value of the wrong kind can reach them, whatever
threads do:

* The accessors and mutators of the library's own structures, which the new
  `struct/unchecked` form (in `unchecked.rkt`) defines with
  `unsafe-struct*-ref` and `unsafe-struct*-set!`.  A field only ever holds a
  value of its declared kind, since every write stores a whole value; a race
  can make the value wrong, but not of the wrong kind.  Values that come from
  callers are checked by the entry points before any accessor sees them.

* Fixnum arithmetic.  Its operands are fields that hold fixnums or indices the
  entry points have checked.  A result that overflowed would wrap, which is a
  wrong answer, not a memory error.

The remaining unchecked operations are vector accesses, and each is guarded:

* Every unchecked access to a chunk's storage goes through `checked-index`,
  two comparisons against the capacity.  While the invariants hold, the index
  is in range anyway; when a race has broken them, the check raises
  `exn:fail:sek` instead of reaching memory.
* An iterator checks a run of storage once, when it installs it, and its inner
  loops then read the run unchecked.
* The value written into an empty slot is now an empty chunk, not a symbol.
  In a support at depth greater than zero every slot then holds a chunk,
  occupied or not, so a stray read of an empty slot -- which only a race can
  cause -- still gives the chunk accessors a chunk.

The two validators, `sek-validate-pseq` and `sek-validate-eseq`, used to rely
on safe mode to reject a non-sequence; they now check their argument.

### Claim a slot with a compare-and-swap (`ae37c96`)

A support's head and size now share one fixnum field, the *extent*, head in
the high half and size in the low half.  A monotonic push reads the extent
once, tests its condition against that value, and claims the slot with a
compare-and-swap that advances the extent; it writes the slot only after the
swap succeeds.  A push that loses the race sees the swap fail and copies the
chunk instead, exactly as if the slot had been taken earlier.  Each slot is
therefore written at most once, by the one push that claimed it, and no view
ever sees a slot change.  `struct/unchecked` defines the compare-and-swap
along with the other field operations.

Packing the two halves limits a capacity to 2^30 - 1 (2^14 - 1 where fixnums
are 30 bits wide), which `sek-configure!` now checks.

### Check the run a segment is built from (`de633d2`)

`segment` now requires the run to fit in its vector, and the vector not to be
a chaperone or impersonator, which an unchecked read cannot handle.

### Make reads read (`acc3780`)

Three reads no longer write anything a read depends on:

* **Iterators.**  A cursor now walks up to five regions at the top level --
  front, inner front, middle, inner back, back -- instead of three, so it reads
  the inner chunks where they are.
* **Iterator validity.**  Every update now advances the version, and an
  iterator is valid while the version is the one it was born with, so creating
  an iterator only reads the version.  Before, creating one marked the
  version as having a live iterator.
* **Snapshots.**  `eseq-snapshot` folds the inner chunks into a persistent
  copy of the middle sequence rather than into the sequence's own.  It writes
  one field of the ephemeral sequence, its ownership id, which only updates
  consult.  `eseq-copy` in `'share` mode likewise writes only ids.

### Let the next update adopt the fold (`5ae61e2`)

Folding into a copy has a cost the in-place fold did not: the inner chunks
stay full, so a sequence that is snapshotted after every few updates folds
them again at every snapshot.  Keeping a snapshot after every push cost about
three times as much as before (905 ns a push against 302), and one change
plus a snapshot about twice as much (173 ns against 71).  A round trip from a
persistent sequence -- `pseq-edit`, some updates, `eseq-snapshot` -- was not
affected, since `pseq-edit` starts with empty inner chunks.

A snapshot now leaves its fold in a field and makes the version negative.  The
next update installs the fold and empties the inner chunks; a snapshot taken
before then reuses it.  An update has exclusive access, so it may rewrite
several fields at once, which a snapshot, running beside readers, may not.
Every update already runs `eseq-invalidate-iterators!` before it looks at the
structure, and already loads the version there, so an update with nothing to
adopt pays one sign test.  Iterator validity uses the version's magnitude, so a
snapshot does not invalidate live iterators.

### Win back what safe compilation cost the hot paths (`b4d0b02`)

Measuring the commits one at a time showed two costs worth removing.
Traversal had become about 20% slower: the element loops are `for` loops over
`in-range`, and `in-range` uses unchecked fixnum arithmetic only in a module
compiled in unsafe mode.  A new clause, `in-fx-range`, always does, and the
loops that use it read their elements through checked indices or from runs
checked once.  And `eseq-invalidate-iterators!`, which every push and pop
runs, had outgrown the compiler's inlining threshold when it gained the branch
to `adopt-fold!`; encouraging the inliner took about a nanosecond off each
push and pop.

## Why we believe it

### The arguments

**Memory safety** needs every unchecked operation's precondition to hold
whatever the threads do.  The preconditions are of three kinds, and each has
its own guard:

* *The argument of an unchecked accessor is a structure of its type.*  Inside
  the library, a value reaches an accessor from a field of known type, from a
  chunk's storage (which at depth greater than zero holds only chunks, the
  empty-slot marker included), or from a caller.  A caller's value is checked
  by the entry point: `argument-check-tests.rkt` calls every export except
  predicates, constructors and aliases with bad first arguments and requires
  the error to name the export, and `error-tests.rkt` covers the other
  arguments case by case.
* *An unchecked vector index is in range.*  Every such index goes through
  `checked-index`, lies in a run the iterator checked when it installed it, lies
  in a run `segment` checked when it built it, or indexes a vector the same
  function just allocated.  A chunk's storage never changes length, so a run
  checked once stays in range whatever happens to the sequence afterwards.
* *An unchecked fixnum operation's operands are fixnums.*  They come from
  fields that are only ever assigned fixnums, or from indices the entry points
  checked with `fixnum?`.

**Sharing persistent sequences** needs every write that a persistent operation
makes to shared storage to be safe under concurrency.  Reading
`persistent.rkt`, `ptree.rkt` and `chunk.rkt` for writes finds only these:
writes to vectors and builders the operation has just allocated, writes to
chunks owned by an ephemeral sequence (ownership means no other structure
shares them), and the monotonic push, which now claims its slot atomically.

**Concurrent readers of an ephemeral sequence** need the reads to write
nothing that another read consults.  The writes left in read paths are the
ownership id, which only updates consult; the fold field, which only updates
and snapshots consult; and the version's sign, which readers mask off.  Two
snapshots that race may each build a fold, and either is correct, since
nothing changed between them.  A snapshot that sees the version already
negative but the fold not yet stored -- as one racing another may -- folds for
itself rather than using a stale value.

### The evidence

All of it was run on Racket 9.3.0.8 CS, x86-64, on a 36-core Xeon.

* **The test suite**: all 5,924 tests pass (a 5,925th, which checks that
  every export is documented, needs the documentation installed), including
  new ones for each change:
  argument checks now covering the validators, `segment`'s range checks,
  iteration and snapshots over full inner chunks, adoption of a fold, and
  `thread-tests.rkt`, which runs parallel threads in lockstep.  Against the
  code before `ae37c96`, two of its persistent-push cases fail; against the
  code before `acc3780`, its two-readers case fails.
* **Conformance with the OCaml library**: all 156 seeds match.  Each seed
  generates a script of 700 commands, run by both implementations, and the
  traces -- every result and the full contents of every sequence after each
  command -- must match exactly.  The scripts use every operation on both
  flavors, iterators and snapshots included, and the seeds were spread over
  five configurations:

  | Configuration | Seeds |
  |---|---|
  | Default capacities (128, 16) and threshold (32) | 60 |
  | Capacities 3 and 2, threshold 2: deep trees | 24 |
  | Capacities 2 and 2, threshold 2: the smallest the OCaml library accepts | 24 |
  | Iterator validity checking off | 24 |
  | Empty slots not overwritten, capacities 3 and 2 | 24 |

  Deep trees are where the five-region cursor and the fold are exercised most;
  the last two rows vary the two settings that the version scheme and the
  empty-slot marker depend on.
* **Fuzzing with threads**: 400 trials per mode, each on a sequence of random
  size up to 30,000, for the original code and for the code at `b4d0b02`,
  before the stress testing described next.  In every mode
  two parallel threads start together; afterwards one thread checks what they
  returned, validates the structure, keeps using the sequences, and checks that
  snapshots taken earlier have not changed.

  | Mode | Original code | `b4d0b02` |
  |---|---|---|
  | Two threads push onto the same persistent sequences, then keep using their results | 20 results with the other thread's elements; 2 malformed chunks | Nothing |
  | Two threads read one ephemeral sequence -- iterate, fold, snapshot, `for` -- then one thread updates it | 486 readings with wrong contents; 120 crashes; 70 trials in which the sequence changed, and 70 in which an earlier snapshot did | Nothing |
  | One thread updates an ephemeral sequence while another reads it *(unsupported)* | 388 crashes | No crashes; the reader's iterator is invalidated, as specified, and in 22 trials the reader saw an internal object |
  | Two threads update one ephemeral sequence *(unsupported)* | 3 of 10 processes aborted; 12 more crashes | No crashes; 21 `exn:fail:sek`, and wrong contents and broken invariants as before |

### Testing the argument directly

Fuzzing tells us about memory safety only when a race happens to corrupt
memory visibly, and only when the race happens at all; both are rare.  Two
builds of the library, chosen by environment variables when it is compiled
(see `sek/unchecked`), remove both limits:

* **The checked build** (`SEK_CHECKED=1`).  `unchecked.rkt` is the only
  module that uses `racket/unsafe/ops`, and in this build every unchecked
  operation it provides -- structure accessors and mutators, fixnum
  operations, vector operations, loop bounds -- first checks the precondition
  that makes it safe, and raises an error naming the operation if it does not
  hold.  A race that would corrupt memory in a normal build is then reported,
  every time, at the operation it reached.
* **The jitter build** (`SEK_JITTER=1`).  Random pauses at the points where
  concurrent operations interleave -- between the steps of an in-place push or
  pop, between reading a support's extent and claiming a slot, between
  building a snapshot's fold and publishing it, during adoption, between the
  reads that start an iterator -- make races far more frequent.

`stress/race-fuzz.rkt` runs N parallel threads that start together on shared
sequences.  Three modes exercise what the documentation promises -- persistent
sequences used by many threads, an ephemeral sequence read by many threads by
every route (iterators that jump, segments, folds, snapshots, copies), and
snapshots handed over channels from a thread that goes on updating to threads
that check, extend and edit them -- and must find nothing.  Two exercise what
it does not promise -- one writer with readers, and several writers -- where
only memory safety and termination are at stake.  A watchdog interrupts any
thread still running after a minute and reports where it was.

**It found two bugs**, both in unsupported uses, which the earlier fuzzing had
missed:

* *An out-of-bounds read* (`1c55da4`).  An iterator that steps into a middle
  sequence trusts that it lands on an item; a concurrent writer could leave an
  empty chunk there, and the cursor then read its current element from the
  empty vector a sentinel holds.  The checked build reported it twice in 540
  runs, at capacity 2; in the jitter build without checks it crashed the
  process ("invalid memory reference") 30 times, where the build without
  jitter never did.
* *A pop that never returned* (`8b556ee`).  Two threads popping one chunk
  could leave its size at -1, which the emptiness test, looking for exactly 0,
  never recognized.  Five runs of the first campaign hung, and the watchdog
  located the hung threads in `eseq-pop-front!`.

**After the fixes**, the campaign ran every combination of the four builds
(normal, checked, jitter, both), the five modes, 2, 4 and 8 threads, and
three capacity settings (the defaults; 4 and 3; 2 and 2), ten seeds each:
1,800 runs and 54,000 trials, on the 36-core machine with 24 runs at a time.

| Mode | Trials | What they found |
|---|---|---|
| Persistent sequences shared by N threads | 10,800 | Nothing |
| N readers of one ephemeral sequence | 10,800 | Nothing |
| Snapshots handed to N - 1 threads while one updates | 10,800 | Nothing |
| One writer and N - 1 readers *(unsupported)* | 10,800 | No precondition violations, crashes or hangs; wrong results and `exn:fail:sek`, as documented |
| N writers *(unsupported)* | 10,800 | No precondition violations, crashes or hangs; wrong results and `exn:fail:sek`, as documented |

The checked build also passes the whole test suite and 30 conformance seeds
with no precondition violated: correct single-threaded use never reaches an
unchecked operation with a bad argument.

### What the evidence does not cover

* **Other hardware.**  All of it ran on x86-64, which orders stores strictly.
  Two protocols depend on the order of stores: a persistent push writes its
  slot after claiming it, and a snapshot stores its fold before it makes the
  version negative.  Neither has been run on hardware with weaker ordering,
  such as ARM.
* **Termination under unsupported races** is tested, not argued.  The
  watchdog found one loop that a race could make endless, now fixed, and has
  found none since.
* **Other versions of Racket.**  The evidence comes from 9.3.0.8; CI also
  tests 9.2, the oldest version the library supports, and the current
  snapshot, but has not yet run these commits.
* **30-bit fixnums.**  The 2^14 capacity limit on such platforms is derived,
  not tested.
* **Writable segments** hand the caller the sequence's live storage, so a
  write through one is an update like any other and needs exclusive access.

### Further testing

These would add to the evidence, roughly in order of value for the effort:

* **Run the campaign on weakly ordered hardware**, an ARM machine with a
  parallel-thread Racket.  It is the one gap above that testing can close.
* **Mutation-test the guards.**  Remove each guard in turn -- `checked-index`,
  the iterator's run check, the sentinel test in `cur-get`, the
  compare-and-swap, the chunk used as the empty-slot marker, the tests in the
  pops -- and confirm that the checked build and the fuzzer report each
  removal.  That measures how sensitive the fuzzer is, rather than assuming it.
* **Model-check the two lock-free protocols** -- claiming a slot through the
  extent, and publishing and adopting a fold -- over every interleaving of
  their steps for two and three threads, in a small model of just those
  fields.
* **Run longer.**  The campaign above took about five minutes of the
  machine; hours cost nothing but time.

## What it costs

Measured at `b4d0b02` against the original code (`2662e1b`) on the same
machine: the benchmark scenarios (`bench/main.rkt`, 81 cells of the two sek
flavors, best of three rounds) and the whole-program workloads
(`bench/workloads.rkt`, best of seven).  The commits after `b4d0b02` cost
nothing measurable: over traversal, stack, queue, hops and random access, the
head runs at 1.00 times `b4d0b02`.

| | `b4d0b02` ÷ original |
|---|---|
| All scenario cells, geometric mean | 1.025 |
| Persistent sequences only | 1.004 |
| Ephemeral sequences only | 1.043 |
| Workloads, geometric mean | 1.067 |

By scenario, construction (0.92), traversal (0.96), queue (0.98) and random
access (1.00) cost the same or less; stack (1.05), splitting (1.09), pushing
and popping at the front (1.11) and concatenating ephemeral sequences (1.14)
cost more.  The worst single cell is concatenating two ephemeral sequences of
5,000 elements, at 1.28.  Snapshots cost what they did: one change plus a
snapshot 69 ns against 71, and keeping a snapshot after every push 290 ns a
push against 306.

Of the workloads, the persistent ones cost at most 9% more, and the ephemeral
editor, queue and scan at most 11%.  The ephemeral `log` and `undo` workloads, which take
snapshots between batches of updates, cost 15--25% more.  Measuring the
commits one at a time puts most of that on compiling safe and a few points on
the compare-and-swap, which every push into a chunk that a snapshot shares
now pays.

### When to use a transient

A transient -- `pseq-edit`, m in-place updates, `eseq-snapshot` -- beats m
persistent updates from these batch sizes on, in the original code and the
final code alike:

| Updates | n = 100 | n = 10,000 | n = 1,000,000 |
|---|---|---|---|
| `set` at random indices | 2 | 4 | 8 |
| Pushes at the back | 4 | 4 | 4 |
| Push at the back and pop at the front | 4 | 4 | 4 |

Past the crossover, per update:

| ns per update, large batches | Original | Final |
|---|---|---|
| Persistent `set`, n = 10,000 | 230 | 202 |
| Transient `set`, n = 10,000 | 55 | 57 |
| Persistent push | 18.4 | 22.5 |
| Transient push | 7.1 | 8.4 |
| Persistent push and pop | 46 | 47 |
| Transient push and pop | 22.0 | 23.7 |

A persistent push pays about 4 ns for the compare-and-swap that makes sharing
safe.

## A separate bug, in Racket's treelists

An earlier run of the fuzzer, which compared sek's sequences under races with
mutable treelists, turned up a memory-safety bug in `racket/treelist` itself,
with no threads involved: alternating
`treelist-cons` and `treelist-add` corrupts memory after 994 operations, in
Racket 9.2 and in current snapshots.  Three places in `treelist.rkt` built or
adjusted a node's sizes as though a relaxed subtree were dense.  The fix is
commit `3fe4ea798f` on the `treelist-relaxed-index` branch of
`samth/racket`, with regression tests; a randomized differential test with the
size invariants checked after every operation passes 24,000 seeds of 2,000
operations.
