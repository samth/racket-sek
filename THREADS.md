# Threads and memory safety

This note explains six commits that change what sek promises when threads
share its sequences, and why we believe the new promises hold.

| Commit | Change |
|---|---|
| `bcd56bd` | Compile safe, and skip checks only where nothing can defeat them |
| `ae37c96` | Let persistent sequences be shared between threads |
| `de633d2` | Check the run a segment is built from |
| `acc3780` | Let several threads read an ephemeral sequence at once |
| `5ae61e2` | Let the next update adopt a snapshot's fold of the inner chunks |
| `b4d0b02` | Win back what safe compilation cost the element loops and the push path |

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
  size up to 30,000, for the original code and the final code.  In every mode
  two parallel threads start together; afterwards one thread checks what they
  returned, validates the structure, keeps using the sequences, and checks that
  snapshots taken earlier have not changed.

  | Mode | Original code | Final code |
  |---|---|---|
  | Two threads push onto the same persistent sequences, then keep using their results | 20 results with the other thread's elements; 2 malformed chunks | Nothing |
  | Two threads read one ephemeral sequence -- iterate, fold, snapshot, `for` -- then one thread updates it | 486 readings with wrong contents; 120 crashes; 70 trials in which the sequence changed, and 70 in which an earlier snapshot did | Nothing |
  | One thread updates an ephemeral sequence while another reads it *(unsupported)* | 388 crashes | No crashes; the reader's iterator is invalidated, as specified, and in 22 trials the reader saw an internal object |
  | Two threads update one ephemeral sequence *(unsupported)* | 3 of 10 processes aborted; 12 more crashes | No crashes; 21 `exn:fail:sek`, and wrong contents and broken invariants as before |

### What the evidence does not cover

* **Other hardware.**  We fuzzed only on x86-64, which orders stores strictly.
  On hardware with weaker ordering, a program that passes a sequence to
  another thread without synchronizing -- through a plain mutable variable,
  say -- relies on Racket's guarantees for publishing an object, as it would
  with any Racket data structure.
* **Other variants of Racket.**  `thread-tests.rkt` runs only where `thread`
  accepts `#:pool`, which excludes Racket BC; BC's threads are not parallel.
  The library compiles on Racket 8.17 BC, the oldest version CI tests, and
  passes a smoke test of the new paths there, but CI has not yet run these
  commits.
* **30-bit fixnums.**  The 2^14 capacity limit on such platforms is derived,
  not tested.
* **Writable segments** hand the caller the sequence's live storage, so a
  write through one is an update like any other and needs exclusive access.

## What it costs

Measured against the original code (`2662e1b`) on the same machine: the
benchmark scenarios (`bench/main.rkt`, 81 cells of the two sek flavors, best
of three rounds) and the whole-program workloads (`bench/workloads.rkt`, best
of seven).

| | Final ÷ original |
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
