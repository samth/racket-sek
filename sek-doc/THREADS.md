# Threads and memory safety

What threads may do with a sek sequence, how the implementation keeps that
promise, and how the promise is tested.

## The contract

The documentation's "Threads" section states it precisely.  In short:

| Use | Status |
|---|---|
| A persistent sequence used by any number of threads, pushes included | Safe, with no synchronization |
| An ephemeral sequence read by several threads at once: iterating, folding, snapshotting, copying | Safe, with no synchronization |
| A snapshot used by one thread while another goes on updating the sequence it came from | Safe, with no synchronization |
| An ephemeral sequence updated by one thread while others use it | Unsupported: an update needs exclusive access |
| One iterator used by two threads at once | Unsupported: an iterator belongs to one thread at a time |

An update is an `esek-...!` operation, a write through an iterator, or
`sek-append*` of an ephemeral sequence.  A program that makes an unsupported
use gets wrong results or an `exn:fail:sek`, and the sequence may be left
inconsistent, but no use of the library, supported or not, corrupts memory.

## How the implementation keeps it

### Unchecked operations, each with a guard

The library is compiled in safe mode.  `private/unchecked.rkt` is the only
module that uses `racket/unsafe/ops`, and it provides three kinds of unchecked
operation, each safe for its own reason whatever threads do:

* **Accessors and mutators of the library's own structures**, which
  `struct/unchecked` defines.  A field only ever holds a value of its declared
  kind, since every write stores a whole value; a race can make the value
  wrong, but not of the wrong kind.  Values that come from callers are checked
  by the entry points before any accessor sees them.  Where a field holds
  either a structure or `#f` -- an ephemeral sequence's middle sequence and
  fold, and several fields of an iterator's cursor -- the code reads the field
  once and tests the value it read, because a second read could see another
  thread's write.
* **Fixnum arithmetic.**  Its operands come from fields that are only ever
  assigned fixnums, from indices the entry points checked with `fixnum?`, or
  from the table of packed item weights, which `config.rkt` fills only where
  a full chunk's weight is a fixnum.  The concatenations refuse a result
  longer than the largest power of two that is a fixnum, so no length or
  weight leaves fixnum range.
* **Vector accesses.**  Every access to a chunk's storage goes through
  `checked-index`, two comparisons against the capacity.  A traversal checks a
  run of storage once, when the iterator installs it or `segment` builds it,
  and then reads the run unchecked; a chunk's storage never changes length, so
  a run checked once stays in range whatever happens to the sequence
  afterwards.  An iterator tests its current index against the vector it
  actually reads.

The value written into an emptied slot is an empty chunk.  In storage at depth
greater than zero every slot then holds a chunk, occupied or not, so a stray
read of an empty slot, which only a race can cause, still gives the chunk
accessors a chunk.

### A persistent push claims its slot

The paper's monotonic update extends a chunk in place when the chunk's view
ends where its support's occupied region ends: the new slot lies outside every
view, so no sequence can see the write.  Sequences that share a support may
be extended by several threads at once, so the push has to test that condition
and take the slot as one step.

A support's head and size share one fixnum field, the *extent*.  A monotonic
push reads the extent once, tests its condition against that value, and claims
the slot with a compare-and-swap that advances the extent; it writes the slot
only after the swap succeeds.  A push that loses the race copies the chunk
instead.  Each slot is therefore written at most once, by the one push that
claimed it, and no view ever sees a slot change.

Packing the two halves limits a capacity to 2^30 - 1, or 2^14 - 1 where
fixnums are 30 bits wide.

### Reads write nothing a read depends on

* **Iterators** walk an ephemeral sequence's five parts -- front, inner front,
  middle, inner back, back -- where they are, rather than first folding the
  inner chunks into the middle sequence.
* **Iterator validity** is a version number.  Every update advances it, and an
  iterator is valid while the version is the one it was born with, so creating
  an iterator only reads the version.
* **Snapshots** fold the inner chunks into a persistent copy of the middle
  sequence, not into the sequence's own.  A snapshot writes the ephemeral
  sequence's ownership id, which only updates consult, and leaves its fold in
  a field with the version's sign as a flag.  The next update, which has
  exclusive access, adopts the fold; a snapshot taken before then reuses it.
  Readers mask the sign off.  Two snapshots that race may each build a fold,
  and either is correct; one that sees the flag before the other's fold is
  stored folds for itself.

### Traversals check before each element

A traversal that hands elements to the caller's code -- a `for` loop, a fold,
`sek-map` -- reads them from a run of storage, and the caller's code may update
the sequence.  Before each element it hands over, the traversal checks that
the version has not changed, and raises if it has.  Without the check the
caller would see the update in the rest of the run, and after a pop would be
handed the empty-slot marker.

## How it is tested

* **The test suite** includes `thread-tests.rkt`, which runs parallel threads
  in lockstep on shared persistent sequences and on one ephemeral sequence
  read by several threads, and `argument-check-tests.rkt` and
  `error-tests.rkt`, which call every export with wrong arguments and require
  an error that names the export.
* **The checked build** (`SEK_CHECKED=1` when the library is compiled).  Every
  unchecked operation first checks the precondition that makes it safe, and
  raises an error naming the operation if it does not hold.  A race that would
  corrupt memory in a normal build is then reported at the operation it
  reached.  The whole test suite passes on this build with no precondition
  violated.
* **The jitter build** (`SEK_JITTER=1`).  Random pauses at the points where
  concurrent operations interleave make races far more frequent.
* **`stress/race-fuzz.rkt`** runs N parallel threads that start together on
  shared sequences.  Three modes exercise the supported uses and must find
  nothing.  Three exercise unsupported uses -- one writer with readers, several
  writers, several threads on one iterator -- where only memory safety and
  termination are at stake.  A watchdog reports any thread still running after
  a minute.
* **`aufheben/model.rkt`** checks operations on both flavors and on iterators
  against a list model that predicts exactly which iterators are valid.
  **`aufheben/torn.rkt`** stores a wrong value of the right kind in one field
  between operations -- the states races reach rarely -- and requires, on the
  checked build, that no operation then violate a precondition or fail to
  finish.  Both need [aufheben](https://github.com/samth/aufheben), which
  sek-test does not depend on, so they run by hand.

The fuzzer has run for millions of trials across the four builds, the six
modes, 2 to 8 threads and three capacity settings on x86-64, and for several
hundred thousand on a two-core ARM machine, where stores are not ordered as
they are on x86-64; the supported modes found nothing, and the unsupported
ones no precondition violation, crash or hang.  Mutation testing, in which
each of fifteen guards was removed or weakened in turn, found a test that
catches each.

## What is not covered

* **Weak memory ordering beyond two cores.**  The compare-and-swap needs no
  further ordering, since another thread reaches a new slot only through a
  sequence handed over with synchronization.  The fold's fallback covers the
  one reordering the protocol admits, and a unit test plants that state.
* **Termination under unsupported races** is tested, not argued.
* **30-bit fixnums.**  The 2^14 capacity limit on such platforms is derived,
  not tested.
