# Validating a change

Notes for an agent working on this repository: what to run before calling a
change done, and how to run it without installing the packages.

## Layout

Four packages: `sek-lib` (the library), `sek-doc`, `sek-test`, and `sek`,
which depends on the first two.  The interface is `sek-lib/sek/main.rkt`;
everything else is in `sek-lib/sek/private/`.  The package catalog tracks
`main`, so a push to `main` is a release.

## The test suite

Without installing anything, point `PLTCOLLECTS` at the checkout (the trailing
colon keeps the default collections):

```sh
export PLTCOLLECTS="$PWD/sek-lib:$PWD/sek-test:"
raco make sek-lib/sek/main.rkt
raco test -y sek-test/tests/sek/*.rkt
```

`test-docs-complete.rkt` fails unless `sek-doc` is installed with its
documentation built; every other file passes from a bare checkout.  The whole
suite takes several gigabytes, so on a small machine run one file at a time.

## The checked build

`SEK_CHECKED=1`, set when the library is compiled, makes every unchecked
operation check its precondition first (see `sek-lib/sek/private/unchecked.rkt`).
`raco make` does not notice a change of environment, so compile this build
from a copy of `sek-lib/sek` with no `compiled` directories:

```sh
rsync -a --delete --exclude compiled sek-lib/sek/ /tmp/sek-checked/sek/
export PLTCOLLECTS="/tmp/sek-checked:$PWD/sek-test:"
SEK_CHECKED=1 raco make /tmp/sek-checked/sek/main.rkt
SEK_CHECKED=1 raco test -y sek-test/tests/sek/*.rkt > checked.log 2>&1
grep -c 'precondition violated' checked.log    # must be 0
```

Run it after any change to a module that requires `unchecked.rkt`.

## What CI runs

`.github/workflows/ci.yml` installs the packages and runs the tests on Racket
9.2, 9.3 and the current snapshot, then checks the declared dependencies.  To
do the same locally, use a fresh `PLTADDONDIR` so the installation does not
touch the real one:

```sh
export PLTADDONDIR=/tmp/sek-addon; rm -rf $PLTADDONDIR
raco pkg install --auto --batch sek-lib/ sek-doc/ sek-test/ sek/
raco test -e -j 2 sek-test/
raco setup --check-pkg-deps --pkgs sek-lib sek-doc sek-test sek
```

`sek-test/tests/info.rkt` keeps the benchmarks, the conformance harness and
the race fuzzer out of `raco test`.  They are still compiled, so a dependency
one of them adds must be declared in `sek-test/info.rkt`.

## Conformance with the OCaml library

After a change to any operation's behavior, in
`sek-test/tests/sek/conformance/`:

```sh
./build.sh              # once; needs ocamlfind
./run-all.sh 1 8 700    # ten configurations, eight seeds each
```

Every line must say `match`.

## Threads

After a change to chunks, snapshots, iterators or traversals, read
`THREADS.md` and run the fuzzer against the checked build:

```sh
racket sek-test/tests/sek/stress/race-fuzz.rkt MODE --threads 4 --trials 20000
```

The supported modes (`psek`, `readers`, `handoff`) must print nothing but the
trial count.  The unsupported ones (`writer`, `writers`, `iterator`) must
print no line that mentions `precondition violated`.

`sek-test/tests/sek/aufheben/model.rkt` and `torn.rkt` need the
[aufheben](https://github.com/samth/aufheben) package and run by hand:

```sh
racket model.rkt --leaf 2 --node 2 --threshold 2 --test-cases 200
racket torn.rkt --leaf 2 --node 2 --threshold 2 --test-cases 200   # checked build
```

## Benchmarks

`sek-test/tests/sek/bench/README.md` says how to run them.  Pin the process to
an idle core, and compare a change against a run of the parent commit taken
the same way on the same machine; numbers from different machines or sessions
do not compare.  Results files are ignored by git; when the numbers quoted in
`README.md` or `bench/README.md` change materially, update them from one
complete run.

## Documentation

`sek-doc/sek/scribblings/sek.scrbl` states the cost of every operation.  A
change to how an operation works usually changes its stated cost, and
`test-docs-complete.rkt` requires every export to be documented.
