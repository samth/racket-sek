#!/bin/bash
# Runs the benchmark suite, one scenario per process.
#
#   ./run.sh                  the Racket implementation
#   ./run.sh --external       the benchmarks borrowed from other libraries
#   ./run.sh --workloads      whole programs rather than single operations
#   ./run.sh --all            all three
#   ./run.sh --ocaml          the OCaml reference (needs ./build-ocaml.sh first)
#   ./run.sh --quick          smaller sizes
#   ./run.sh --json FILE      also record every table in FILE, one per line
#
# Set RACKET to choose the executable; it defaults to whatever `racket` is on
# PATH.  Worth having because a shell rc can put a different Racket in front of
# the one you meant -- PATH set on the command line does not survive into this
# script if the rc rewrites it.
#
# A fresh process per scenario keeps one scenario's garbage from being charged
# to the next, and keeps the heap from growing across a long run.

set -u
RACKET="${RACKET:-racket}"
here="$(cd "$(dirname "$0")" && pwd)"
cd "$here"

ocaml=no
which=main
json=
args=()
while [ $# -gt 0 ]; do
  case "$1" in
    --ocaml) ocaml=yes ;;
    --external) which=external ;;
    --workloads) which=workloads ;;
    --all) which=all ;;
    --json) json=$2; shift ;;
    *) args+=("$1") ;;
  esac
  shift
done

main_scenarios="stack front-stack queue burst-check sync-cost traversal random-access hops
                update construction concat split snapshot transient filter fill
                capacities"
external_scenarios="apply-sequential update-sequential apprepend ends slice
                    bulk-append map filter-ratio take-drop push-move split-parts"
ocaml_scenarios="stack front-stack queue traversal random-access hops update
                 construction concat split snapshot transient filter fill"

# A fresh JSON file, since each process appends to it.
[ -n "$json" ] && : > "$json"

run_one() { # <file> <scenario>
  "$RACKET" -y "$1" ${args[@]+"${args[@]}"} ${json:+--json "$json"} "$2" | tail -n +2
}

if [ "$ocaml" = yes ]; then
  if [ ! -x ./bench-ocaml ]; then
    echo "./bench-ocaml is missing; run ./build-ocaml.sh first" >&2
    exit 2
  fi
  echo "sek benchmarks -- OCaml reference"
  for s in $ocaml_scenarios; do
    ./bench-ocaml ${args[@]+"${args[@]}"} "$s" | tail -n +2
  done
  exit 0
fi

echo "sek benchmarks -- Racket $("$RACKET" -e '(display (version))')"
if [ "$which" = main ] || [ "$which" = all ]; then
  for s in $main_scenarios; do run_one main.rkt "$s"; done
fi
if [ "$which" = external ] || [ "$which" = all ]; then
  for s in $external_scenarios; do run_one external.rkt "$s"; done
fi
if [ "$which" = workloads ] || [ "$which" = all ]; then
  # one process for the lot: a workload is already a whole program, and the
  # sizes it runs at are its own
  "$RACKET" -y workloads.rkt ${args[@]+"${args[@]}"} | tail -n +2
fi
