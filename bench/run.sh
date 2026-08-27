#!/bin/bash
# Runs the benchmark suite, one scenario per process.
#
#   ./run.sh                  the Racket implementation
#   ./run.sh --ocaml          the OCaml reference (needs ./build-ocaml.sh first)
#   ./run.sh --quick          smaller sizes
#
# A fresh process per scenario keeps one scenario's garbage from being charged
# to the next, and keeps the heap from growing across a long run.

set -u
here="$(cd "$(dirname "$0")" && pwd)"
cd "$here"

ocaml=no
args=()
for a in "$@"; do
  case "$a" in
    --ocaml) ocaml=yes ;;
    *) args+=("$a") ;;
  esac
done

racket_scenarios="stack front-stack queue traversal random-access hops update
                  construction concat split snapshot transient filter fill capacities"
ocaml_scenarios="stack front-stack queue traversal random-access hops update
                 construction concat split snapshot transient filter fill"

if [ "$ocaml" = yes ]; then
  if [ ! -x ./bench-ocaml ]; then
    echo "./bench-ocaml is missing; run ./build-ocaml.sh first" >&2
    exit 2
  fi
  ./bench-ocaml --version-banner-only 2>/dev/null | head -0
  echo "sek benchmarks -- OCaml reference"
  for s in $ocaml_scenarios; do
    ./bench-ocaml ${args[@]+"${args[@]}"} "$s" | tail -n +2
  done
else
  echo "sek benchmarks -- Racket $(racket -e '(display (version))')"
  for s in $racket_scenarios; do
    racket -y main.rkt ${args[@]+"${args[@]}"} "$s" | tail -n +2
  done
fi
