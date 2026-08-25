#!/bin/bash
# Differential testing against the reference implementation.
#
#   ./build.sh                 # build the OCaml driver (once)
#   ./run.sh [seed-from] [seed-to] [steps]
#
# Each seed produces one random script; the script is executed by the OCaml
# reference and by this library, and the two traces must be identical.  A
# trace records the result of every command *and* the full contents of every
# slot afterwards, so a divergence is reported at the command that causes it.
#
# The chunk capacities are taken from the environment by both drivers, so the
# same script can be replayed at several tree shapes:
#   SEK_LEAF, SEK_NODE, SEK_THRESHOLD, SEK_OVERWRITE, SEK_CHECKITER

set -u
here="$(cd "$(dirname "$0")" && pwd)"
cd "$here"

from=${1:-1}
to=${2:-8}
steps=${3:-700}

if [ ! -x ./driver ]; then
  echo "the OCaml driver is missing; run ./build.sh first" >&2
  exit 2
fi

status=0
for seed in $(seq "$from" "$to"); do
  racket -y gen.rkt "$seed" "$steps" > "/tmp/sek-script-$seed.txt"
  ./driver < "/tmp/sek-script-$seed.txt" > "/tmp/sek-ocaml-$seed.txt" 2>&1
  racket -y driver.rkt < "/tmp/sek-script-$seed.txt" > "/tmp/sek-racket-$seed.txt" 2>&1
  if diff -q "/tmp/sek-ocaml-$seed.txt" "/tmp/sek-racket-$seed.txt" > /dev/null; then
    echo "seed $seed: match ($(wc -l < "/tmp/sek-script-$seed.txt") commands)"
    rm -f "/tmp/sek-script-$seed.txt" "/tmp/sek-ocaml-$seed.txt" "/tmp/sek-racket-$seed.txt"
  else
    echo "seed $seed: DIVERGES -- see /tmp/sek-{script,ocaml,racket}-$seed.txt"
    diff "/tmp/sek-ocaml-$seed.txt" "/tmp/sek-racket-$seed.txt" | head -20
    status=1
  fi
done
exit $status
