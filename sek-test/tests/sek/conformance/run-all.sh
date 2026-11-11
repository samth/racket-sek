#!/bin/bash
# Run the conformance check at every configuration it has been checked at:
# eight tree shapes, plus the default shape with each of the two boolean
# settings turned off.  Arguments are passed on to run.sh (seeds and steps).
#
#   ./build.sh && ./run-all.sh
set -u
here="$(cd "$(dirname "$0")" && pwd)"
status=0
# leaf node threshold overwrite check-iterators
while read -r leaf node threshold overwrite checkiter; do
  echo "== leaf $leaf, node $node, threshold $threshold, overwrite $overwrite, check iterators $checkiter"
  SEK_LEAF=$leaf SEK_NODE=$node SEK_THRESHOLD=$threshold \
    SEK_OVERWRITE=$overwrite SEK_CHECKITER=$checkiter "$here/run.sh" "$@" || status=1
done <<'CONFIGS'
128 16 32 1 1
2 2 2 1 1
3 2 2 1 1
4 4 4 1 1
5 3 5 1 1
8 4 6 1 1
16 8 12 1 1
32 4 8 1 1
128 16 32 0 1
128 16 32 1 0
CONFIGS
exit $status
