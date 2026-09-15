#!/bin/bash
# Builds the OCaml benchmark against the reference implementation.
#
#   ./build-ocaml.sh [path-to-sek-checkout]
#
# The reference is compiled here in its *release* configuration: its cppo
# `dev` blocks are dropped and assertions are disabled.  That matters a great
# deal -- with assertions on, every operation runs the library's O(n) internal
# validator, which makes it look about thirty times slower than it is.  (The
# conformance harness deliberately builds the other way round.)
#
# Produces ./bench-ocaml, which takes the same scenario names as main.rkt.

set -eu
here="$(cd "$(dirname "$0")" && pwd)"
cd "$here"

src=${1:-}
if [ -z "$src" ]; then
  for candidate in ../conformance/sek-ocaml ./sek-ocaml; do
    if [ -d "$candidate" ]; then src=$candidate; break; fi
  done
fi
if [ -z "$src" ]; then
  git clone --depth 1 https://gitlab.inria.fr/fpottier/sek.git sek-ocaml
  src=./sek-ocaml
fi

rm -rf build
mkdir build
cp "$src"/src/*.ml "$src"/src/*.mli build/
cp ../conformance/PPrint.ml bench.ml build/
cd build

python3 - <<'EOF'
src = open('ShareableChunk.cppo.ml').read()
out, keep = [], [True]
for line in src.split('\n'):
    st = line.strip()
    if st.startswith('#ifdef'):
        keep.append(st.split()[1] == 'release'); out.append(''); continue
    if st.startswith('#else'):
        keep[-1] = not keep[-1]; out.append(''); continue
    if st.startswith('#endif'):
        keep.pop(); out.append(''); continue
    out.append(line if all(keep) else '')
open('ShareableChunk.ml', 'w').write('\n'.join(out))
EOF
rm -f ShareableChunk.cppo.ml

order=$(ocamldep -sort ./*.ml ./*.mli | tr ' ' '\n' | sed 's|^\./||' | grep -v '^bench\.ml$')
for f in $order; do ocamlfind ocamlopt -noassert -O3 -w -a -c "$f"; done
cmx=$(for f in $order; do case "$f" in *.ml) echo "${f%.ml}.cmx";; esac; done | tr '\n' ' ')

ocamlfind ocamlopt -package unix -linkpkg -noassert -O3 -w -a -c bench.ml
ocamlfind ocamlopt -package unix -linkpkg -noassert -O3 -w -a -o "$here/bench-ocaml" $cmx bench.cmx

cd ..
echo "built ./bench-ocaml"
