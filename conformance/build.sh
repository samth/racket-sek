#!/bin/bash
# Builds the OCaml driver against the reference implementation.
#
#   ./build.sh [path-to-sek-checkout]
#
# With no argument the reference is cloned from Inria's GitLab.  The build
# does not use dune: it emulates the one cppo directive the sources rely on,
# substitutes a stand-in for pprint (which Sek uses only for its debugging
# printers), and compiles the modules in dependency order with ocamlfind.

set -eu
here="$(cd "$(dirname "$0")" && pwd)"
cd "$here"

src=${1:-}
if [ -z "$src" ]; then
  if [ ! -d sek-ocaml ]; then
    git clone --depth 1 https://gitlab.inria.fr/fpottier/sek.git sek-ocaml
  fi
  src=sek-ocaml
fi

rm -rf build
mkdir build
cp "$src"/src/*.ml "$src"/src/*.mli build/
cp PPrint.ml driver.ml build/
cd build

# cppo: the sources use #ifdef dev to guard debugging code; keep it, since the
# assertions it enables are exactly what we want a reference to check.
python3 - <<'EOF'
src = open('ShareableChunk.cppo.ml').read()
out, keep = [], [True]
for line in src.split('\n'):
    st = line.strip()
    if st.startswith('#ifdef'):
        keep.append(st.split()[1] == 'dev'); out.append(''); continue
    if st.startswith('#else'):
        keep[-1] = not keep[-1]; out.append(''); continue
    if st.startswith('#endif'):
        keep.pop(); out.append(''); continue
    out.append(line if all(keep) else '')
open('ShareableChunk.ml', 'w').write('\n'.join(out))
EOF
rm -f ShareableChunk.cppo.ml

order=$(ocamldep -sort ./*.ml ./*.mli)
for f in $order; do ocamlfind ocamlopt -w -a -c "$f"; done
cmx=$(for f in $order; do case "$f" in *.ml) echo "${f%.ml}.cmx";; esac; done | tr '\n' ' ')
ocamlfind ocamlopt -w -a -o ../driver $cmx

cd ..
echo "built ./driver"
