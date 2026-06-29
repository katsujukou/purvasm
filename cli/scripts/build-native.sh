#!/usr/bin/env sh
# Build the native Level-3 purvasm and stage it for the launcher (`cli/index.js`, ADR-0055):
#
#   1. build the Level-1 bootstrap (`boot`, OCaml)
#   2. compile the Level-2 PureScript compiler to CoreFn (`spago build`)
#   3. have Level-1 compile Level-2 to a native binary (`purvm native`, overlaying `ulib`)
#   4. stage the binary + the platform-independent `ulib` under `cli/bin/`, where `index.js` resolves
#      them relative to itself and passes `ulib` to the binary via `PURVASM_LIB`.
#
# Run from anywhere; paths are resolved from this script's location.
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT=$(CDPATH= cd -- "$SCRIPT_DIR/../.." && pwd)
cd "$ROOT"

# The staged ulib overlay (refresh with `sh ulib-tools/prepare-release.sh`). Honour PURVASM_LIB.
ULIB="${PURVASM_LIB:-dist/ulib}"
PURVM="boot/_build/default/bin/main.exe"

if [ ! -d "$ULIB" ]; then
  echo "build-native: ulib not found at '$ULIB' — run: sh ulib-tools/prepare-release.sh" >&2
  exit 1
fi

echo "==> [1/4] build Level-1 bootstrap (boot)"
( cd boot && dune build )

echo "==> [2/4] compile Level-2 to CoreFn (spago build)"
spago build

echo "==> [3/4] Level-1 compiles Level-2 -> native (purvm native)"
"$PURVM" native -m Purvasm.CLI.Native --ulib "$ULIB" -o output-purvm

echo "==> [4/4] stage cli/bin (binary + ulib)"
mkdir -p cli/bin
cp output-purvm/app cli/bin/purvasm
chmod +x cli/bin/purvasm
rm -rf cli/bin/ulib
cp -R "$ULIB" cli/bin/ulib

echo "==> done: cli/bin/purvasm (+ cli/bin/ulib). Run e.g.: node cli/index.js --version"
