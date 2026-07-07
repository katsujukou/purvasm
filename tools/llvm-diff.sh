#!/usr/bin/env bash
# The ADR-0082 native-backend byte-identity differential: for a given module + entry, boot's LLVM
# backend (`--no-opt`) and the Level-2 native backend (`purvasm build`) must emit byte-identical `.ll`
# — the standing forcing function for the codegen transcription while boot remains the golden reference.
#
# Level 2 is B2 (separate per-module compilation), so each module's object contains all its bindings;
# for a clean file-level diff use an entry whose reachable set is the whole corpus (no dead code), or
# a single-binding module. The init/entry object matches regardless (reachability is entry-driven).
#
# Usage: tools/llvm-diff.sh <corefn-dir> <entry-module> <entry-name> [--value]
#   e.g. tools/llvm-diff.sh /tmp/slice1c/output Slice1 identInt --value
set -euo pipefail

COREFN_DIR="${1:?usage: llvm-diff.sh <corefn-dir> <entry-module> <entry-name> [--value]}"
ENTRY_MODULE="${2:?missing entry-module}"
ENTRY_NAME="${3:?missing entry-name}"
VALUE_FLAG="${4:-}"

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BOOT="$ROOT/boot/_build/default/bin/main.exe"
BOOT_OUT="$(mktemp -d)"
L2_OUT="$(mktemp -d)"

# boot side (whole-program link → transl → program_split partitions per module).
boot_value=""; [ "$VALUE_FLAG" = "--value" ] && boot_value="--value"
"$BOOT" native --backend llvm --no-opt $boot_value \
  --corefn-dir "$COREFN_DIR" -m "$ENTRY_MODULE" -e "$ENTRY_NAME" -o "$BOOT_OUT" >/dev/null

# Level-2 side (B2: per-module `.ll`). `--emit-llvm` stops at the IR (no clang/link — the diff needs
# only the `.ll`). PURVASM_LIB satisfies the ulib overlay resolution.
: "${PURVASM_LIB:=$ROOT/purvasm_lib}"
l2_value=""; [ "$VALUE_FLAG" = "--value" ] && l2_value="--value"
PURVASM_LIB="$PURVASM_LIB" node "$ROOT/cli/index.node.js" build --no-opt --emit-llvm $l2_value \
  --entry "$ENTRY_MODULE" --entry-name "$ENTRY_NAME" \
  --corefn-dir "$COREFN_DIR" --outdir "$L2_OUT" >/dev/null

# Diff every emitted object.
rc=0
for f in "$BOOT_OUT"/_build/*.ll; do
  name="$(basename "$f")"
  if diff -q "$f" "$L2_OUT/_build/$name" >/dev/null 2>&1; then
    echo "  OK   $name"
  else
    echo "  DIFF $name"
    diff "$f" "$L2_OUT/_build/$name" | head -40 || true
    rc=1
  fi
done

rm -rf "$BOOT_OUT" "$L2_OUT"
if [ "$rc" -eq 0 ]; then echo "★ all objects byte-identical (boot --no-opt == Level-2 build)"; fi
exit "$rc"
