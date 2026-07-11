#!/usr/bin/env bash
# The ADR-0082 native-backend **execution** differential: build a small corpus of pure (no-`ulib`,
# `Prim`-only) PureScript fixtures to a native binary with BOTH boot's LLVM backend and the Level-2
# `purvasm build`, run each, and check the two binaries agree with each other and with the expected
# value. This complements `tools/llvm-diff.sh` (which asserts byte-identical `.ll`): here we drive the
# whole native path — codegen → `clang` → link → run — so the pure-value surface (calls, closures,
# recursion, ADT/`case`, records, arrays, strings, by-need CAFs) is exercised end-to-end, not just
# transcription-checked.
#
# Unlike the byte-identity harness, execution tolerates the front-half B1/B2 differences (whole-program
# `$aN` CAF float; reachability DCE), so fixtures need not be single-binding — they only need a
# deterministic `Int` result (the `--value` entry prints it via `pv_print_int`).
#
# Usage (from the repo root, inside `nix develop`):
#   tools/native-run-diff.sh
#
# Prerequisites (located, not built): boot (`dune build` in `boot/`), the runtime staticlib
# (`cargo build --release` in `runtime/`, or `$PURVASM_RT_A`), `clang`, `purs`, `node`.
set -u

cd "$(dirname "${BASH_SOURCE[0]}")/.."
ROOT="$(pwd)"

BOOT="$ROOT/boot/_build/default/bin/main.exe"
FIX_SRC="$ROOT/compiler/test/fixtures/native-run/src"
: "${PURVASM_RT_A:=$ROOT/runtime/target/release/libpurvasm_rt.a}"
: "${PURVASM_LIB:=$ROOT/purvasm_lib}"
export PURVASM_RT_A PURVASM_LIB

# One row per fixture: <module> <entry-name> <expected-stdout>.
CASES="
RunCalls  answer  42
RunCase   answer  55
RunRecord answer  30
RunArray  answer  9
RunStr    answer  2
RunGuard  answer  200
RunRec    answer  3
"

for tool in "$BOOT" "$PURVASM_RT_A"; do
  [ -e "$tool" ] || { echo "missing prerequisite: $tool" >&2; exit 2; }
done

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# Compile every fixture to CoreFn once (into $WORK/output).
( cd "$WORK" && purs compile --codegen corefn "$FIX_SRC"/*.purs >/dev/null 2>&1 ) \
  || { echo "purs compile failed" >&2; exit 2; }
COREFN="$WORK/output"

rc=0
printf '%-12s %-8s  %-8s %-8s %-8s\n' MODULE ENTRY BOOT L2 EXPECT
while read -r mod entry expect; do
  [ -z "$mod" ] && continue

  bdir="$WORK/boot-$mod"; ldir="$WORK/l2-$mod"
  bout="?"; lout="?"

  if "$BOOT" native --backend llvm --no-opt --value \
      --corefn-dir "$COREFN" -m "$mod" -e "$entry" -o "$bdir" >"$bdir.log" 2>&1 \
      && [ -x "$bdir/app" ]; then
    bout="$("$bdir/app" 2>/dev/null)"
  fi

  if node "$ROOT/cli/index.node.js" build --no-opt --value \
      --entry "$mod" --entry-name "$entry" \
      --corefn-dir "$COREFN" --outdir "$ldir" >"$ldir.log" 2>&1 \
      && [ -x "$ldir/app" ]; then
    lout="$("$ldir/app" 2>/dev/null)"
  fi

  status="OK"
  if [ "$bout" != "$expect" ] || [ "$lout" != "$expect" ] || [ "$bout" != "$lout" ]; then
    status="FAIL"; rc=1
  fi
  printf '%-12s %-8s  %-8s %-8s %-8s  %s\n' "$mod" "$entry" "$bout" "$lout" "$expect" "$status"
done <<< "$CASES"

if [ "$rc" -eq 0 ]; then
  echo "★ all native binaries agree (boot == Level-2 == expected)"
else
  echo "✗ divergence — see the table above (logs in the temp workdir until exit)" >&2
fi
exit "$rc"
