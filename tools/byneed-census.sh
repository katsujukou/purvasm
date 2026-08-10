#!/usr/bin/env bash
# ADR-0107 by-need demand-site census, with its accounting gate.
#
# Two legs over the SAME snapshotted inputs, the same entry and the same optimiser mode. The
# inputs (CoreFn closure + the compiler's own compiled JS + the two wrappers + the ulib overlay)
# are copied into $WORK first and both legs run from the copy, so "same input" is structural: a
# concurrent `spago build` cannot swap the compiler between leg 1 and leg 2.
#
#   1. a real native build stopped at `.ll` (`purvasm build --emit-llvm`) — the ground truth;
#   2. the `census byneed` tool, which drives the same `Purvasm.Compiler.build` with a census
#      `Backend` and the CLI's own `CompilerAction`.
#
# The gate: for EVERY emitted object, the census's counted demand sites must equal the number of
# `fchk` force chains in that object's `.ll`. A census that predicts fewer (or more) chains than the
# emitter emits is measuring something other than what it claims — that is exactly how the first
# 2026-08-06 census under-counted (it walked the raw ANF alternatives, while the emitter emits a
# force per decision-tree occurrence, and rows duplicate across specialised branches).
#
# Usage: tools/byneed-census.sh [--opt] [--entry MODULE] [--corefn-dir DIR] [--out FILE]
#   --opt   census the optimiser'd corpus (default: --no-opt, ADR-0107's primary corpus)
set -euo pipefail
cd "$(dirname "$0")/.."
ROOT="$PWD"

MODE_FLAG=--no-opt
MODE_LABEL=no-opt
ENTRY_MODULE=Purvasm.CLI.Native
ENTRY_NAME=main
COREFN_DIR=output
OUT=byneed-census.tsv

while [ $# -gt 0 ]; do
  case "$1" in
    --opt) MODE_FLAG=; MODE_LABEL=opt; shift ;;
    --no-opt) MODE_FLAG=--no-opt; MODE_LABEL=no-opt; shift ;;
    --entry) ENTRY_MODULE="$2"; shift 2 ;;
    --entry-name) ENTRY_NAME="$2"; shift 2 ;;
    --corefn-dir) COREFN_DIR="$2"; shift 2 ;;
    --out) OUT="$2"; shift 2 ;;
    *) echo "byneed-census.sh: unknown argument $1" >&2; exit 2 ;;
  esac
done

: "${PURVASM_LIB:=$ROOT/dist/ulib}"
export PURVASM_LIB

for required in "$COREFN_DIR/$ENTRY_MODULE/corefn.json" "$PURVASM_LIB"; do
  [ -e "$required" ] || { echo "byneed-census.sh: missing $required" >&2; exit 1; }
done

WORK="${BYNEED_WORK:-$ROOT/_build/byneed-census-$MODE_LABEL}"
rm -rf "$WORK"
mkdir -p "$WORK"

echo "== building (${MODE_LABEL}) =========================================="
# `census` depends on `cli`, so this one build covers both legs' entry points.
spago build -p census >"$WORK/spago.log" 2>&1 ||
  { echo "byneed-census.sh: spago build failed; see $WORK/spago.log" >&2; exit 1; }

# --- pin the inputs: snapshot everything BOTH legs read (as selfhost-fixpoint-diff.sh does) ----
# The two legs run minutes apart against a tree that is itself the corpus, so "same input" must be
# a structural property of this harness, not a property of nobody having run `spago build` in
# between.
#
# TWO snapshots, deliberately, because `output/` plays TWO roles: it is the compiled JS of the
# compiler the wrappers import (by the relative path `../output/…`) AND, by default, the CoreFn
# closure under measurement. Copying it once and using the copy for both silently ignores
# `--corefn-dir`: a run asked to measure a different closure would measure `output/` anyway.
echo "== snapshotting inputs (compiler JS, CoreFn closure, wrappers, ulib) ="
cp -R "$ROOT/output" "$WORK/output"          # the compiler's own JS (the wrappers' `../output`)
mkdir -p "$WORK/cli" "$WORK/census"
cp "$ROOT/cli/index.node.js" "$WORK/cli/index.node.js"
cp "$ROOT/census/index.js" "$WORK/census/index.js"
cp -R "$PURVASM_LIB" "$WORK/ulib"
# The CoreFn closure under measurement: the same copy when it IS `output/` (the default — copying
# it twice would only waste minutes), a SEPARATE snapshot otherwise.
if [ "$(cd "$COREFN_DIR" 2>/dev/null && pwd -P)" = "$(cd "$ROOT/output" && pwd -P)" ]; then
  SNAP_COREFN="$WORK/output"
else
  cp -R "$COREFN_DIR" "$WORK/corefn"
  SNAP_COREFN="$WORK/corefn"
fi
SNAP_ULIB="$WORK/ulib"
echo "   corefn closure: $COREFN_DIR → $SNAP_COREFN"
# the compiler under the ulib overlay reads `$PURVASM_LIB`; point it INTO the snapshot too.
export PURVASM_LIB="$SNAP_ULIB"
# Measurement knobs are HARNESS-owned: an ambient export must not silently change what the two
# legs mean (a stray `PURVASM_BYNEED_OFF` would make the "with the lattice" leg the counterfactual).
unset PURVASM_BYNEED_OFF PURVASM_EMIT_DEBUG_ABI PURVASM_GC_STRESS PURVASM_STATS PURVASM_HEAP_WORDS

echo "== leg 1: native .ll emission ======================================"
# `cli/index.node.js` is the Node-interpreted Level-2 compiler — the one built from this tree by
# the `spago build` above. (`cli/index.js` is the launcher for the prebuilt NATIVE binary under
# `cli/bin`, which is a different, possibly older compiler; it must not be used here.)
# shellcheck disable=SC2086 # MODE_FLAG is an intentional word-or-nothing
node "$WORK/cli/index.node.js" build \
  --corefn-dir "$SNAP_COREFN" --entry "$ENTRY_MODULE" --entry-name "$ENTRY_NAME" \
  --outdir "$WORK/build" --emit-llvm $MODE_FLAG >"$WORK/build.log" 2>&1 ||
  { echo "byneed-census.sh: build leg failed; see $WORK/build.log" >&2; exit 1; }

echo "== leg 2: census ==================================================="
# shellcheck disable=SC2086
node "$WORK/census/index.js" byneed \
  --corefn-dir "$SNAP_COREFN" --entry "$ENTRY_MODULE" --entry-name "$ENTRY_NAME" \
  --workdir "$WORK/census-work" --out "$WORK/$OUT" $MODE_FLAG >"$WORK/census.log" 2>&1 ||
  { echo "byneed-census.sh: census leg failed; see $WORK/census.log" >&2; exit 1; }

echo "== reconciliation: sites == chains, per object ======================"
# Emitted chains per object. `forceValue` is the only producer of an `fchk` block label, so the
# count of `fchk<n>:` label definitions IS the object's force-chain count.
: >"$WORK/chains.tsv"
for ll in "$WORK"/build/_build/mod_*.ll; do
  idx=$(basename "$ll" .ll); idx=${idx#mod_}
  printf '%s\t%s\n' "$idx" "$(grep -cE '^fchk[0-9]+:' "$ll" || true)" >>"$WORK/chains.tsv"
done
printf 'entry\t%s\n' "$(grep -cE '^fchk[0-9]+:' "$WORK/build/_build/entry.ll" || true)" >>"$WORK/chains.tsv"

# Census EMITTED occurrences per object (the `all` row's `emitted` column). Report columns:
# index, object, class, elided, emitted. The elided column is checked separately below.
awk -F'\t' '$3 == "all" { print $1 "\t" $5 }' "$WORK/$OUT" | sort >"$WORK/sites.tsv"
sort -o "$WORK/chains.tsv" "$WORK/chains.tsv"

join -t $'\t' -a1 -a2 -e MISSING -o 0,1.2,2.2 "$WORK/sites.tsv" "$WORK/chains.tsv" >"$WORK/joined.tsv"

mismatches=$(awk -F'\t' '$2 != $3' "$WORK/joined.tsv" | tee "$WORK/mismatches.tsv" | wc -l | tr -d ' ')
objects=$(wc -l <"$WORK/joined.tsv" | tr -d ' ')

echo
awk -F'\t' '
  /^#/ { next }
  $3 != "all" { elided[$3] += $4; emitted[$3] += $5 }
  $3 == "all" { te += $4; tm += $5; objs++ }
  END {
    printf "%-16s %10s %10s %9s\n", "class", "elided", "emitted", "elided%"
    for (c in emitted) {
      t = elided[c] + emitted[c]
      printf "%-16s %10d %10d %8.2f%%\n", c, elided[c], emitted[c], (t ? 100 * elided[c] / t : 0)
    }
    t = te + tm
    printf "%-16s %10d %10d %8.2f%%   (%d objects)\n", "TOTAL", te, tm, (t ? 100 * te / t : 0), objs
  }
' "$WORK/$OUT"

echo
echo "objects reconciled: $objects"
if [ "$mismatches" -ne 0 ]; then
  echo "FAIL: $mismatches object(s) where census-emitted occurrences != emitted fchk chains:" >&2
  head -20 "$WORK/mismatches.tsv" >&2
  echo "(columns: object, census emitted, .ll chains; full list in $WORK/mismatches.tsv)" >&2
  exit 1
fi
echo "OK: census emitted occurrences == emitted force chains for all $objects objects"

# The ADR-0107 §2 accounting identity, stated over emission occurrences: the chains the lattice
# DELETED must equal the occurrences it proved `Never`. The counterfactual leg is the same build
# with the lattice off (`PURVASM_BYNEED_OFF=1`), so "deleted" is measured, not inferred.
if [ -n "${BYNEED_ACCOUNTING:-}" ]; then
  echo
  echo "== accounting: deleted chains == elided occurrences ================="
  # shellcheck disable=SC2086
  PURVASM_BYNEED_OFF=1 node "$WORK/cli/index.node.js" build \
    --corefn-dir "$SNAP_COREFN" --entry "$ENTRY_MODULE" --entry-name "$ENTRY_NAME" \
    --outdir "$WORK/build-off" --emit-llvm $MODE_FLAG >"$WORK/build-off.log" 2>&1 ||
    { echo "byneed-census.sh: counterfactual leg failed; see $WORK/build-off.log" >&2; exit 1; }

  chains_on=$(cat "$WORK/chains.tsv" | awk -F'\t' '{s += $2} END {print s}')
  chains_off=$(grep -hcE '^fchk[0-9]+:' "$WORK"/build-off/_build/*.ll | paste -sd+ - | bc)
  elided=$(awk -F'\t' '$3 == "all" { s += $4 } END {print s}' "$WORK/$OUT")
  deleted=$((chains_off - chains_on))
  echo "chains without the lattice: $chains_off"
  echo "chains with the lattice:    $chains_on"
  echo "deleted:                    $deleted"
  echo "elided occurrences:         $elided"
  if [ "$deleted" -ne "$elided" ]; then
    echo "FAIL: deleted chains ($deleted) != elided occurrences ($elided)" >&2
    exit 1
  fi
  echo "OK: accounting identity holds"
fi
echo "report: $WORK/$OUT"
