#!/usr/bin/env bash
# ADR-0108 §2 apply census + its six-column accounting gate.
#
# Two legs over snapshotted inputs (same discipline as byneed-census.sh): a real native build
# stopped at `.ll`, and the `census apply` tool, which reads the classification events the emitter
# recorded while emitting those same objects.
#
# The gate reconciles EVERY guest-call form, because the naive "generic == Σ MissReason" is false
# against this emitter in both directions:
#
#   pv_apply    == generic-apply + structural-apply   (the unsaturated-CCtor builder application is
#                                                      a `pv_apply` that no MissReason explains)
#   pv_tailcall == generic-tail                       (a generic TAIL call is a trampoline store,
#                                                      invisible in any `pv_apply` count)
#   musttail    == direct-musttail
#   guestDirect == direct-nontail + wrapper-entry     (a lifted function's generic entry is a direct
#                                                      call in the `.ll` but not a call SITE)
#
# Counting caveats, all three found live by this gate rather than by inspection:
#
#   1. `declare i64 @pv_apply(…)` matches a naive symbol needle — so the needles include the call
#      keyword;
#   2. `musttail call tailcc i64 @…` matches the direct-call needle too — so musttail sites are
#      subtracted from it;
#   3. THE CORPUS IS THE COMPILER. A module that emits LLVM carries the emitted syntax as string
#      constants: `Backend.LLVM.Root` contains `@.str.N = … c" = musttail call tailcc i64 @"`, which
#      a text search counts as a call. Every needle is therefore anchored to the two-space
#      instruction indent, which globals (`@…`) and `declare` lines do not have.
#
# Usage: tools/apply-census.sh [--opt|--no-opt] [--entry MODULE] [--corefn-dir DIR] [--out FILE]
#        tools/apply-census.sh ... --toolchain DIR   (run from a caller-pinned toolchain snapshot:
#        DIR/{output,cli/index.node.js,census/index.js,ulib}; nothing is built or re-snapshotted, and
#        --corefn-dir is taken as-is. Used by apply-profile.sh --selfhost so the census and the
#        profiled binary share ONE compiler, not just one CoreFn closure.)
set -euo pipefail
cd "$(dirname "$0")/.."
ROOT="$PWD"

MODE_FLAG=
MODE_LABEL=opt
ENTRY_MODULE=Purvasm.CLI.Native
ENTRY_NAME=main
COREFN_DIR=output
OUT=apply-census.tsv
TOOLCHAIN=

while [ $# -gt 0 ]; do
  case "$1" in
    --opt) MODE_FLAG=; MODE_LABEL=opt; shift ;;
    --no-opt) MODE_FLAG=--no-opt; MODE_LABEL=no-opt; shift ;;
    --entry) ENTRY_MODULE="$2"; shift 2 ;;
    --entry-name) ENTRY_NAME="$2"; shift 2 ;;
    --corefn-dir) COREFN_DIR="$2"; shift 2 ;;
    --out) OUT="$2"; shift 2 ;;
    --toolchain) TOOLCHAIN="$2"; shift 2 ;;
    *) echo "apply-census.sh: unknown argument $1" >&2; exit 2 ;;
  esac
done

# The CoreFn closure is required either way. The ulib deliberately is NOT checked here: under
# --toolchain it comes from the snapshot (checked in that branch), and demanding an ambient or
# tree-default one first would make a COMPLETE `{output,cli,census,ulib}` snapshot fail over a
# `dist/ulib` the run never touches — which is the opposite of the self-contained contract the
# option advertises.
[ -e "$COREFN_DIR/$ENTRY_MODULE/corefn.json" ] ||
  { echo "apply-census.sh: missing $COREFN_DIR/$ENTRY_MODULE/corefn.json" >&2; exit 1; }

WORK="${APPLY_WORK:-$ROOT/_build/apply-census-$MODE_LABEL}"
rm -rf "$WORK"; mkdir -p "$WORK"

if [ -n "$TOOLCHAIN" ]; then
  # --- caller-supplied toolchain -----------------------------------------------------------------
  # The CLASSIFIER is an input too. Snapshotting only the CoreFn pins what is measured but not what
  # measures it: this script's own `spago build` + re-snapshot of `output/` would re-derive the
  # compiler from whatever the tree holds NOW, so a long-running caller (apply-profile.sh --selfhost
  # spends hours in its earlier legs) could census with a different compiler than the one whose
  # sites it is reporting. With --toolchain, everything is the caller's pinned copy and nothing is
  # rebuilt; the CoreFn dir is taken as-is because it is by contract already inside that snapshot.
  # `output/` is part of the contract, not an extra: both wrappers resolve the compiled modules
  # RELATIVE TO THEMSELVES (`<dir>/cli/../output`), so a toolchain without it silently resolves to
  # nothing and node dies with ERR_MODULE_NOT_FOUND at the first leg.
  for required in "$TOOLCHAIN/output" "$TOOLCHAIN/cli/index.node.js" "$TOOLCHAIN/census/index.js" "$TOOLCHAIN/ulib"; do
    [ -e "$required" ] || { echo "apply-census.sh: --toolchain is missing $required" >&2; exit 1; }
  done
  CLI_JS="$TOOLCHAIN/cli/index.node.js"
  CENSUS_JS="$TOOLCHAIN/census/index.js"
  SNAP_COREFN="$COREFN_DIR"
  export PURVASM_LIB="$TOOLCHAIN/ulib"
  echo "== using caller-pinned toolchain ==================================="
  echo "   toolchain:      $TOOLCHAIN (no build, no re-snapshot)"
  echo "   corefn closure: $SNAP_COREFN"
else
  : "${PURVASM_LIB:=$ROOT/dist/ulib}"
  export PURVASM_LIB
  [ -e "$PURVASM_LIB" ] ||
    { echo "apply-census.sh: missing $PURVASM_LIB" >&2; exit 1; }

  echo "== building (${MODE_LABEL}) =========================================="
  spago build -p census >"$WORK/spago.log" 2>&1 ||
    { echo "apply-census.sh: spago build failed; see $WORK/spago.log" >&2; exit 1; }

  # --- pin the inputs (see byneed-census.sh: `output/` is BOTH compiler JS and the default closure)
  echo "== snapshotting inputs (compiler JS, CoreFn closure, wrappers, ulib) ="
  cp -R "$ROOT/output" "$WORK/output"
  mkdir -p "$WORK/cli" "$WORK/census"
  cp "$ROOT/cli/index.node.js" "$WORK/cli/index.node.js"
  cp "$ROOT/census/index.js" "$WORK/census/index.js"
  cp -R "$PURVASM_LIB" "$WORK/ulib"
  if [ "$(cd "$COREFN_DIR" 2>/dev/null && pwd -P)" = "$(cd "$ROOT/output" && pwd -P)" ]; then
    SNAP_COREFN="$WORK/output"
  else
    cp -R "$COREFN_DIR" "$WORK/corefn"
    SNAP_COREFN="$WORK/corefn"
  fi
  CLI_JS="$WORK/cli/index.node.js"
  CENSUS_JS="$WORK/census/index.js"
  export PURVASM_LIB="$WORK/ulib"
  echo "   corefn closure: $COREFN_DIR → $SNAP_COREFN"
fi
# measurement knobs are harness-owned
unset PURVASM_BYNEED_OFF PURVASM_EMIT_DEBUG_ABI PURVASM_GC_STRESS PURVASM_STATS PURVASM_HEAP_WORDS PURVASM_PROFILE_APPLY

echo "== leg 1: native .ll emission ======================================"
# shellcheck disable=SC2086
node "$CLI_JS" build \
  --corefn-dir "$SNAP_COREFN" --entry "$ENTRY_MODULE" --entry-name "$ENTRY_NAME" \
  --outdir "$WORK/build" --emit-llvm $MODE_FLAG >"$WORK/build.log" 2>&1 ||
  { echo "apply-census.sh: build leg failed; see $WORK/build.log" >&2; exit 1; }

echo "== leg 2: census ==================================================="
# shellcheck disable=SC2086
node "$CENSUS_JS" apply \
  --corefn-dir "$SNAP_COREFN" --entry "$ENTRY_MODULE" --entry-name "$ENTRY_NAME" \
  --workdir "$WORK/census-work" --out "$WORK/$OUT" $MODE_FLAG >"$WORK/census.log" 2>&1 ||
  { echo "apply-census.sh: census leg failed; see $WORK/census.log" >&2; exit 1; }

echo "== reconciliation: six columns, per object ========================="
# Emitted call forms per object.
: >"$WORK/emitted.tsv"
emit_row() { # $1=object id  $2=path
  local id="$1" f="$2" apply tailcall musttail direct
  # `^  ` anchors to the instruction indent — see caveat 3 in the header.
  apply=$(grep -cE '^  .*call i64 @pv_apply\(' "$f" || true)
  tailcall=$(grep -cE '^  call void @pv_tailcall\(' "$f" || true)
  musttail=$(grep -cE '^  .*musttail call' "$f" || true)
  direct=$(grep -cE '^  .*call tailcc i64 @' "$f" || true)
  # a `musttail call tailcc i64 @…` line matches the direct needle too.
  printf '%s\t%s\t%s\t%s\t%s\n' "$id" "$apply" "$tailcall" "$musttail" "$((direct - musttail))" >>"$WORK/emitted.tsv"
}
for ll in "$WORK"/build/_build/mod_*.ll; do
  id=$(basename "$ll" .ll); emit_row "${id#mod_}" "$ll"
done
emit_row entry "$WORK/build/_build/entry.ll"

# Recorded events per object, folded into the same four numbers.
awk -F'\t' '
  /^#/ { next }
  $3 == "class" {
    if ($4 == "generic-apply")    ga[$1] += $5
    else if ($4 == "structural-apply") sa[$1] += $5
    else if ($4 == "generic-tail")     gt[$1] += $5
    else if ($4 == "direct-musttail")  dm[$1] += $5
    else if ($4 == "direct-nontail")   dn[$1] += $5
    else if ($4 == "wrapper-entry")    we[$1] += $5
    seen[$1] = 1
  }
  END { for (o in seen) printf "%s\t%d\t%d\t%d\t%d\n", o, ga[o] + sa[o], gt[o], dm[o], dn[o] + we[o] }
' "$WORK/$OUT" | sort >"$WORK/recorded.tsv"
sort -o "$WORK/emitted.tsv" "$WORK/emitted.tsv"

join -t $'\t' -a1 -a2 -e MISSING -o 0,1.2,1.3,1.4,1.5,2.2,2.3,2.4,2.5 \
  "$WORK/recorded.tsv" "$WORK/emitted.tsv" >"$WORK/joined.tsv"

mismatches=$(awk -F'\t' '$2 != $6 || $3 != $7 || $4 != $8 || $5 != $9' "$WORK/joined.tsv" \
  | tee "$WORK/mismatches.tsv" | wc -l | tr -d ' ')
objects=$(wc -l <"$WORK/joined.tsv" | tr -d ' ')

# The REASON axis, per object. Without this the form columns can be right while the reason rows are
# short, duplicated or mis-keyed: the class totals would still reconcile against the `.ll`, and the
# ranking — the actual output of this census — would be wrong with a green gate.
#
#   generic-apply == Σ generic-apply/<reason>      generic-tail == Σ generic-tail/<reason>
#
# TWO reasons are fail-closed at ZERO, because both are diagnostics rather than levers and a
# non-zero count of either is a compiler bug report that must not be ranked (ADR-0108 §1/§4):
#
#   unknown-key     `readVar` crashes on such a callee, so a successfully emitted object cannot
#                   contain one;
#   callee-literal  applying a literal is not something a well-typed program does. The ADR gives it
#                   the same contract as unknown-key, so the GATE gives it the same treatment —
#                   otherwise the contract is prose and a literal application appearing in dead code
#                   would census green.
awk -F'\t' '
  /^#/ { next }
  $3 == "class"  && $4 == "generic-apply" { ga[$1] = $5; seen[$1] = 1 }
  $3 == "class"  && $4 == "generic-tail"  { gt[$1] = $5; seen[$1] = 1 }
  $3 == "reason" {
    split($4, p, "/")
    if (p[1] == "generic-apply") ra[$1] += $5
    else if (p[1] == "generic-tail") rt[$1] += $5
    if (p[2] == "unknown-key") uk[$1] += $5
    if (p[2] == "callee-literal") cl[$1] += $5
    seen[$1] = 1
  }
  END {
    for (o in seen) {
      if (ga[o] + 0 != ra[o] + 0)
        printf "%s\tgeneric-apply class %d != Σ reasons %d\n", o, ga[o], ra[o]
      if (gt[o] + 0 != rt[o] + 0)
        printf "%s\tgeneric-tail class %d != Σ reasons %d\n", o, gt[o], rt[o]
      if (uk[o] + 0 != 0)
        printf "%s\tunknown-key %d (COMPILER BUG: readVar cannot have emitted this object)\n", o, uk[o]
      if (cl[o] + 0 != 0)
        printf "%s\tcallee-literal %d (COMPILER BUG: a well-typed program does not apply a literal)\n", o, cl[o]
    }
  }
' "$WORK/$OUT" | sort >"$WORK/reason-violations.tsv"
reason_bad=$(wc -l <"$WORK/reason-violations.tsv" | tr -d ' ')

total_calls=$(awk -F'\t' '/^#/ {next} $3 == "class" && $4 != "wrapper-entry" { s += $5 } END {print s}' "$WORK/$OUT")

echo
printf '%-18s %10s %8s\n' "call form" "count" "share"
awk -F'\t' -v tot="$total_calls" '
  /^#/ { next }
  $3 == "class" { cls[$4] += $5 }
  END { for (c in cls) printf "%-18s %10d %7.1f%%\n", c, cls[c], (c == "wrapper-entry" ? 0 : 100 * cls[c] / tot) }
' "$WORK/$OUT" | sort -k2 -rn

echo
printf '%-42s %10s %8s\n' "generic dispatch by reason" "count" "share"
awk -F'\t' '
  /^#/ { next }
  $3 == "reason" { rsn[$4] += $5; tot += $5 }
  END { for (r in rsn) printf "%-42s %10d %7.1f%%\n", r, rsn[r], 100 * rsn[r] / tot }
' "$WORK/$OUT" | sort -k2 -rn

echo
echo "objects reconciled: $objects"
rc=0
if [ "$mismatches" -ne 0 ]; then
  echo "FAIL: $mismatches object(s) where recorded events != emitted call forms:" >&2
  head -10 "$WORK/mismatches.tsv" >&2
  echo "(columns: object, recorded apply/tail/musttail/direct, emitted apply/tail/musttail/direct)" >&2
  rc=1
else
  echo "OK: recorded events == emitted call forms for all $objects objects"
fi
if [ "$reason_bad" -ne 0 ]; then
  echo "FAIL: $reason_bad reason-axis violation(s):" >&2
  head -10 "$WORK/reason-violations.tsv" >&2
  rc=1
else
  echo "OK: every generic class equals the sum of its reasons; unknown-key and callee-literal are 0"
fi
[ "$rc" -eq 0 ] || exit 1
echo "report: $WORK/$OUT"
