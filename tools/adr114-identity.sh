#!/usr/bin/env bash
# ADR-0114 amendment: the byte-identity checkpoint, run at EVERY stage of the ANF parameterisation.
#
# The parameterisation touches the shared ANF type and the bytecode backend, so "nothing moved" has
# to be a check rather than an expectation — and a check at each stage rather than only at the end,
# because a diff found three stages later costs three stages of bisection.
#
# Both backends, one frozen corpus:
#   * LLVM      — the emitted `.ll` for the pinned CoreFn snapshot
#   * bytecode  — the `.pmo`/`.pmi` for a handful of modules through `compile`
#
# The corpus is FROZEN (`_build/adr114-baseline/corefn`) rather than the live `output/`: the compiler
# is its own corpus, so comparing against `output/` would re-derive the input from the tree being
# changed and could not tell an emission change from a corpus change.
#
# THE BASELINE IS A FIXED YARDSTICK. It is NOT re-taken because the corpus grew.
#
# The frozen pair (corefn + its prepared manifest) answers exactly one question: did the COMPILER's
# emission move on THIS corpus. A later merge that grows \`output/\` does not change that question,
# and overwriting the baseline to match a newer tree would destroy the only thing able to notice an
# unintended emission change — including one arriving from another track. The headline MEASUREMENT
# corpus is a separate artifact, built fresh by \`toolchain_prepare\`; the two must not be conflated.
#
# \`baseline\` is therefore a deliberate, reviewed act for an INTENTIONAL emission change only.
#
# Usage:  tools/adr114-identity.sh baseline   (re-take; only on a reviewed, intentional change)
#         tools/adr114-identity.sh check      (compare against the baseline)
set -uo pipefail
cd "$(dirname "$0")/.."
ROOT="$PWD"
B="$ROOT/_build/adr114-baseline"
MODULES="Data.Maybe Data.Array Data.Foldable Control.Monad.ST.Internal"

: "${PURVASM_LIB:=$ROOT/dist/ulib}"
: "${PURVASM_RT_A:=$ROOT/runtime/target/release/libpurvasm_rt.a}"
export PURVASM_LIB PURVASM_RT_A
unset PURVASM_FOREIGN_CLOSURE PURVASM_FOREIGN_CALL PURVASM_STATS PURVASM_PROFILE_APPLY

. "$ROOT/tools/toolchain-manifest.sh"

# The frozen corpus must be the one a PREPARED toolchain leg actually produced, and that must be
# checked on EVERY run rather than confirmed once by hand.
#
# Confirming it by hand was the first version, and a one-off confirmation is not a gate: swap a
# module the entry never reaches and the emitted `.ll` do not move, so `check` reports byte-identity
# for a corpus that is no longer the one the manifest describes. The comparison is byte-identity
# BETWEEN TWO RUNS OVER ONE CORPUS; if the corpus can change underneath it, the property it reports
# is not the property it claims.
verify_corpus() {
  local m="$B/toolchain-manifest.tsv" want have
  [ -f "$m" ] || {
    echo "adr114-identity: no prepared manifest at $m." >&2
    echo "  The frozen corpus has no recorded provenance; re-take it from a toolchain_prepare leg." >&2
    return 1
  }
  grep -qx 'prepared	1' "$m" || {
    echo "adr114-identity: $m is not from a prepared leg (prepared != 1)" >&2
    return 1
  }
  want=$(grep '^corefn	' "$m" | cut -f4)
  [ -n "$want" ] || { echo "adr114-identity: $m records no corefn row" >&2; return 1; }
  have=$(_toolchain_hash "$B/corefn")
  if [ "$want" != "$have" ]; then
    echo "adr114-identity: the frozen corpus is NOT the one the manifest describes." >&2
    echo "  manifest: $want" >&2
    echo "  frozen:   $have" >&2
    echo "  A corpus that moves under the comparison makes byte-identity a statement about nothing." >&2
    return 1
  fi
  echo "corpus    OK  frozen corefn matches the prepared manifest ($(echo "$have" | cut -c1-12)…)"
}

[ -d "$B/corefn" ] || { echo "adr114-identity: no frozen corpus at $B/corefn" >&2; exit 2; }
verify_corpus || exit 1

emit() { # $1 = destination root
  rm -rf "$1"; mkdir -p "$1"
  node cli/index.node.js build --corefn-dir "$B/corefn" --entry Purvasm.CLI.Native \
    --entry-name main --outdir "$1/llvm" --emit-llvm --no-opt >"$1/llvm.log" 2>&1 \
    || { echo "adr114-identity: LLVM emission failed; see $1/llvm.log" >&2; return 1; }
  for m in $MODULES; do
    node cli/index.node.js compile --corefn-dir "$B/corefn" --outdir "$1/bc" --quiet "$m" \
      >>"$1/bc.log" 2>&1 || { echo "adr114-identity: compile $m failed; see $1/bc.log" >&2; return 1; }
  done
}

case "${1:-check}" in
  baseline)
    emit "$B/current" || exit 1
    rm -rf "$B/llvm" "$B/bc"
    mv "$B/current/llvm" "$B/llvm"; mv "$B/current/bc" "$B/bc"; rm -rf "$B/current"
    echo "adr114-identity: baseline taken — $(ls "$B"/llvm/_build/*.ll | wc -l | tr -d ' ') .ll, $(ls "$B"/bc/_build | wc -l | tr -d ' ') bytecode artifacts"
    ;;
  check)
    emit "$B/current" || exit 1
    rc=0
    if diff -r "$B/llvm/_build" "$B/current/llvm/_build" >"$B/llvm.diff" 2>&1; then
      echo "LLVM      OK  $(ls "$B"/llvm/_build/*.ll | wc -l | tr -d ' ') objects byte-identical"
    else
      echo "LLVM      DIFFERS ($(wc -l <"$B/llvm.diff" | tr -d ' ') lines; see $B/llvm.diff)" >&2; rc=1
    fi
    if diff -r "$B/bc/_build" "$B/current/bc/_build" >"$B/bc.diff" 2>&1; then
      echo "bytecode  OK  $(ls "$B"/bc/_build | wc -l | tr -d ' ') artifacts byte-identical"
    else
      echo "bytecode  DIFFERS ($(wc -l <"$B/bc.diff" | tr -d ' ') lines; see $B/bc.diff)" >&2; rc=1
    fi
    exit $rc
    ;;
  *) echo "usage: tools/adr114-identity.sh [baseline|check]" >&2; exit 2 ;;
esac
