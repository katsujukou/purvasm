#!/usr/bin/env bash
# The ADR-0104 §2 **Level-2 native behavioural gate** — the replacement net that must be green
# before the first intentional emission divergence (§5 step 3, bridge removal), and the standing
# behavioural anchor thereafter.
#
# The oracle (ADR-0104 §2, 2026-07-18 amendment) is the **frozen boot VM plus each fixture's OWN
# expected stdout** (test-fixtures/l2-behavioural/expected/<Module>.out — fixture-owned traces, so
# Level-2 and the VM sharing one error still fails). A direct CESK leg is deliberately NOT part of
# the gate: the frozen CESK runner is an implementation of the current sequential semantics, not a
# normative one (see the ADR).
#
# For each fixture module (test-fixtures/l2-behavioural, built into output/ by the workspace
# `spago build`):
#
#   1. reference leg — boot's VM (`purvm build` + `purvm run`), asserted ≡ the fixture's expected
#      stdout;
#   2. Level-2 legs — `purvasm build` (Node-hosted Level-2 compiler) to a native binary, in BOTH
#      `--opt` and `--no-opt`, each run under a deliberately SMALL heap (`PURVASM_HEAP_WORDS`) with
#      `PURVASM_STATS=1`;
#   3. asserts, per Level-2 run:
#        - stdout (values + `Effect` order, line-for-line) ≡ the expected stdout;
#        - the stats schema: EXACTLY one `purvasm-stats:v1 ` line on stderr, and its
#          `gc_collections >= 1` — a run where no collection fired is NOT GC coverage, so a
#          too-generous heap fails loudly instead of passing vacuously.
#
# The fixture set deliberately includes dictionary-dispatch-heavy code (the very path bridge
# removal turns on for native `--no-opt`) and transient-garbage churn (rooting/relocation
# coverage for the future liveness-rooting divergence, §5 step 4).
#
# Usage (from the repo root, inside `nix develop`):
#   tools/l2-native-behavioural.sh
#
# Prerequisites (located, not built): boot (`dune build` in boot/), the staged ulib (`dist/ulib`),
# the release runtime staticlib (or $PURVASM_RT_A), `clang`, `node`, and fixture CoreFn in
# `output/` (workspace `spago build`).
set -u

cd "$(dirname "${BASH_SOURCE[0]}")/.."
ROOT="$(pwd)"

BOOT="$ROOT/boot/_build/default/bin/main.exe"
: "${PURVASM_RT_A:=$ROOT/runtime/target/release/libpurvasm_rt.a}"
: "${PURVASM_LIB:=$ROOT/dist/ulib}"
export PURVASM_RT_A PURVASM_LIB

# Small enough that every fixture's churn overflows it repeatedly; large enough for the live set
# (ulib CAFs + program). The gc_collections>=1 assertion below keeps this honest if fixtures or
# the runtime change: a silently-too-large heap FAILS the gate rather than hollowing it out.
: "${GATE_HEAP_WORDS:=65536}"

MODULES="
Gate.GcChurn
Gate.EffectOrder
Gate.DictDispatch
Gate.Mixed
"

for tool in "$BOOT" "$PURVASM_RT_A" "$PURVASM_LIB"; do
  [ -e "$tool" ] || { echo "missing prerequisite: $tool" >&2; exit 2; }
done
for cmd in node clang; do
  command -v "$cmd" >/dev/null || { echo "missing prerequisite: $cmd on PATH (run inside nix develop)" >&2; exit 2; }
done

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

rc=0
printf '%-18s %-8s %-14s %-14s\n' MODULE VM "L2(no-opt)" "L2(opt)"

# Run one Level-2-built binary under the small heap; echo "OK gcN" / a failure tag.
run_l2() { # $1=app path  $2=expected stdout file
  local app="$1" ref="$2" out err gc nstats
  out="$app.out"; err="$app.err"
  if ! PURVASM_HEAP_WORDS="$GATE_HEAP_WORDS" PURVASM_STATS=1 "$app" >"$out" 2>"$err"; then
    echo "RUN-FAIL"; return 1
  fi
  if ! diff -q "$ref" "$out" >/dev/null; then
    echo "DIVERGED"; return 1
  fi
  # Stats as a schema, not a substring: exactly ONE `purvasm-stats:v1 ` line may exist (a fixture
  # or diagnostic printing a lookalike `gc_collections=` elsewhere must not be silently adopted),
  # carrying exactly ONE `gc_collections=<digits>` token (a multi-match would make `gc` multi-line
  # and slip past a naive numeric compare), read token-bounded from that line alone.
  nstats="$(grep -c '^purvasm-stats:v1 ' "$err" || true)"
  if [ "$nstats" != "1" ]; then
    echo "STATS-SCHEMA(x$nstats)"; return 1
  fi
  gc="$(grep '^purvasm-stats:v1 ' "$err" | tr ' ' '\n' | sed -n 's/^gc_collections=\([0-9][0-9]*\)$/\1/p')"
  case "$gc" in
    *[!0-9]* | "") echo "STATS-SCHEMA(gc=${gc:-missing})"; return 1 ;;
  esac
  if [ "$gc" -lt 1 ]; then
    echo "NO-GC($gc)"; return 1
  fi
  echo "OK gc$gc"; return 0
}

while read -r mod; do
  [ -z "$mod" ] && continue
  base="$WORK/$(echo "$mod" | tr . _)"
  mkdir -p "$base"

  # the fixture-owned expected trace — the oracle both legs are held to.
  expected="$ROOT/test-fixtures/l2-behavioural/expected/$mod.out"
  if [ ! -f "$expected" ]; then
    printf '%-18s %s\n' "$mod" "MISSING-EXPECTED($expected)"
    rc=1
    continue
  fi

  # reference leg: boot VM, asserted against the expected trace.
  vm="FAIL"
  if "$BOOT" build --corefn-dir output --ulib "$PURVASM_LIB" -m "$mod" -o "$base/vm" \
      >"$base/vm.build.log" 2>&1 \
      && "$BOOT" run "$base/vm/app.pvm" >"$base/vm.out" 2>"$base/vm.err"; then
    if diff -q "$expected" "$base/vm.out" >/dev/null; then
      vm="OK"
    else
      vm="DIVERGED"
    fi
  fi

  # Level-2 legs, both modes (plain variables — macOS ships bash 3.2, no associative arrays).
  l2_leg() { # $1=mode-dir-suffix  $2=extra build flag (may be empty)
    local mode="$1" flag="$2" r="BUILD-FAIL"
    if node "$ROOT/cli/index.node.js" build $flag \
        --entry "$mod" --entry-name main \
        --corefn-dir output --outdir "$base/l2-$mode" >"$base/l2-$mode.build.log" 2>&1 \
        && [ -x "$base/l2-$mode/app" ]; then
      r="$(run_l2 "$base/l2-$mode/app" "$expected")" || true
    fi
    echo "$r"
  }
  leg_noopt="$(l2_leg noopt --no-opt)"
  leg_opt="$(l2_leg opt "")"

  status_ok=1
  [ "$vm" = "OK" ] || status_ok=0
  case "$leg_noopt" in OK*) ;; *) status_ok=0 ;; esac
  case "$leg_opt" in OK*) ;; *) status_ok=0 ;; esac
  [ "$status_ok" -eq 1 ] || rc=1

  printf '%-18s %-8s %-14s %-14s\n' "$mod" "$vm" "$leg_noopt" "$leg_opt"
done <<< "$MODULES"

if [ "$rc" -eq 0 ]; then
  echo "★ Level-2 native behaviour ≡ oracle (boot VM + fixture-owned traces) under forced GC (both modes, all fixtures)"
else
  echo "✗ gate failed — logs under $WORK (kept until this shell exits)" >&2
  # keep the workdir for inspection on failure.
  trap - EXIT
  echo "  workdir: $WORK" >&2
fi
exit "$rc"
