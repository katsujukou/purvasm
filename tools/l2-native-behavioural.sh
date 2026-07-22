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
#          too-generous heap fails loudly instead of passing vacuously;
#   4. GC-STRESS legs (ADR-0105 §5 slice 0): the same binaries re-run under `PURVASM_GC_STRESS=1`
#      (collect at EVERY allocation — every missing-root window deterministically exercised),
#      additionally asserting `gc_copied_words > 0` (a stress run that relocated nothing proves
#      the leg exercised nothing — vacuous, so it fails);
#   5. the DEBUG-ABI-PROFILE leg (ADR-0105 §4): `Gate.RootStress` rebuilt with
#      `PURVASM_EMIT_DEBUG_ABI=1` (entry-call IR, no inline fast paths) and linked against the
#      DEBUG runtime staticlib, then run under stress. The pairing is harness-selected, not
#      link-stamp-enforced (debug objects carry no stamp), so the leg itself audits it: no
#      `pv_ctx_abi_v1` reference in the emitted IR, and `nm` shows the debug stamp symbol in the
#      staticlib it linked.
#
# The fixture set deliberately includes dictionary-dispatch-heavy code (the very path bridge
# removal turns on for native `--no-opt`) and transient-garbage churn (rooting/relocation
# coverage for the future liveness-rooting divergence, §5 step 4).
#
# Usage (from the repo root, inside `nix develop`):
#   tools/l2-native-behavioural.sh
#
# Prerequisites (located, not built): boot (`dune build` in boot/), the staged ulib (`dist/ulib`),
# the RELEASE runtime staticlib (or $PURVASM_RT_A) AND the DEBUG one (or $PURVASM_RT_A_DEBUG —
# `cargo build` in runtime/, for the debug-ABI-profile leg), `clang`, `node`, `nm`, and fixture
# CoreFn in `output/` (workspace `spago build`).
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
Gate.RootStress
"

# The debug runtime staticlib for the debug-ABI-profile leg (assertions on, generation-checked
# entries) — a separate knob from PURVASM_RT_A so the release default stays untouched.
: "${PURVASM_RT_A_DEBUG:=$ROOT/runtime/target/debug/libpurvasm_rt.a}"

for tool in "$BOOT" "$PURVASM_RT_A" "$PURVASM_RT_A_DEBUG" "$PURVASM_LIB"; do
  [ -e "$tool" ] || { echo "missing prerequisite: $tool" >&2; exit 2; }
done
for cmd in node clang nm; do
  command -v "$cmd" >/dev/null || { echo "missing prerequisite: $cmd on PATH (run inside nix develop)" >&2; exit 2; }
done

# Own the gate's mode axes (ADR-0105 review P1): NOT setting a variable does not make it absent —
# an ambient PURVASM_GC_STRESS=1 would silently turn every leg into a stress leg, and an ambient
# PURVASM_EMIT_DEBUG_ABI=1 would make even the "release" builds emit debug entry-call IR (which
# runs fine against the release runtime, so everything would stay green while inline-ABI coverage
# silently vanished). The harness unsets both up front; each owned leg passes `=1` explicitly to
# its own child process only.
unset PURVASM_GC_STRESS PURVASM_EMIT_DEBUG_ABI

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

rc=0
printf '%-18s %-8s %-14s %-14s %-16s %-16s\n' MODULE VM "L2(no-opt)" "L2(opt)" "stress(no-opt)" "stress(opt)"

# Token-bounded numeric extraction from THE stats line; empty/multi-token/non-numeric → "".
stat_of() { # $1=err file  $2=field name
  local v
  v="$(grep '^purvasm-stats:v1 ' "$1" | tr ' ' '\n' | sed -n "s/^$2=\([0-9][0-9]*\)\$/\1/p")"
  case "$v" in *[!0-9]* | "") echo "" ;; *) echo "$v" ;; esac
}

# Run one Level-2-built binary under the small heap; echo "OK gcN" / a failure tag.
# stress=1 (ADR-0105 §5): run under PURVASM_GC_STRESS=1 and additionally require
# gc_copied_words > 0 — a stressed run that relocated nothing exercised nothing.
run_l2() { # $1=app path  $2=expected stdout file  $3=stress (0|1)
  local app="$1" ref="$2" stress="${3:-0}" out err gc copied nstats sfx=""
  [ "$stress" = "1" ] && sfx=".stress"
  out="$app$sfx.out"; err="$app$sfx.err"
  # The knob accepts absent-or-"1" ONLY (an empty value aborts by contract), so the variable is
  # set on the stress path and entirely absent otherwise.
  if [ "$stress" = "1" ]; then
    PURVASM_HEAP_WORDS="$GATE_HEAP_WORDS" PURVASM_STATS=1 PURVASM_GC_STRESS=1 \
      "$app" >"$out" 2>"$err" || { echo "RUN-FAIL"; return 1; }
  else
    PURVASM_HEAP_WORDS="$GATE_HEAP_WORDS" PURVASM_STATS=1 \
      "$app" >"$out" 2>"$err" || { echo "RUN-FAIL"; return 1; }
  fi
  if ! diff -q "$ref" "$out" >/dev/null; then
    echo "DIVERGED"; return 1
  fi
  # Stats as a schema, not a substring: exactly ONE `purvasm-stats:v1 ` line may exist (a fixture
  # or diagnostic printing a lookalike `gc_collections=` elsewhere must not be silently adopted),
  # carrying exactly ONE token per field (a multi-match would slip past a naive numeric compare),
  # read token-bounded from that line alone.
  nstats="$(grep -c '^purvasm-stats:v1 ' "$err" || true)"
  if [ "$nstats" != "1" ]; then
    echo "STATS-SCHEMA(x$nstats)"; return 1
  fi
  gc="$(stat_of "$err" gc_collections)"
  if [ -z "$gc" ]; then
    echo "STATS-SCHEMA(gc=missing)"; return 1
  fi
  if [ "$gc" -lt 1 ]; then
    echo "NO-GC($gc)"; return 1
  fi
  if [ "$stress" = "1" ]; then
    copied="$(stat_of "$err" gc_copied_words)"
    if [ -z "$copied" ]; then
      echo "STATS-SCHEMA(copied=missing)"; return 1
    fi
    if [ "$copied" -lt 1 ]; then
      echo "VACUOUS(copied=0)"; return 1
    fi
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

  # GC-stress legs (ADR-0105 §5): the SAME binaries, every allocation collecting. Skipped when
  # the build leg already failed (no binary to run).
  stress_noopt="SKIP"
  [ -x "$base/l2-noopt/app" ] && { stress_noopt="$(run_l2 "$base/l2-noopt/app" "$expected" 1)" || true; }
  stress_opt="SKIP"
  [ -x "$base/l2-opt/app" ] && { stress_opt="$(run_l2 "$base/l2-opt/app" "$expected" 1)" || true; }

  status_ok=1
  [ "$vm" = "OK" ] || status_ok=0
  case "$leg_noopt" in OK*) ;; *) status_ok=0 ;; esac
  case "$leg_opt" in OK*) ;; *) status_ok=0 ;; esac
  case "$stress_noopt" in OK*) ;; *) status_ok=0 ;; esac
  case "$stress_opt" in OK*) ;; *) status_ok=0 ;; esac
  [ "$status_ok" -eq 1 ] || rc=1

  printf '%-18s %-8s %-14s %-14s %-16s %-16s\n' "$mod" "$vm" "$leg_noopt" "$leg_opt" "$stress_noopt" "$stress_opt"
done <<< "$MODULES"

# --- the debug-ABI-profile leg (ADR-0105 §4) ---------------------------------------------------
# Gate.RootStress rebuilt as DEBUG entry-call IR (PURVASM_EMIT_DEBUG_ABI=1, the slice-0 vehicle)
# and linked against the DEBUG runtime staticlib, then run under GC stress. The release/debug
# pairing is harness-selected, not link-stamp-enforced (debug objects carry no stamp), so the leg
# audits it directly: no release-stamp reference in the emitted IR, and the staticlib we linked
# really is the debug one (nm shows its debug stamp symbol).
dbg="FAIL"
dbase="$WORK/debug-abi"
mkdir -p "$dbase"
dexpected="$ROOT/test-fixtures/l2-behavioural/expected/Gate.RootStress.out"
if PURVASM_EMIT_DEBUG_ABI=1 PURVASM_RT_A="$PURVASM_RT_A_DEBUG" \
    node "$ROOT/cli/index.node.js" build --no-opt \
    --entry Gate.RootStress --entry-name main \
    --corefn-dir output --outdir "$dbase/build" >"$dbase/build.log" 2>&1 \
    && [ -x "$dbase/build/app" ]; then
  # (a) EVERY emitted object must be stamp-free (one inline-emitting module would corrupt the
  #     debug generation net silently — entry-call IR alone can't catch it).
  if grep -l "pv_ctx_abi_v1" "$dbase/build/_build/"*.ll >/dev/null 2>&1; then
    dbg="STAMP-IN-DEBUG-IR($(grep -l 'pv_ctx_abi_v1' "$dbase/build/_build/"*.ll | head -1 | xargs basename))"
  # (b) the staticlib we linked really is the debug one: exact-symbol audit BOTH ways — the debug
  #     stamp present AND the release stamp absent (leading `_` = the platform mangle; `-E` with
  #     `$`-anchoring keeps `pv_ctx_abi_v1` from matching its `_debug` sibling).
  elif ! nm "$PURVASM_RT_A_DEBUG" 2>/dev/null | grep -qE ' _?pv_ctx_abi_v1_debug$'; then
    dbg="NOT-DEBUG-RUNTIME(no-debug-stamp)"
  elif nm "$PURVASM_RT_A_DEBUG" 2>/dev/null | grep -qE ' _?pv_ctx_abi_v1$'; then
    dbg="NOT-DEBUG-RUNTIME(release-stamp-present)"
  else
    dbg="$(run_l2 "$dbase/build/app" "$dexpected" 1)" || true
  fi
fi
case "$dbg" in OK*) ;; *) rc=1 ;; esac
printf '%-18s %-8s %-14s %-14s %-16s %-16s\n' "debug-abi(RootStress)" "-" "-" "-" "-" "$dbg"

if [ "$rc" -eq 0 ]; then
  echo "★ Level-2 native behaviour ≡ oracle (boot VM + fixture-owned traces) under forced GC (both modes, all fixtures)"
else
  echo "✗ gate failed — logs under $WORK (kept until this shell exits)" >&2
  # keep the workdir for inspection on failure.
  trap - EXIT
  echo "  workdir: $WORK" >&2
fi
exit "$rc"
