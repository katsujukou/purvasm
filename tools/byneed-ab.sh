#!/usr/bin/env bash
# ADR-0107 slice-1 run-time A/B: the SAME benchmark program, compiled twice by the SAME Level-2
# compiler with the by-need lattice ON and OFF (`PURVASM_BYNEED_OFF=1`), then measured against each
# other. The lattice is the only variable — same corefn, same ulib, same runtime staticlib, same
# heap size, same binary-producing path.
#
# Why a separate harness. `benchmarks/run-benchmarks.sh`'s `llvm` leg builds with BOOT's backend, so
# it cannot see a Level-2 emitter change at all; `benchmarks/paired-ab.sh` varies the RUNTIME
# staticlib, not the compiler. What this ADR needs is the compiler as the variable and everything
# else fixed.
#
# What the harness fixes, so that claim is structural rather than hopeful:
#
#  * INPUTS ARE SNAPSHOTTED (compiler JS + CoreFn, the wrapper, ulib, the runtime staticlib) and
#    both legs run from the copy — otherwise a concurrent `spago build`/`cargo build` can swap the
#    compiler between the two builds, or a binary's runtime between its build and its timing;
#  * every measurement knob is HARNESS-OWNED: `PURVASM_BYNEED_OFF` is the only difference between
#    the legs, `GC_STRESS`/`STATS`/`EMIT_DEBUG_ABI` are cleared, and `HEAP_WORDS` is cleared from
#    the ambient environment and then SET EXPLICITLY, per program, from the table below — the same
#    value for both legs. Leaving the heap to the built-in default let collection frequency drift
#    between runs and was the single largest noise source here (the same probe went from a per-pair
#    ratio range of [0.897–1.315] to [0.9966–1.0073] once it was pinned);
#  * every run's EXIT STATUS is required to be 0 — comparing stdout alone passes when both legs
#    fail identically, which then reads as a matching pair of programs that never ran;
#  * a program whose `fchk` count does not drop is reported as NO-ELISION rather than timed: it is
#    not a probe for this change.
#
# Measurement discipline (sidenote 0011): reps are PAIRED and the order ALTERNATES between them, and
# the reported statistic is the median of the per-pair ratios with the observed range — not a
# quotient of two independently-taken minima, which are two different machine states.
#
# Usage: tools/byneed-ab.sh [--programs fib,quicksort,...] [--reps N]
set -uo pipefail
cd "$(dirname "$0")/.."
ROOT="$PWD"

REPS=5
PROGRAMS=""
: "${PURVASM_LIB:=$ROOT/dist/ulib}"
: "${PURVASM_RT_A:=$ROOT/runtime/target/release/libpurvasm_rt.a}"
export PURVASM_LIB PURVASM_RT_A

while [ $# -gt 0 ]; do
  case "$1" in
    --reps) REPS="$2"; shift 2 ;;
    --programs) PROGRAMS="$2"; shift 2 ;;
    *) echo "byneed-ab.sh: unknown argument $1" >&2; exit 2 ;;
  esac
done

# <name> <entry module> <arg> <heap words> — `run-benchmarks.sh`'s programs, but at sizes CALIBRATED
# to ~0.3–1.3 s per run rather than that driver's sweep START sizes: at the start sizes every program
# finishes in 6–22 ms, which is process startup plus noise, and an A/B ratio there measures nothing.
# `json-parse` is omitted deliberately — its input scales with a fixture, not with the argument, so
# it has no size knob here (recorded rather than silently dropped).
BENCH_TABLE="
fib              Bench.Fib.Main            20000  8388608
count-state      Bench.CountState.Main     200000 8388608
effect-ref       Bench.EffectRef.Main      500000 8388608
run-state-except Bench.RunStateExcept.Main 50000  33554432
st-ref           Bench.STRef.Main          500000 8388608
map-fold-array   Bench.MapFoldArray.Main   500000 33554432
quicksort        Bench.Quicksort.Main      50000  33554432
"

TIMER=benchmarks/time-run.pl
WORK="${BYNEED_AB_WORK:-$ROOT/_build/byneed-ab}"
rm -rf "$WORK"; mkdir -p "$WORK"

for required in "$PURVASM_LIB" "$PURVASM_RT_A" "$TIMER"; do
  [ -e "$required" ] || { echo "byneed-ab.sh: missing $required" >&2; exit 2; }
done

echo "building the Level-2 compiler ..."
spago build -p cli >"$WORK/spago.log" 2>&1 ||
  { echo "byneed-ab.sh: spago build failed; see $WORK/spago.log" >&2; exit 1; }

# --- pin the inputs (same discipline as byneed-census.sh / selfhost-fixpoint-diff.sh) ----------
# "The lattice is the only variable" has to be structural. Both legs otherwise read the LIVE
# `output/` (compiler JS AND CoreFn), the live wrapper, ulib and runtime staticlib, any of which a
# concurrent `spago build`/`cargo build` can swap between the two builds — or between a build and
# its timing runs, which is worse because the binary would then not match its own `.ll`.
echo "snapshotting inputs (compiler JS + CoreFn, wrapper, ulib, runtime .a) ..."
cp -R "$ROOT/output" "$WORK/output"
mkdir -p "$WORK/cli"
cp "$ROOT/cli/index.node.js" "$WORK/cli/index.node.js"
cp -R "$PURVASM_LIB" "$WORK/ulib"
cp "$PURVASM_RT_A" "$WORK/libpurvasm_rt.a"
SNAP_CLI="$WORK/cli/index.node.js"
SNAP_COREFN="$WORK/output"
export PURVASM_LIB="$WORK/ulib"
export PURVASM_RT_A="$WORK/libpurvasm_rt.a"

# Every measurement knob is HARNESS-owned: the legs differ in `PURVASM_BYNEED_OFF` and in NOTHING
# else, so an ambient `PURVASM_GC_STRESS=1` (or `PURVASM_STATS`, or a heap override) must not reach
# either leg — it would change what is being timed while the summary still claimed defaults.
unset PURVASM_BYNEED_OFF PURVASM_EMIT_DEBUG_ABI PURVASM_GC_STRESS PURVASM_STATS PURVASM_HEAP_WORDS PURVASM_PROFILE_APPLY
# The run wrapper: clean environment, the per-program heap size, and a REQUIRED exit status. The
# heap is passed EXPLICITLY (and identically to both legs) rather than left to the built-in default:
# the table carries a per-program size, and a column the harness silently ignores is a lie about
# what was measured — collection frequency is part of the measurement.
run_app() { # $1=binary  $2=arg  $3=stdout file  $4=heap words
  env -u PURVASM_STATS -u PURVASM_GC_STRESS PURVASM_HEAP_WORDS="$4" "$1" "$2" >"$3" 2>&1
}

printf '%-17s %10s %12s %8s   %s\n' program 'ratio(med)' 'range' 'pairs' 'chains on/off'

rc=0
while read -r name module arg heap; do
  [ -z "$name" ] && continue
  case ",$PROGRAMS," in *,,*) ;; *",$name,"*) ;; *) continue ;; esac

  base="$WORK/$name"
  mkdir -p "$base"

  build_leg() { # $1=leg (on|off)  $2=non-empty ⇒ lattice OFF (the counterfactual)
    local leg="$1" off="$2" r
    if [ -n "$off" ]; then export PURVASM_BYNEED_OFF=1; else unset PURVASM_BYNEED_OFF; fi
    node "$SNAP_CLI" build \
      --corefn-dir "$SNAP_COREFN" --entry "$module" --entry-name main \
      --outdir "$base/$leg" >"$base/$leg.build.log" 2>&1
    r=$?
    unset PURVASM_BYNEED_OFF
    return $r
  }

  if ! build_leg on "" || ! build_leg off 1; then
    printf '%-17s %s\n' "$name" "BUILD-FAIL (see $base/*.build.log)"; rc=1; continue
  fi

  # Self-check. BOTH the exit status and the output are checked: comparing stdout alone passes when
  # the two legs fail identically (same crash, same message on stderr), which would then be
  # "measured" as a matching pair of programs that never ran.
  run_app "$base/on/app" "$arg" "$base/on.out" "$heap"; on_rc=$?
  run_app "$base/off/app" "$arg" "$base/off.out" "$heap"; off_rc=$?
  if [ "$on_rc" -ne 0 ] || [ "$off_rc" -ne 0 ]; then
    printf '%-17s %s\n' "$name" "RUN-FAIL (exit on=$on_rc off=$off_rc — see $base/*.out)"; rc=1; continue
  fi
  if ! diff -q "$base/on.out" "$base/off.out" >/dev/null; then
    printf '%-17s %s\n' "$name" "DIVERGED (on/off stdout differ — see $base/*.out)"; rc=1; continue
  fi

  chains_on=$(grep -hcE '^fchk[0-9]+:' "$base"/on/_build/*.ll | paste -sd+ - | bc)
  chains_off=$(grep -hcE '^fchk[0-9]+:' "$base"/off/_build/*.ll | paste -sd+ - | bc)
  if [ "$chains_on" -ge "$chains_off" ]; then
    # a program where the lattice elides nothing measures nothing about the lattice.
    printf '%-17s %s\n' "$name" "NO-ELISION($chains_on/$chains_off) — not a probe for this change"; rc=1; continue
  fi

  # One untimed warm-up per leg (page cache, first-touch of the fresh binary): without it the
  # first timed rep carries a cold-start penalty. Its exit status is checked like every other run —
  # this harness has no `set -e`, so an unchecked warm-up failure would sail on into the timed reps.
  if ! run_app "$base/on/app" "$arg" "$base/on.warmup.out" "$heap" ||
    ! run_app "$base/off/app" "$arg" "$base/off.warmup.out" "$heap"; then
    printf '%-17s %s\n' "$name" "RUN-FAIL (warm-up — see $base/*.warmup.out)"; rc=1; continue
  fi

  # PAIRED reps with the ORDER ALTERNATED between them, reported as the DISTRIBUTION of the
  # per-pair ratios. Two disciplines, each learned the hard way here:
  #
  #  * alternate the order — running the same leg first in every rep hands it any systematic
  #    first-in-pair penalty, which is how this harness once reported a 1.36× "regression" that
  #    re-measurement could not reproduce;
  #  * ratio per PAIR, then take the median — two independent minima taken minutes apart are two
  #    different machine states, and their quotient is not a ratio of anything. A pair measured
  #    back-to-back shares its machine state, so the pair's ratio is the quantity with meaning,
  #    and the spread across pairs is the evidence about whether it means anything at all.
  : >"$base/pairs.txt"
  i=0
  while [ "$i" -lt "$REPS" ]; do
    if [ $((i % 2)) -eq 0 ]; then order="on off"; else order="off on"; fi
    t_on=""; t_off=""
    for leg in $order; do
      t=$(perl "$TIMER" env -u PURVASM_STATS -u PURVASM_GC_STRESS PURVASM_HEAP_WORDS="$heap" "$base/$leg/app" "$arg")
      if [ $? -ne 0 ] || [ -z "$t" ]; then
        printf '%-17s %s\n' "$name" "RUN-FAIL (timed run of leg $leg failed)"; rc=1; continue 3
      fi
      if [ "$leg" = on ]; then t_on="$t"; else t_off="$t"; fi
    done
    awk -v on="$t_on" -v off="$t_off" 'BEGIN{printf "%.6f %.4f %.4f\n", on/off, on, off}' >>"$base/pairs.txt"
    i=$((i + 1))
  done

  read -r med lo hi n <<EOT
$(sort -n "$base/pairs.txt" | awk '{r[NR]=$1} END {
    if (NR == 0) { print "NA NA NA 0"; exit }
    m = (NR % 2) ? r[(NR+1)/2] : (r[NR/2] + r[NR/2+1]) / 2
    printf "%.4f %.4f %.4f %d\n", m, r[1], r[NR], NR
  }')
EOT
  printf '%-17s %10s %10s %8s   %s/%s\n' "$name" "med=$med" "[${lo}-${hi}]" "n=$n" "$chains_on" "$chains_off"
done <<EOF
$BENCH_TABLE
EOF

echo
echo "med = median of $REPS PAIRED ratios (on/off), [lo–hi] = observed range; < 1.000 = lattice faster."
echo "A ratio whose range straddles 1.000 is not evidence of anything — see the ADR-0107 note on"
echo "this machine class. Snapshotted inputs, harness-owned knobs, exit status required per run."
exit $rc
