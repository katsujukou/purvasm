#!/usr/bin/env bash
# ADR-0109 §5.2/§5.3: the run-time endpoints the three slices left OWED — the A/A noise floor on
# both endpoints of every leg, and the leg's A/B verdict, classified mechanically.
#
# WHAT THIS IS NOT. It is not the mechanical transfer: `tools/apply-profile.sh --paired
# closure|apply|tail` owns that, and owns it exactly (integer identities, cross-mechanism checks,
# fault injection). This harness times the SAME stage lattice that harness builds, and it makes NO
# claim the timings do not carry. In particular a leg whose transfer apply-profile verdicted OK can
# still come out INCONCLUSIVE here; the two answer different questions and neither substitutes.
#
# THE STAGE LATTICE (identical to `--paired`'s axes, so the two harnesses describe one lattice):
#
#   s0  closure=per-use  call=via-apply              the pre-ADR-0109 baseline
#   s1  closure=hoisted  call=via-apply              slice A
#   s2  closure=hoisted  call=direct-apply-only      slice A+B
#   s3  closure=hoisted  call=direct-apply-and-tail  slice A+B+C  (the shipped default)
#
# and the three legs §5.2 requires reported SEPARATELY: A = s0→s1, B = s1→s2, C = s2→s3. They are
# never averaged: slice A's expected large win must not be allowed to absorb a slice B regression,
# which is the whole reason the knob has three call stages rather than two.
#
# THE KNOB IS A BUILD-MODE AXIS (ADR-0109's first paired run failed on this). It decides how the
# MEASURED BINARY is lowered, not what work that binary performs. For a `selfhost` case the measured
# binary is the native compiler and the timed work is a compile — so the WORKLOAD's own emission must
# be identical across stages, which is checked here as a verdict (`diff -r` over the emitted trees),
# not assumed.
#
# WHY THE RELEVANCE GATE EXISTS. §5.2 refuses to time a program the change does not alter. That is
# not a formality on this corpus: measured 2026-08-17, the benchmark programs are essentially not
# probes for this change (`quicksort` moves 23 dispatches out of 6,192,904; `fib` 42 out of 42 total).
# A case that fails its leg's gate is REPORTED as NOT-A-PROBE with its counters, never silently
# dropped — the refusal is a result about the corpus.
#
# THE NOISE FLOOR IS DEFINED BEFORE ANY COMPARISON RUNS (§5.3). Per case and per stage binary, an
# A/A pair set — the same binary against itself, same protocol — gives a paired-ratio distribution
# and its [p5, p95] by NEAREST RANK (idx5 = ceil(0.05N), idx95 = ceil(0.95N)). The leg's floor is the
# ENVELOPE — the union — of its two endpoints' intervals, because removing an allocation can change
# the variance in either direction. Both intervals are recorded, not just the envelope.
#
#   WIN          the leg's paired-ratio median lies BELOW the envelope
#   REGRESSION   it lies ABOVE
#   INCONCLUSIVE it lies INSIDE  — a distinct verdict, never rounded to either neighbour and never
#                                  reported as "no regression"
#
# PLATFORM. §5.2 asks for a quiet dedicated Linux box, and names `perf stat` beside wall time. On a
# machine that is not that, this harness still runs — and says so in the header of its own report:
# the perf columns print `unavailable` with the reason, and the load average is sampled before and
# after and printed. A column that is silently absent is a lie about what was measured; a column that
# says why it is absent is a measurement. A run whose load average exceeds --max-load is REFUSED
# before it starts (override with --max-load, which then appears in the report).
#
# Usage (from the repo root):
#   tools/foreign-ab.sh [--cases a,b,...] [--axes closure,apply,tail] [--pairs N] [--max-load X]
#   tools/foreign-ab.sh --list                 (print the case table and exit)
#   tools/foreign-ab.sh --self-test            (assert the verdict/statistics code on injected data)
set -uo pipefail
cd "$(dirname "$0")/.."
ROOT="$PWD"

PAIRS=20
AXES=closure,apply,tail
CASES=""
MAX_LOAD=2.0
LIST=0
SELF_TEST=0
KEEP_BUILDS=0

while [ $# -gt 0 ]; do
  case "$1" in
    --pairs) PAIRS="$2"; shift 2 ;;
    --axes) AXES="$2"; shift 2 ;;
    --cases) CASES="$2"; shift 2 ;;
    --max-load) MAX_LOAD="$2"; shift 2 ;;
    --list) LIST=1; shift ;;
    --self-test) SELF_TEST=1; shift ;;
    --keep-builds) KEEP_BUILDS=1; shift ;;
    *) echo "foreign-ab.sh: unknown argument $1" >&2; exit 2 ;;
  esac
done

# --- the stage lattice ----------------------------------------------------------------------------
# `<stage> <closure knob> <call knob>`; the legs below name stages, never knobs, so a leg cannot be
# built with a knob combination that is not a stage.
stage_closure() { case "$1" in
    s0) echo per-use ;; s1|s2|s3) echo hoisted ;;
    *) echo "foreign-ab.sh: no such stage: $1" >&2; return 2 ;;
  esac; }
stage_call() { case "$1" in
    s0|s1) echo via-apply ;; s2) echo direct-apply-only ;; s3) echo direct-apply-and-tail ;;
    *) echo "foreign-ab.sh: no such stage: $1" >&2; return 2 ;;
  esac; }

# `<axis> <before stage> <after stage>` — the ADR's three slices.
axis_before() { case "$1" in closure) echo s0 ;; apply) echo s1 ;; tail) echo s2 ;; esac; }
axis_after()  { case "$1" in closure) echo s1 ;; apply) echo s2 ;; tail) echo s3 ;; esac; }

# What each leg must move for the case to BE a probe for it, and what must stay put. `pv_apply_entries`
# and `pv_tailcall_writes` are read from the uninstrumented binary's own `purvasm-stats:v1` line, so
# the gate costs no extra build; the closure axis has no such counter (the per-`Kind` allocation
# census rides the applyprofile line, which needs an instrumented build — apply-profile's business,
# not this harness's), so it gates on the GC counters the removed allocations must move.
axis_moves() { case "$1" in
    closure) echo gc_copied_words ;; apply) echo pv_apply_entries ;; tail) echo pv_tailcall_writes ;;
  esac; }
axis_pinned() { case "$1" in
    closure) echo "" ;; apply) echo pv_tailcall_writes ;; tail) echo "" ;;
  esac; }

# --- the case table -------------------------------------------------------------------------------
# <name> <kind> <target module> <workload> <heap words> <substrate>
#
#   kind=selfhost  the measured binary is the NATIVE COMPILER (built at each stage) and the timed
#                  work is `build --entry <workload> --no-opt --emit-llvm`. This is the workload the
#                  ADR's 437.3 M / 430.2 M / 9.4 M were counted on, so it is the case that can move.
#   kind=program   the measured binary is the benchmark itself, timed on <workload> as its argument.
#
#   substrate      `string` = the ADR-0103 string leaves dominate its foreign dispatches; `mixed`
#                  otherwise. §5.2 asks the corpus to include at least one non-string-dominated case
#                  so the result is not a measurement of `byteAt` alone. Whether any such case is a
#                  PROBE at all is decided by the gate, not by this column — see the report footer.
CASE_TABLE="
selfhost-fib   selfhost Purvasm.CLI.Native Bench.Fib.Main       134217728 string
selfhost-json  selfhost Purvasm.CLI.Native Bench.JsonParse.Main 134217728 string
quicksort      program  Bench.Quicksort.Main   50000            33554432  mixed
map-fold-array program  Bench.MapFoldArray.Main 500000          33554432  mixed
fib            program  Bench.Fib.Main         20000            8388608   mixed
"

if [ "$LIST" = 1 ]; then
  printf '%-15s %-9s %-22s %-22s %-11s %s\n' name kind target workload heap substrate
  echo "$CASE_TABLE" | while read -r n k t w h s; do
    [ -z "${n:-}" ] && continue
    printf '%-15s %-9s %-22s %-22s %-11s %s\n' "$n" "$k" "$t" "$w" "$h" "$s"
  done
  exit 0
fi

# --- statistics ------------------------------------------------------------------------------------
# One awk program owns every reported statistic, so the A/A intervals and the A/B median cannot drift
# apart by being computed in two places. Input: one ratio per line. Output: `n med p5 p95 lo hi`.
#
# NEAREST RANK, stated rather than left to a library: with the ratios sorted ascending, p5 is the
# element at index ceil(0.05n) and p95 the element at ceil(0.95n) (1-based, clamped to [1,n]). At the
# protocol's n=20 that is the 1st and the 19th — so the floor is not defined by a single outlier at
# either end, and it narrows as n grows rather than jumping.
stats_of() { # $1=file of ratios
  sort -n "$1" | awk '
    { r[NR] = $1 }
    END {
      if (NR == 0) { print "0 NA NA NA NA NA"; exit }
      m = (NR % 2) ? r[(NR + 1) / 2] : (r[NR / 2] + r[NR / 2 + 1]) / 2
      i5 = int(0.05 * NR); if (i5 < 0.05 * NR) i5++; if (i5 < 1) i5 = 1
      i95 = int(0.95 * NR); if (i95 < 0.95 * NR) i95++; if (i95 > NR) i95 = NR
      printf "%d %.6f %.6f %.6f %.6f %.6f\n", NR, m, r[i5], r[i95], r[1], r[NR]
    }'
}

# The §5.3 decision rule, as a function of the leg median and the ENVELOPE of the two endpoints'
# A/A intervals. Fail-closed: a missing or non-numeric input is never silently a verdict.
verdict_of() { # $1=median  $2=p5(before)  $3=p95(before)  $4=p5(after)  $5=p95(after)
  awk -v m="$1" -v b5="$2" -v b95="$3" -v a5="$4" -v a95="$5" 'BEGIN {
    if (m == "" || b5 == "" || b95 == "" || a5 == "" || a95 == "" ||
        m == "NA" || b5 == "NA" || b95 == "NA" || a5 == "NA" || a95 == "NA") { print "NO-VERDICT"; exit }
    lo = (b5 < a5) ? b5 : a5          # the envelope is the UNION of the two intervals
    hi = (b95 > a95) ? b95 : a95
    if (m < lo)      print "WIN"
    else if (m > hi) print "REGRESSION"
    else             print "INCONCLUSIVE"
  }'
}

# The ABSOLUTE wall times of a pair set, both legs pooled: `min med max`. The floor is a statement
# about the machine state a set was taken in, so the state has to be observable per set.
wall_of() { # $1=file
  awk '{ print $2; print $3 }' "$1" | sort -n | awk '
    { v[NR] = $1 }
    END { if (NR == 0) { print "NA NA NA"; exit }
          printf "%.2f %.2f %.2f\n", v[1],
                 (NR % 2) ? v[(NR + 1) / 2] : (v[NR / 2] + v[NR / 2 + 1]) / 2, v[NR] }'
}

# THE REGIME CHECK, and why a verdict is worthless without it (found by this harness's own first
# protocol run, 2026-08-18). §5.3's floor is an A/A distribution measured on this machine — which
# only bounds the A/B if the A/B ran in the SAME machine state. On that run the tail axis's A/A sets
# were taken at ~7.6 s/run and then 13 of the 20 A/B pairs ran at ~13.5 s: something else started
# mid-set. The pair RATIOS stayed sane (that is what pairing buys), so nothing in the ratio
# statistics could show it, and the axis reported a confident INCONCLUSIVE against a floor that
# described a machine state its comparison never ran in.
#
# The criterion uses no constant: the A/B set's median must lie inside the wall times the two A/A
# sets actually OBSERVED. Outside that span, the floor never sampled the regime and the verdict is
# withheld rather than reported — the same fail-closed rule the rest of this track uses.
regime_ok() { # $1=ab median  $2..$4=A/A(before) min med max  $5..$7=A/A(after) min med max
  awk -v m="$1" -v b1="$2" -v b3="$4" -v a1="$5" -v a3="$7" 'BEGIN {
    if (m == "NA" || b1 == "NA" || a1 == "NA") { print "no"; exit }
    lo = (b1 < a1) ? b1 : a1
    hi = (b3 > a3) ? b3 : a3
    print (m >= lo && m <= hi) ? "yes" : "no"
  }'
}

envelope_of() { # $1=p5(before) $2=p95(before) $3=p5(after) $4=p95(after)
  awk -v b5="$1" -v b95="$2" -v a5="$3" -v a95="$4" 'BEGIN {
    lo = (b5 < a5) ? b5 : a5; hi = (b95 > a95) ? b95 : a95
    printf "[%.4f-%.4f]\n", lo, hi
  }'
}

if [ "$SELF_TEST" = 1 ]; then
  st_rc=0
  t=$(mktemp); trap 'rm -f "$t"' EXIT
  check() { # $1=label $2=expected $3=actual
    if [ "$2" = "$3" ]; then echo "ok   $1"; else echo "FAIL $1: expected $2, got $3" >&2; st_rc=1; fi
  }
  # nearest-rank at the protocol's n: p5 = 1st, p95 = 19th of 20 — NOT the extremes, so a single
  # outlier at either end widens the range without widening the floor.
  : >"$t"; i=1; while [ "$i" -le 20 ]; do echo "1.$(printf '%04d' $((i * 100)))" >>"$t"; i=$((i + 1)); done
  read -r n med p5 p95 lo hi <<EOT
$(stats_of "$t")
EOT
  check "n"   "20" "$n"
  check "p5"  "1.010000" "$p5"
  check "p95" "1.190000" "$p95"
  check "lo"  "1.010000" "$lo"
  check "hi"  "1.200000" "$hi"
  check "median (even n = mean of the two middles)" "1.105000" "$med"

  check "median below the envelope is a WIN"          "WIN"          "$(verdict_of 0.80 0.99 1.01 0.98 1.02)"
  check "median above the envelope is a REGRESSION"   "REGRESSION"   "$(verdict_of 1.30 0.99 1.01 0.98 1.02)"
  check "median inside the envelope is INCONCLUSIVE"  "INCONCLUSIVE" "$(verdict_of 1.00 0.99 1.01 0.98 1.02)"
  # the envelope is the UNION: a median inside the WIDER endpoint's interval is INCONCLUSIVE even
  # though it sits outside the narrower one. This is the §5.3 P2 correction, and it is the row that
  # fails if the envelope is ever computed as an intersection or from one endpoint alone.
  check "the envelope is the union, not the first endpoint" \
    "INCONCLUSIVE" "$(verdict_of 0.95 0.99 1.01 0.90 1.10)"
  check "a missing input yields NO-VERDICT, never a verdict" "NO-VERDICT" "$(verdict_of 0.80 NA 1.01 0.98 1.02)"
  check "an empty sample yields no statistics" "0 NA NA NA NA NA" "$(: >"$t"; stats_of "$t")"

  # the regime check, over the shape that produced it: the A/A sets sampled ~7.5-8.3 s and the A/B
  # median landed at 13.3 s, so the floor never saw the state the comparison ran in.
  check "an A/B inside the A/A wall span keeps its floor" \
    "yes" "$(regime_ok 8.05 7.83 7.95 8.13 7.90 8.24 8.40)"
  check "the real tail-axis drift is caught"              \
    "no"  "$(regime_ok 13.30 7.46 7.65 8.30 7.42 7.56 8.10)"
  check "a drift to FASTER is caught too, not only slower" \
    "no"  "$(regime_ok 5.10 7.46 7.65 8.30 7.42 7.56 8.10)"
  check "an unmeasurable regime is not silently ok"       \
    "no"  "$(regime_ok NA 7.46 7.65 8.30 7.42 7.56 8.10)"
  # boundary: exactly at the observed edge is INSIDE — the span is what the floor sampled, and a
  # sample at its edge was sampled.
  check "the span's edge is inside it" "yes" "$(regime_ok 8.30 7.46 7.65 8.30 7.42 7.56 8.10)"

  [ "$st_rc" -eq 0 ] && echo "OK: the statistics and the decision rule behave as §5.3 pins them" ||
    echo "FAIL: a self-test row did not hold" >&2
  exit "$st_rc"
fi

# --- prerequisites and the platform record ----------------------------------------------------------
: "${PURVASM_LIB:=$ROOT/dist/ulib}"
: "${PURVASM_RT_A:=$ROOT/runtime/target/release/libpurvasm_rt.a}"
TIMER=benchmarks/time-run.pl
for required in "$PURVASM_LIB" "$PURVASM_RT_A" "$TIMER" "$ROOT/cli/index.node.js"; do
  [ -e "$required" ] || { echo "foreign-ab.sh: missing $required" >&2; exit 2; }
done
for cmd in node clang perl; do
  command -v "$cmd" >/dev/null || { echo "foreign-ab.sh: missing $cmd on PATH" >&2; exit 2; }
done

OS=$(uname -s)
if [ "$OS" = Linux ] && command -v perf >/dev/null 2>&1; then
  PERF_NOTE="perf stat: available"
  HAVE_PERF=1
else
  HAVE_PERF=0
  if [ "$OS" = Linux ]; then PERF_NOTE="perf stat: UNAVAILABLE (perf not on PATH)"
  else PERF_NOTE="perf stat: UNAVAILABLE (not Linux — $OS has no equivalent; §5.2's counter leg does not run)"; fi
fi

load_now() { uptime | sed 's/.*averages*: *//' | awk '{print $1}' | tr -d ','; }
LOAD_START=$(load_now)
if awk -v l="$LOAD_START" -v m="$MAX_LOAD" 'BEGIN { exit !(l > m) }'; then
  echo "foreign-ab.sh: REFUSED — load average $LOAD_START exceeds --max-load $MAX_LOAD." >&2
  echo "  §5.2 asks for a quiet box. Quiesce the machine (close editors/builds) and re-run, or" >&2
  echo "  raise --max-load deliberately — the value is printed in the report either way." >&2
  exit 3
fi

WORK="${FOREIGN_AB_WORK:-$ROOT/_build/foreign-ab}"
[ "$KEEP_BUILDS" = 1 ] || rm -rf "$WORK"
mkdir -p "$WORK"

echo "== building the Level-2 compiler once ==============================="
spago build -p cli >"$WORK/spago.log" 2>&1 ||
  { echo "foreign-ab.sh: spago build failed; see $WORK/spago.log" >&2; exit 1; }

# --- pin the inputs ----------------------------------------------------------------------------------
# Every stage is built from ONE snapshot: this harness runs for hours, and a concurrent `spago build`
# or ulib re-stage between two stage builds would make the stages differ in something other than the
# knob — while the report still named the knob as the variable. `output/` is both the compiler's own
# compiled JS and the CoreFn closure, so it is snapshotted once and used as both.
if [ ! -d "$WORK/output" ]; then
  echo "== snapshotting inputs (compiler JS + CoreFn, ulib, runtime .a) ====="
  cp -R "$ROOT/output" "$WORK/output"
  mkdir -p "$WORK/cli" "$WORK/rt"
  cp "$ROOT/cli/index.node.js" "$WORK/cli/index.node.js"
  cp -R "$PURVASM_LIB" "$WORK/ulib"
  cp "$PURVASM_RT_A" "$WORK/rt/libpurvasm_rt.a"
fi
export PURVASM_LIB="$WORK/ulib"
export PURVASM_RT_A="$WORK/rt/libpurvasm_rt.a"
CLI="$WORK/cli/index.node.js"
COREFN="$WORK/output"

# Every measurement knob is harness-owned. The two ADR-0109 knobs are set per stage below and must
# never be inherited; `PURVASM_STATS` is set only for the gate runs, never for a timed one; an
# ambient `PURVASM_BYNEED_OFF` would time a lattice-OFF compiler while the report described the
# shipped path.
unset PURVASM_FOREIGN_CLOSURE PURVASM_FOREIGN_CALL PURVASM_BYNEED_OFF PURVASM_PROFILE_APPLY \
  PURVASM_EMIT_DEBUG_ABI PURVASM_GC_STRESS PURVASM_STATS PURVASM_HEAP_WORDS

field_of() { # $1=file  $2=counter name — token-bounded, so `pv_apply_entries` never matches a longer key
  tr ' ' '\n' <"$1" | awk -F= -v k="$2" '$1 == k { print $2; found = 1 } END { if (!found) print "" }'
}

# --- the run wrappers --------------------------------------------------------------------------------
# A `selfhost` case's timed work writes an outdir. It is REMOVED BEFORE EACH RUN and outside the
# timer: a compiler that skips existing artifacts would otherwise make every run after the first
# measure a different program.
prep_run() { # $1=kind  $2=outdir
  [ "$1" = selfhost ] && rm -rf "$2"
  return 0
}

# The command a case times, as an argv array in the caller's `CMD`.
build_cmd() { # $1=kind $2=binary $3=workload $4=outdir
  case "$1" in
    selfhost) CMD=("$2" build --corefn-dir "$COREFN" --entry "$3" --entry-name main \
      --outdir "$4" --emit-llvm --no-opt) ;;
    program) CMD=("$2" "$3") ;;
  esac
}

rc=0
REPORT="$WORK/report.txt"
: >"$REPORT"
say() { echo "$*" | tee -a "$REPORT"; }

say "ADR-0109 §5.2 run-time A/A + A/B — $(date '+%Y-%m-%d %H:%M:%S')"
say "host: $OS $(uname -m); $PERF_NOTE"
say "protocol: $PAIRS paired reps per set, order alternated; ratio = t(after)/t(before); floor ="
say "  the union of the two endpoints' A/A [p5,p95] by nearest rank. load at start: $LOAD_START (max $MAX_LOAD)"
# §5.2 pins ≥ 20 pairs, and the reason is visible in the statistics rather than conventional: at small
# n the nearest-rank p5/p95 collapse onto the extreme order statistics (at n=2 the "interval" IS the
# two observed values), so the floor is UNDERSTATED and every verdict is biased away from
# INCONCLUSIVE — in BOTH directions. A short run is therefore a harness exercise, never a result, and
# it says so on its own report rather than leaving the reader to notice the n column.
if [ "$PAIRS" -lt 20 ]; then
  say ""
  say "!! NOT THE PROTOCOL: $PAIRS pairs < the 20 §5.2 requires. At this n the nearest-rank [p5,p95]"
  say "!! degenerates towards the observed extremes, so the noise floor is TOO NARROW and the WIN /"
  say "!! REGRESSION verdicts below are not evidence in either direction. Harness exercise only."
fi
say ""

while read -r name kind target workload heap substrate; do
  [ -z "${name:-}" ] && continue
  case ",$CASES," in *,,*) ;; *",$name,"*) ;; *) continue ;; esac

  say "== case $name ($kind, $target, workload $workload, heap $heap, substrate $substrate) =="
  cdir="$WORK/$name"; mkdir -p "$cdir"

  # which stages this case needs: the endpoints of every selected axis.
  need=""
  for axis in ${AXES//,/ }; do
    for st in "$(axis_before "$axis")" "$(axis_after "$axis")"; do
      case " $need " in *" $st "*) ;; *) need="$need $st" ;; esac
    done
  done

  built_ok=1
  for st in $need; do
    [ -x "$cdir/$st/app" ] && continue
    echo "   building stage $st (closure=$(stage_closure "$st") call=$(stage_call "$st")) ..."
    PURVASM_FOREIGN_CLOSURE="$(stage_closure "$st")" PURVASM_FOREIGN_CALL="$(stage_call "$st")" \
      node "$CLI" build --corefn-dir "$COREFN" --entry "$target" --entry-name main \
      --outdir "$cdir/$st" >"$cdir/$st.build.log" 2>&1 ||
      { say "   BUILD-FAIL stage $st (see $cdir/$st.build.log)"; built_ok=0; break; }
  done
  if [ "$built_ok" != 1 ]; then rc=1; say ""; continue; fi

  # --- equivalence + the counters the gates read -----------------------------------------------------
  # One instrumented-free run per stage under PURVASM_STATS. Both the EXIT STATUS and the produced
  # output are checked: comparing output alone passes when every stage fails identically, which then
  # reads as a lattice of programs that never ran.
  eq_ok=1; ref=""
  for st in $need; do
    prep_run "$kind" "$cdir/$st.work"
    build_cmd "$kind" "$cdir/$st/app" "$workload" "$cdir/$st.work"
    PURVASM_STATS=1 PURVASM_HEAP_WORDS="$heap" "${CMD[@]}" >"$cdir/$st.out" 2>"$cdir/$st.err"
    if [ $? -ne 0 ]; then say "   RUN-FAIL stage $st (see $cdir/$st.err)"; eq_ok=0; break; fi
    if [ -z "$ref" ]; then ref="$st"; continue; fi
    case "$kind" in
      selfhost)
        # the workload's own emission must not move with the knob: the knob is a BUILD-mode axis, and
        # if the emitted trees differ the stages did DIFFERENT WORK and no timing between them means
        # anything (ADR-0109's first paired run failed exactly here).
        diff -r "$cdir/$ref.work" "$cdir/$st.work" >"$cdir/$st.emission.diff" 2>&1 ||
          { say "   DIVERGED stage $st: workload emission differs from $ref (see $cdir/$st.emission.diff)"; eq_ok=0; break; } ;;
      program)
        diff -q "$cdir/$ref.out" "$cdir/$st.out" >/dev/null ||
          { say "   DIVERGED stage $st: stdout differs from $ref"; eq_ok=0; break; } ;;
    esac
  done
  if [ "$eq_ok" != 1 ]; then rc=1; say ""; continue; fi

  # §5.2 asks for the GC counters per leg. They are reported for every stage, not only for the axis
  # that gates on one of them: an allocation change that moved a leg this run was NOT supposed to
  # touch is exactly the kind of thing a table shows and a single gated counter hides.
  for st in $need; do
    say "   stage $st (closure=$(stage_closure "$st") call=$(stage_call "$st")): \
gc_collections=$(field_of "$cdir/$st.err" gc_collections) \
gc_copied_words=$(field_of "$cdir/$st.err" gc_copied_words) \
gc_max_live_words=$(field_of "$cdir/$st.err" gc_max_live_words) \
pv_apply_entries=$(field_of "$cdir/$st.err" pv_apply_entries) \
pv_tailcall_writes=$(field_of "$cdir/$st.err" pv_tailcall_writes)"
  done

  # --- per axis: the relevance gate, the two A/A floors, the A/B verdict -------------------------------
  for axis in ${AXES//,/ }; do
    b=$(axis_before "$axis"); a=$(axis_after "$axis")
    moves=$(axis_moves "$axis"); pinned=$(axis_pinned "$axis")
    mb=$(field_of "$cdir/$b.err" "$moves"); ma=$(field_of "$cdir/$a.err" "$moves")

    if [ -z "$mb" ] || [ -z "$ma" ]; then
      say "   axis $axis: NO-COUNTER ($moves absent from a stats line — gate cannot run)"; rc=1; continue
    fi
    if [ "$ma" -ge "$mb" ] 2>/dev/null; then
      say "   axis $axis: NOT-A-PROBE — $moves $mb -> $ma (this case does not exercise the change)"
      continue
    fi
    if [ -n "$pinned" ]; then
      pb=$(field_of "$cdir/$b.err" "$pinned"); pa=$(field_of "$cdir/$a.err" "$pinned")
      if [ "$pb" != "$pa" ]; then
        say "   axis $axis: AXIS-BLEED — $pinned $pb -> $pa must be INVARIANT on this axis"; rc=1; continue
      fi
    fi

    # PAIRED reps, order ALTERNATED. `x` and `y` are stage directories; for A/A they are the same one.
    pairset() { # $1=label  $2=stage x  $3=stage y  $4=output file
      local label="$1" sx="$2" sy="$3" out="$4" i=0 order tx ty t s
      : >"$out"
      while [ "$i" -lt "$PAIRS" ]; do
        if [ $((i % 2)) -eq 0 ]; then order="x y"; else order="y x"; fi
        tx=""; ty=""
        for slot in $order; do
          if [ "$slot" = x ]; then s="$sx"; else s="$sy"; fi
          prep_run "$kind" "$cdir/timed.work"
          build_cmd "$kind" "$cdir/$s/app" "$workload" "$cdir/timed.work"
          t=$(env -u PURVASM_STATS -u PURVASM_GC_STRESS PURVASM_HEAP_WORDS="$heap" \
            perl "$TIMER" "${CMD[@]}")
          if [ $? -ne 0 ] || [ -z "$t" ]; then
            say "   axis $axis: RUN-FAIL during $label (stage $s)"; return 1
          fi
          if [ "$slot" = x ]; then tx="$t"; else ty="$t"; fi
        done
        awk -v x="$tx" -v y="$ty" 'BEGIN { printf "%.6f %.4f %.4f\n", y / x, x, y }' >>"$out"
        i=$((i + 1))
      done
      return 0
    }

    echo "   axis $axis: A/A on $b ..."
    pairset "A/A($b)" "$b" "$b" "$cdir/$axis.aa-$b.txt" || { rc=1; continue; }
    echo "   axis $axis: A/A on $a ..."
    pairset "A/A($a)" "$a" "$a" "$cdir/$axis.aa-$a.txt" || { rc=1; continue; }
    echo "   axis $axis: A/B $b -> $a ..."
    pairset "A/B" "$b" "$a" "$cdir/$axis.ab.txt" || { rc=1; continue; }

    read -r nb medb p5b p95b lob hib <<EOT
$(stats_of "$cdir/$axis.aa-$b.txt")
EOT
    read -r na meda p5a p95a loa hia <<EOT
$(stats_of "$cdir/$axis.aa-$a.txt")
EOT
    read -r nx medx p5x p95x lox hix <<EOT
$(stats_of "$cdir/$axis.ab.txt")
EOT
    # the absolute wall times, so §5.2's "cases sized to 3–10 s" is READABLE from the report rather
    # than being a property of the case table that nobody re-checks after the corpus moves.
    # `asort` is a gawk extension and this must run under the BSD awk a mac ships, so each column is
    # sorted by `sort` and reduced by the same median expression `stats_of` uses.
    med_col() { # $1=file  $2=column
      awk -v c="$2" '{ print $c }' "$1" | sort -n | awk '
        { v[NR] = $1 }
        END { if (NR == 0) { print "NA"; exit }
              printf "%.2f\n", (NR % 2) ? v[(NR + 1) / 2] : (v[NR / 2] + v[NR / 2 + 1]) / 2 }'
    }
    tb=$(med_col "$cdir/$axis.ab.txt" 2)
    ta=$(med_col "$cdir/$axis.ab.txt" 3)
    read -r wbmin wbmed wbmax <<EOT
$(wall_of "$cdir/$axis.aa-$b.txt")
EOT
    read -r wamin wamed wamax <<EOT
$(wall_of "$cdir/$axis.aa-$a.txt")
EOT
    read -r wxmin wxmed wxmax <<EOT
$(wall_of "$cdir/$axis.ab.txt")
EOT
    env_str=$(envelope_of "$p5b" "$p95b" "$p5a" "$p95a")
    v=$(verdict_of "$medx" "$p5b" "$p95b" "$p5a" "$p95a")
    if [ "$(regime_ok "$wxmed" "$wbmin" "$wbmed" "$wbmax" "$wamin" "$wamed" "$wamax")" != yes ]; then
      v="NO-FLOOR (would have been $v)"
      rc=1
    fi
    say "   axis $axis ($b->$a): $moves $mb -> $ma"
    say "     A/A($b) n=$nb med=$medb [p5,p95]=[$p5b,$p95b] range=[$lob,$hib]  wall ${wbmin}/${wbmed}/${wbmax}s"
    say "     A/A($a) n=$na med=$meda [p5,p95]=[$p5a,$p95a] range=[$loa,$hia]  wall ${wamin}/${wamed}/${wamax}s"
    say "     A/B     n=$nx med=$medx  envelope=$env_str  wall ${wxmin}/${wxmed}/${wxmax}s (${tb}s -> ${ta}s)  ->  $v"
    [ "$v" = "NO-VERDICT" ] && rc=1
  done
  say ""
done <<EOF
$CASE_TABLE
EOF

LOAD_END=$(load_now)
say "load at end: $LOAD_END"
say ""
say "med < envelope = WIN, med > envelope = REGRESSION, med inside = INCONCLUSIVE (never rounded)."
say "NO-FLOOR = the A/B set's wall times fell outside the span the A/A sets sampled, so the floor"
say "describes a machine state the comparison never ran in. The verdict is WITHHELD, not softened."
say "NOT-A-PROBE is a result about the CORPUS, not a skipped case: the leg's counter did not move,"
say "so §5.2's refusal to time a program the change does not alter applies."
say "$PERF_NOTE"
say "report: $REPORT"
exit $rc
