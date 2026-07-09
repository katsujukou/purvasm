#!/usr/bin/env bash
# The cross-backend wall-clock benchmark driver (ADR-0075): for each benchmark program and each
# implementation leg, sweep input sizes (doubling), record min-of-k whole-process wall times to
# CSV, render log-log plots, and print a summary of per-series scaling exponents and ratios to
# the `js` baseline.
#
# Usage (from the repo root, inside `nix develop`):
#
#   ./benchmarks/run-benchmarks.sh [--legs vm,ml,llvm,js] [--programs fib,quicksort,...]
#                                  [--reps K] [--budget SECONDS] [--max-points N]
#
#   ./benchmarks/run-benchmarks.sh --opt-effect [--programs fib,...]
#       Optimiser-effect mode (ADR-0082 §2): compile the corpus with the *Level-2* CLI at --opt
#       and --no-opt, run each on the VM with deterministic instruction counting (`purvm run
#       --count`), and report the opt/no-opt instruction ratio. VM-only (the wall-clock legs
#       stay the cross-backend story); instruction count is exact, so no reps/timing apply.
#       Needs `spago build` (the Level-2 CLI JS) + boot + ulib — not the runtime staticlib.
#
# Prerequisites: workspace `spago build` (CoreFn + JS output), boot `dune build`, the runtime
# staticlib (`cargo build --release` in runtime/ — the default lookup prefers release), clang,
# node, gnuplot. Timing is external and whole-process (ADR-0075 §5; no guest clock leaf).
set -u

cd "$(dirname "$0")/.."

PURVM=./boot/_build/default/bin/main.exe
TIMER=benchmarks/time-run.pl
OUT=benchmarks/out
LEGS="vm ml llvm js"
REPS=3
BUDGET=2.0
MAXPOINTS=12
ONLY=""
OPT_EFFECT=0

while [ $# -gt 0 ]; do
  case "$1" in
    --opt-effect)
      OPT_EFFECT=1
      ;;
    --legs)
      shift
      LEGS="${1//,/ }"
      ;;
    --programs)
      shift
      ONLY="${1//,/ }"
      ;;
    --reps)
      shift
      REPS="$1"
      ;;
    --budget)
      shift
      BUDGET="$1"
      ;;
    --max-points)
      shift
      MAXPOINTS="$1"
      ;;
    *)
      echo "unknown argument: $1" >&2
      exit 2
      ;;
  esac
  shift
done

# One line per benchmark: <name> <entry module> <start n> <llvm --heap-words>. The completeness
# check below errors on a benchmarks/bench-* package missing from this table (no silent omission).
BENCH_TABLE="
fib             Bench.Fib.Main           250    8388608
count-state     Bench.CountState.Main    1000   8388608
map-fold-array  Bench.MapFoldArray.Main  4000   33554432
quicksort       Bench.Quicksort.Main     1000   33554432
json-parse      Bench.JsonParse.Main     500    33554432
"

if [ ! -x "$PURVM" ]; then
  echo "error: $PURVM not found — run \`dune build\` in boot/ first" >&2
  exit 2
fi

missing=0
for d in benchmarks/bench-*/; do
  name=$(basename "$d")
  name=${name#bench-}
  if ! echo "$BENCH_TABLE" | awk '{print $1}' | grep -qx "$name"; then
    echo "error: benchmarks/bench-$name has no entry in run-benchmarks.sh — add it" >&2
    missing=1
  fi
done
[ $missing -eq 0 ] || exit 2

mkdir -p "$OUT"

# --- optimiser-effect mode (ADR-0082 §2) ------------------------------------------------
# Compile each benchmark with the Level-2 CLI at --opt and --no-opt, run both on the VM with
# deterministic instruction counting, and report the opt/no-opt ratio. The instruction count is
# exact (a fired optimisation moves it, ADR-0030), so one size per benchmark suffices — no reps.
if [ "$OPT_EFFECT" = "1" ]; then
  L2_JS="output/Purvasm.CLI.Main/index.js"
  if [ ! -f "$L2_JS" ]; then
    echo "error: Level-2 CLI not built ($L2_JS) — run \`spago build\` first" >&2
    exit 2
  fi
  ULIB="${PURVASM_LIB:-dist/ulib}"
  if [ ! -d "$ULIB" ]; then
    echo "error: ulib not found at '$ULIB' — run \`sh ulib-tools/prepare-release.sh\` or set PURVASM_LIB" >&2
    exit 2
  fi
  # A stable entry into the freshly-built Level-2 CLI JS (Main drops argv[0..1] = node + script).
  L2_ENTRY="$OUT/l2-cli.mjs"
  printf 'import { main } from "%s/%s";\nmain();\n' "$PWD" "$L2_JS" >"$L2_ENTRY"

  # Compile <module> to <outdir>/app.pvm at the given optimisation mode (opt|noopt).
  l2_compile() {
    local flag=""
    [ "$3" = "noopt" ] && flag="--no-opt"
    PURVASM_LIB="$ULIB" node "$L2_ENTRY" run --corefn-dir output --outdir "$2" --entry "$1" $flag \
      >"$2/compile.log" 2>&1
  }
  # `purvm run --count` prints stats to stderr, the guest's result to stdout.
  instr_count() { "$PURVM" run --count "$1" "$2" 2>&1 1>/dev/null | awk '/instructions/{print $2}'; }
  guest_out() { "$PURVM" run "$1" "$2" 2>/dev/null; }

  optsum="$OUT/opt-effect.txt"
  printf '%-16s %-10s %14s %14s %8s %7s\n' bench n opt no-opt ratio 'red%' | tee "$optsum"
  echo "$BENCH_TABLE" | while read -r name module start heap; do
    [ -n "$name" ] || continue
    if [ -n "$ONLY" ] && ! echo "$ONLY" | tr ' ' '\n' | grep -qx "$name"; then continue; fi
    odir="$OUT/opt/$name/opt"
    ndir="$OUT/opt/$name/noopt"
    mkdir -p "$odir" "$ndir"
    if ! l2_compile "$module" "$odir" opt || ! l2_compile "$module" "$ndir" noopt; then
      printf '%-16s %-10s %14s\n' "$name" "$start" "COMPILE-FAILED" | tee -a "$optsum"
      touch "$OUT/.opt-failed"
      continue
    fi
    # Correctness: --opt must not change the observable output, and must equal --no-opt.
    oo=$(guest_out "$odir/app.pvm" "$start")
    no=$(guest_out "$ndir/app.pvm" "$start")
    if [ -z "$oo" ] || [ "$oo" != "$no" ]; then
      printf '%-16s %-10s %14s (opt=%s / noopt=%s)\n' "$name" "$start" "RUN-FAILED/DIVERGED" "$oo" "$no" | tee -a "$optsum"
      touch "$OUT/.opt-failed"
      continue
    fi
    oi=$(instr_count "$odir/app.pvm" "$start")
    ni=$(instr_count "$ndir/app.pvm" "$start")
    ratio=$(awk -v n="$ni" -v o="$oi" 'BEGIN { if (o > 0) printf "%.3f", n / o; else printf "n/a" }')
    red=$(awk -v n="$ni" -v o="$oi" 'BEGIN { if (n > 0) printf "%.1f", (n - o) * 100 / n; else printf "n/a" }')
    printf '%-16s %-10s %14s %14s %8s %7s\n' "$name" "$start" "$oi" "$ni" "$ratio" "$red" | tee -a "$optsum"
  done
  echo
  echo "opt-effect summary → $optsum  (--opt vs --no-opt VM instruction counts; exact, ratio = noopt/opt)"
  st=0
  [ -f "$OUT/.opt-failed" ] && st=1
  rm -f "$OUT/.opt-failed"
  exit $st
fi

# min of $REPS timed runs of the argv command; empty output on any failed run.
time_min() {
  local best=""
  local t
  for _ in $(seq 1 "$REPS"); do
    t=$(perl "$TIMER" "$@") || return 1
    if [ -z "$best" ]; then best=$t; else best=$(awk -v a="$best" -v b="$t" 'BEGIN { print (b < a) ? b : a }'); fi
  done
  echo "$best"
}

# Build one (program, leg) once; echo the run-command prefix (the size is appended per point).
# An artifact older than $PURVM is STALE, not a cache hit: a toolchain rebuild must rebuild the
# legs, or the sweep silently times binaries of the previous compiler (and the vm leg trips the
# artifact version check instead of running).
fresh() { [ -e "$1" ] && [ ! "$PURVM" -nt "$1" ]; }

build_leg() {
  local name=$1 module=$2 heap=$3 leg=$4
  local bdir="$OUT/build/$name/$leg"
  mkdir -p "$bdir"
  case "$leg" in
    vm)
      fresh "$bdir/app.pvm" || "$PURVM" build --corefn-dir output --ulib dist/ulib -m "$module" -o "$bdir" >"$bdir/build.log" 2>&1 || return 1
      echo "$PURVM run $bdir/app.pvm"
      ;;
    ml)
      fresh "$bdir/app" || "$PURVM" native --backend ocaml --corefn-dir output --ulib dist/ulib -m "$module" -o "$bdir" >"$bdir/build.log" 2>&1 || return 1
      echo "$bdir/app"
      ;;
    llvm)
      fresh "$bdir/app" || "$PURVM" native --backend llvm --heap-words "$heap" --corefn-dir output --ulib dist/ulib -m "$module" -o "$bdir" >"$bdir/build.log" 2>&1 || return 1
      echo "$bdir/app"
      ;;
    js)
      if [ ! -f "$bdir/main.mjs" ]; then
        printf 'import { main } from "%s/output/%s/index.js";\nmain();\n' "$PWD" "$module" >"$bdir/main.mjs"
      fi
      echo "node $bdir/main.mjs"
      ;;
    *)
      echo "unknown leg: $leg" >&2
      return 1
      ;;
  esac
}

overall=0
summary="$OUT/summary.txt"
: >"$summary"

echo "$BENCH_TABLE" | while read -r name module start heap; do
  [ -n "$name" ] || continue
  if [ -n "$ONLY" ] && ! echo "$ONLY" | tr ' ' '\n' | grep -qx "$name"; then continue; fi
  echo "== $name ($module)"

  # Build every leg, then a self-check differential at the smallest size: every leg must print
  # the same output before any timing is trusted. Each passing leg's run-command is recorded in a
  # per-leg marker file (`.timecmd`) rather than an associative array — portable to bash 3.2
  # (macOS' default, which lacks `declare -A`), and the `while read` pipe already runs this body in
  # a subshell, so a file is the natural carrier across the build/timing split anyway.
  ref=""
  for leg in $LEGS; do
    rm -f "$OUT/build/$name/$leg/.timecmd" # clear any stale marker from a previous run
    if ! cmd=$(build_leg "$name" "$module" "$heap" "$leg"); then
      echo "   $leg: BUILD FAILED (see $OUT/build/$name/$leg/build.log)"
      touch "$OUT/.failed"
      continue
    fi
    out=$($cmd "$start" 2>/dev/null)
    if [ -z "$ref" ]; then
      ref=$out
    elif [ "$out" != "$ref" ]; then
      echo "   $leg: SELF-CHECK DIVERGED at n=$start ('$out' vs '$ref') — leg skipped"
      touch "$OUT/.failed"
      continue
    fi
    printf '%s\n' "$cmd" >"$OUT/build/$name/$leg/.timecmd"
  done

  for leg in $LEGS; do
    cmdfile="$OUT/build/$name/$leg/.timecmd"
    [ -f "$cmdfile" ] || continue
    cmd=$(cat "$cmdfile")
    csv="$OUT/$name.$leg.csv"
    : >"$csv"
    n=$start
    points=0
    while [ "$points" -lt "$MAXPOINTS" ]; do
      t=$(time_min $cmd "$n") || {
        echo "   $leg: run failed at n=$n — series truncated here"
        break
      }
      echo "$n,$t" >>"$csv"
      points=$((points + 1))
      over=$(awk -v t="$t" -v b="$BUDGET" 'BEGIN { print (t > b) ? 1 : 0 }')
      [ "$over" = "1" ] && break
      n=$((n * 2))
    done
    echo "   $leg: $points point(s), last n=$n"
  done

  # Per-series scaling exponent (least-squares slope on log-log) and ratio to the js baseline at
  # the largest common n.
  {
    echo "$name:"
    for leg in $LEGS; do
      csv="$OUT/$name.$leg.csv"
      [ -s "$csv" ] || continue
      slope=$(awk -F, '{ x=log($1); y=log($2); sx+=x; sy+=y; sxx+=x*x; sxy+=x*y; c++ }
                       END { if (c>1) printf "%.2f", (c*sxy-sx*sy)/(c*sxx-sx*sx); else printf "n/a" }' "$csv")
      ratio="n/a"
      if [ "$leg" != "js" ] && [ -s "$OUT/$name.js.csv" ]; then
        ratio=$(awk -F, 'NR==FNR { js[$1]=$2; next } ($1 in js) { n=$1; r=$2/js[$1] }
                         END { if (n) printf "%.1fx @ n=%d", r, n; else printf "n/a" }' \
          "$OUT/$name.js.csv" "$csv")
      fi
      printf "  %-6s exponent=%-6s vs-js=%s\n" "$leg" "$slope" "$ratio"
    done
  } | tee -a "$summary"

  # log-log plot, one series per leg with data.
  gp="$OUT/$name.gp"
  {
    echo "set terminal pngcairo size 900,600"
    echo "set output '$OUT/$name.png'"
    echo "set logscale xy"
    echo "set datafile separator ','"
    echo "set title '$name — wall-clock vs input size (min of $REPS)'"
    echo "set xlabel 'n'; set ylabel 'seconds'"
    plots=""
    for leg in $LEGS; do
      [ -s "$OUT/$name.$leg.csv" ] || continue
      [ -n "$plots" ] && plots="$plots, "
      plots="$plots'$OUT/$name.$leg.csv' using 1:2 with linespoints title '$leg'"
    done
    [ -n "$plots" ] && echo "plot $plots"
  } >"$gp"
  gnuplot "$gp" 2>/dev/null || echo "   (gnuplot unavailable — CSVs written, plot skipped)"
done

[ ! -f "$OUT/.failed" ]
status=$?
rm -f "$OUT/.failed"
echo
echo "CSV/plots/summary under $OUT/"
exit $status
