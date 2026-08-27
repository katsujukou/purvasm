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
effect-ref      Bench.EffectRef.Main     1000   8388608
run-state-except Bench.RunStateExcept.Main 1000   33554432
st-ref          Bench.STRef.Main         1000   8388608
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

  # Compile <module> into <outdir> at the given optimisation mode (opt|noopt), without running it:
  # this harness runs each image itself, on the runner it is measuring.
  l2_compile() {
    local flag=""
    [ "$3" = "noopt" ] && flag="--no-opt"
    PURVASM_LIB="$ULIB" node "$L2_ENTRY" run --corefn-dir output --outdir "$2" --build-only -m "$1" $flag \
      >"$2/compile.log" 2>&1
  }
  # `purvm run --count` prints stats to stderr, the guest's result to stdout.
  instr_count() { "$PURVM" run --count "$1" "$2" 2>&1 1>/dev/null | awk '/instructions/{print $2}'; }
  guest_out() { "$PURVM" run "$1" "$2" 2>/dev/null; }

  # The owned VM (ADR-0110), run alongside boot's on the SAME compilation: `l2_compile` writes both
  # image forms, so `app.boot.pvm` (version 3) goes to boot and `app.pvm` goes here.
  #
  # What is compared is the **output**. The two runners were also held to equal instruction counts
  # while §4(a) was the only difference (§6 step C — taken, 8/8, and recorded in the ADR); §4(b)'s
  # tree dispatch then changed what a step is, so equal counts would no longer mean the same program
  # ran. The owned VM's own counts are the measurement field's baseline from here on.
  #
  # Opt-in by path because building the owned VM is a native link of minutes; when it is absent the
  # legs below say SKIPPED rather than passing quietly.
  OWNED_VM="${PURVASM_VM:-}"
  # The ulib leaves, as a loadable provider. A leaf like `Data.Number.isFinite` is shipped by ulib as
  # a `.c` and linked into a natively compiled program — the owned VM's guest has no such link, so it
  # reaches those keys the way ADR-0111 §4 says a workspace-provided key is reached: through a module
  # the runner loads. The whole corpus needs eight of them from two files, so one `.so` covers it.
  #
  # Built here rather than found, because nothing packages ulib's native side as a shared object yet;
  # ADR-0110 §6 step E owes that (a manifest beside the image, and the provider to go with it). Until
  # then this is the harness's own, and it uses the ordinary `--ffi` path with nothing special about it.
  OWNED_FFI=""
  if [ -n "$OWNED_VM" ]; then
    ULIB_C="$(node -e 'const m=require("fs").readFileSync(process.argv[1],"utf8");const f=JSON.parse(m).foreign||{};console.log([...new Set(Object.values(f))].join(" "))' "$ULIB/ulib.json" 2>/dev/null)"
    if [ -n "$ULIB_C" ]; then
      # shellcheck disable=SC2086
      set -- $ULIB_C
      abs=""
      for c in "$@"; do abs="$abs $ULIB/$c"; done
      OWNED_FFI="$OUT/ulib-provider.so"
      # A provider leaves every `pv_*` undefined and binds it against the host at `dlopen` — the same
      # link shape `tools/vm-loader-e2e.sh` uses, the ABI stamp `pv_foreign_abi_v1` included: its
      # unresolved reference is what makes a version mismatch fail the load (ADR-0111 §5).
      case "$(uname -s)" in
        Darwin) SHARED_FLAGS="-shared -undefined dynamic_lookup" ;;
        *)      SHARED_FLAGS="-shared -fPIC" ;;
      esac
      # shellcheck disable=SC2086
      if ! clang $SHARED_FLAGS -O2 -I "${PURVASM_INCLUDE:-runtime/include}" $abs -o "$OWNED_FFI" \
             >"$OUT/ulib-provider.log" 2>&1; then
        echo "error: could not build the ulib provider for the owned VM — see $OUT/ulib-provider.log" >&2
        exit 2
      fi
    fi
  fi
  # `--` separates the VM's flags from the guest's argv (the owned VM's convention; boot takes the
  # trailing arguments positionally). Both runners must hand the program the SAME size, or the two
  # legs measure different programs and their counts have nothing to say to each other.
  owned_count() { "$OWNED_VM" ${OWNED_FFI:+--ffi "$OWNED_FFI"} --image "$1" --count -- "$2" 2>&1 1>/dev/null | awk '/instructions/{print $2}'; }
  # Keeps the exit status: a REFUSED image and a diverging one are different findings, and a runner
  # that failed produces empty output — which any content-based diagnosis below would misread.
  owned_out() { "$OWNED_VM" ${OWNED_FFI:+--ffi "$OWNED_FFI"} --image "$1" -- "$2" 2>"$OUT/owned.err"; }
  # Whether the owned VM's guest reads the size at all: the same image run at two different sizes.
  # Identical output means the argument never reached the program — an argv-injection regression
  # rather than the two runners disagreeing. Asked of the owned VM alone, so no assumption about how
  # boot's flag parsing would treat a mimicked command line can creep into the attribution.
  owned_ignores_size() {
    [ "$(owned_out "$1" "$2")" = "$(owned_out "$1" "$3")" ]
  }
  now() { perl -MTime::HiRes=time -e 'printf "%.3f", time'; }
  pmo_bytes() { cat "$1"/_build/*.pmo 2>/dev/null | wc -c | tr -d ' '; }

  # The ADR-0089 §7 size/time gate: a silent optimiser blow-up the behavioural checks cannot see
  # must fail loudly. Thresholds carry headroom over the measured baseline (2026-07-11: emitted
  # `.pmo` size ratio ≈ 0.94–1.0 — the optimiser is a net shrink today — and compile wall time
  # ≈ 1.5–2.4× --no-opt, but wall-clock on ~1s node processes swings ±2× with startup noise — hence the wide time headroom); tighten or justify raising them with measurements.
  SIZE_RATIO_MAX=1.5
  TIME_RATIO_MAX=4.0

  optsum="$OUT/opt-effect.txt"
  if [ -n "$OWNED_VM" ]; then
    echo "owned VM: $OWNED_VM (running alongside boot on the same compilation)" | tee "$optsum"
  else
    echo "owned VM: SKIPPED (set PURVASM_VM to the built owned VM to run the ADR-0110 step-C leg)" | tee "$optsum"
  fi
  printf '%-16s %-10s %14s %14s %8s %7s %7s %7s %8s %14s %14s %8s\n' \
    bench n opt no-opt ratio 'red%' 'size×' 'time×' owned 'own-opt' 'own-no-opt' 'own-r' | tee -a "$optsum"
  echo "$BENCH_TABLE" | while read -r name module start heap; do
    [ -n "$name" ] || continue
    if [ -n "$ONLY" ] && ! echo "$ONLY" | tr ' ' '\n' | grep -qx "$name"; then continue; fi
    odir="$OUT/opt/$name/opt"
    ndir="$OUT/opt/$name/noopt"
    # Fresh outdirs: pmo_bytes sums the whole `_build`, so a `.pmo` left by an earlier run (or
    # another branch's closure shape) would silently pollute the size gate.
    rm -rf "$odir" "$ndir"
    mkdir -p "$odir" "$ndir"
    # Warm the page cache (the closure's corefn.json reads) with an untimed compile: whichever
    # mode runs first otherwise pays the cold I/O alone, biasing the time× ratio. The timed
    # measurement is min-of-2 per mode (the ADR-0075 noise discipline) — single-shot wall clock on
    # ~1s node processes swings past the gate on outliers.
    l2_compile "$module" "$ndir" noopt >/dev/null 2>&1 || true
    ot=""
    nt=""
    cfail=0
    for _rep in 1 2; do
      t0=$(now)
      l2_compile "$module" "$odir" opt || cfail=1
      t1=$(now)
      l2_compile "$module" "$ndir" noopt || cfail=1
      t2=$(now)
      [ "$cfail" = "1" ] && break
      od=$(awk -v a="$t0" -v b="$t1" 'BEGIN { printf "%.3f", b - a }')
      nd=$(awk -v b="$t1" -v c="$t2" 'BEGIN { printf "%.3f", c - b }')
      if [ -z "$ot" ]; then ot=$od; else ot=$(awk -v x="$ot" -v y="$od" 'BEGIN { print (y < x) ? y : x }'); fi
      if [ -z "$nt" ]; then nt=$nd; else nt=$(awk -v x="$nt" -v y="$nd" 'BEGIN { print (y < x) ? y : x }'); fi
    done
    if [ "$cfail" = "1" ]; then
      printf '%-16s %-10s %14s\n' "$name" "$start" "COMPILE-FAILED" | tee -a "$optsum"
      touch "$OUT/.opt-failed"
      continue
    fi
    # Correctness: --opt must not change the observable output, and must equal --no-opt.
    oo=$(guest_out "$odir/app.boot.pvm" "$start")
    no=$(guest_out "$ndir/app.boot.pvm" "$start")
    if [ -z "$oo" ] || [ "$oo" != "$no" ]; then
      printf '%-16s %-10s %14s (opt=%s / noopt=%s)\n' "$name" "$start" "RUN-FAILED/DIVERGED" "$oo" "$no" | tee -a "$optsum"
      touch "$OUT/.opt-failed"
      continue
    fi
    oi=$(instr_count "$odir/app.boot.pvm" "$start")
    ni=$(instr_count "$ndir/app.boot.pvm" "$start")
    ratio=$(awk -v n="$ni" -v o="$oi" 'BEGIN { if (o > 0) printf "%.3f", n / o; else printf "n/a" }')
    red=$(awk -v n="$ni" -v o="$oi" 'BEGIN { if (n > 0) printf "%.1f", (n - o) * 100 / n; else printf "n/a" }')
    # The size/time gate (ADR-0089 §7). Zero emitted bytes on either side means the compile did
    # not actually produce a closure into the fresh outdir — a harness bug, not a measurement.
    os=$(pmo_bytes "$odir")
    ns=$(pmo_bytes "$ndir")
    if [ "$os" = "0" ] || [ "$ns" = "0" ]; then
      printf '%-16s %-10s %14s (opt=%sB / noopt=%sB)\n' "$name" "$start" "NO-PMO-EMITTED" "$os" "$ns" | tee -a "$optsum"
      touch "$OUT/.opt-failed"
      continue
    fi
    sizer=$(awk -v a="$os" -v b="$ns" 'BEGIN { if (b > 0) printf "%.3f", a / b; else printf "n/a" }')
    timer=$(awk -v o="$ot" -v n="$nt" 'BEGIN { if (n > 0) printf "%.3f", o / n; else printf "n/a" }')
    gate=""
    if awk -v r="$sizer" -v m="$SIZE_RATIO_MAX" 'BEGIN { exit !(r > m) }'; then
      gate=" SIZE-GATE-EXCEEDED(>$SIZE_RATIO_MAX)"
      touch "$OUT/.opt-failed"
    fi
    if awk -v r="$timer" -v m="$TIME_RATIO_MAX" 'BEGIN { exit !(r > m) }'; then
      gate="$gate TIME-GATE-EXCEEDED(>$TIME_RATIO_MAX)"
      touch "$OUT/.opt-failed"
    fi
    # The ADR-0110 step-C leg. Both modes are checked: `--opt` and `--no-opt` reach the leaf lowering
    # by different routes, and only one of them running the optimiser is exactly the asymmetry that
    # would hide a divergence in the other.
    owned="-"
    if [ -n "$OWNED_VM" ]; then
      owned="≡"
      for m in opt noopt; do
        [ "$m" = "opt" ] && d="$odir" || d="$ndir"
        [ "$m" = "opt" ] && bo="$oo" || bo="$no"
        if [ ! -f "$d/app.pvm" ]; then
          owned="NO-IMAGE($m)"; touch "$OUT/.opt-failed"; break
        fi
        if ! vo=$(owned_out "$d/app.pvm" "$start"); then
          owned="REFUSED($m)"; touch "$OUT/.opt-failed"
          sed 's/^/    /' "$OUT/owned.err" >>"$optsum"
          break
        fi
        if [ "$vo" != "$bo" ]; then
          # Name the cause when it is a known one rather than reporting a bare mismatch. The corpus
          # reads its input size from the guest's argv (ADR-0075 §4); if the owned run is insensitive
          # to a doubled size, the guest never received one and the argv injection has regressed —
          # which is a different bug from the two runners disagreeing about the program.
          if owned_ignores_size "$d/app.pvm" "$start" "$((start * 2))"; then
            owned="ARGV($m)"
          else
            owned="OUT($m)"
          fi
          touch "$OUT/.opt-failed"; break
        fi
      done
      # The owned VM's OWN counts: the measurement field's baseline from §4(b) on (ADR-0110 §6's
      # pinned terms). Reported, not compared — boot's numbers describe a different instruction
      # vocabulary now, and the ratio here is the one an optimiser change should move.
      if [ "$owned" = "≡" ]; then
        voi=$(owned_count "$odir/app.pvm" "$start")
        vni=$(owned_count "$ndir/app.pvm" "$start")
        oratio=$(awk -v n="$vni" -v o="$voi" 'BEGIN { if (o > 0) printf "%.3f", n / o; else printf "n/a" }')
      else
        voi="-"; vni="-"; oratio="-"
      fi
    else
      voi="-"; vni="-"; oratio="-"
    fi
    printf '%-16s %-10s %14s %14s %8s %7s %7s %7s %8s %14s %14s %8s%s\n' \
      "$name" "$start" "$oi" "$ni" "$ratio" "$red" "$sizer" "$timer" "$owned" "$voi" "$vni" "$oratio" "$gate" | tee -a "$optsum"
  done
  # The self-compile size leg (ADR-0089 self-compile extension): the compiler's own closure is
  # the regression gate for the inliner blow-up class the five bench closures cannot see
  # (measured before the fix: whole-closure 1.666, CST.Parser x14.5). Runs on the DEFAULT
  # node stack: the middle-end stack-safety item closed 2026-07-16 (Normalize and the Match
  # assembler loop over data-sized spines instead of stacking frames), so this leg now
  # doubles as the default-stack completion assert the earlier diagnostic posture promised.
  if [ -z "$ONLY" ]; then
    sdir="$OUT/opt/self-compile"
    rm -rf "$sdir"
    mkdir -p "$sdir/opt" "$sdir/noopt"
    sc_ok=1
    PURVASM_LIB="$ULIB" node "$L2_ENTRY" run --corefn-dir output \
      --outdir "$sdir/noopt" --build-only -m Purvasm.CLI.Main --no-opt >"$sdir/noopt.log" 2>&1 || sc_ok=0
    PURVASM_LIB="$ULIB" node "$L2_ENTRY" run --corefn-dir output \
      --outdir "$sdir/opt" --build-only -m Purvasm.CLI.Main >"$sdir/opt.log" 2>&1 || sc_ok=0
    if [ "$sc_ok" = "0" ]; then
      printf '%-16s %-10s %14s\n' "self-compile" "-" "COMPILE-FAILED" | tee -a "$optsum"
      touch "$OUT/.opt-failed"
    else
      sso=$(pmo_bytes "$sdir/opt")
      ssn=$(pmo_bytes "$sdir/noopt")
      if [ "$sso" = "0" ] || [ "$ssn" = "0" ]; then
        printf '%-16s %-10s %14s\n' "self-compile" "-" "NO-PMO-EMITTED" | tee -a "$optsum"
        touch "$OUT/.opt-failed"
      else
        ssr=$(awk -v a="$sso" -v b="$ssn" 'BEGIN { printf "%.3f", a / b }')
        sgate=""
        if awk -v r="$ssr" -v m="$SIZE_RATIO_MAX" 'BEGIN { exit !(r > m) }'; then
          sgate=" SIZE-GATE-EXCEEDED(>$SIZE_RATIO_MAX)"
          touch "$OUT/.opt-failed"
        fi
        printf '%-16s %-10s %14s %14s %8s %7s %7s %7s%s\n' "self-compile" "-" "$sso" "$ssn" "-" "-" "$ssr" "-" "$sgate" | tee -a "$optsum"
      fi
    fi
  fi
  echo
  echo "opt-effect summary → $optsum  (ratio = noopt/opt instructions; size×/time× = opt/noopt emitted-.pmo bytes and compile wall time, gated at $SIZE_RATIO_MAX/$TIME_RATIO_MAX per ADR-0089 §7)"
  # ratio bar chart next to the 4-way wall-clock PNGs. Note the scope difference: those graphs are
  # boot-compiled legs; only this plot (and the table above) shows the Level-2 optimiser's effect.
  gp="$OUT/opt-effect.gp"
  {
    echo "set terminal pngcairo size 900,480"
    echo "set output '$OUT/opt-effect.png'"
    echo "set title 'Level-2 optimiser effect — VM instruction ratio (--no-opt / --opt), higher is better'"
    echo "set style data histogram"
    echo "set style fill solid 0.8 border -1"
    echo "set boxwidth 0.6"
    echo "set yrange [0:*]"
    echo "set grid ytics"
    echo "set ylabel 'ratio'"
    echo "plot '$optsum' every ::1 using 5:xtic(1) notitle, \\"
    echo "     '' every ::1 using 0:5:5 with labels offset 0,0.6 notitle"
  } >"$gp"
  gnuplot "$gp" 2>/dev/null && echo "opt-effect plot → $OUT/opt-effect.png" \
    || echo "   (gnuplot unavailable — table written, plot skipped)"
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
