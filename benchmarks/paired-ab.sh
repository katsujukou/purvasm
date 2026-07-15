#!/usr/bin/env bash
# A generic paired-interleaved-runs A/B driver: build the same benchmark program against two
# different runtime staticlibs (or, more generally, judge any two `--runtime-lib` builds against
# each other), self-check they agree, then measure them the way ADR-0102 §5 and
# sidenotes/0011-v1-gap-anatomy-post-0079.md's "measurement discipline" require — which
# `benchmarks/run-benchmarks.sh` (ADR-0075) does not do: that driver times one leg at a time across
# a size sweep, not two builds interleaved in one loop at a fixed size.
#
# Usage (from the repo root, inside `nix develop`):
#
#   ./benchmarks/paired-ab.sh --lib-a PATH --lib-b PATH --module Bench.Foo.Main --arg N \
#                             --heap-words WORDS [--label-a a] [--label-b b] [--reps 5]
#
# `--lib-a`/`--lib-b` are paths to a `libpurvasm_rt.a` (ADR-0071 §1's `--runtime-lib` lookup
# override, `boot/bin/main.ml`'s `resolve_runtime_lib`) — e.g. one built at a baseline git commit in
# a scratch `git worktree`, the other the current tree's `runtime/target/release/libpurvasm_rt.a`.
# Both legs are compiled from the SAME `purvm` binary, CoreFn/ulib inputs, and `--heap-words`/
# `--backend llvm` flags, so the runtime staticlib is the only variable. `--label-a`/`--label-b` are
# **display-only** — the internal build directories are the fixed `leg-a`/`leg-b`, never derived from
# the label, so passing the same label for both legs cannot make one leg's build silently overwrite
# the other's.
#
# Discipline (sidenotes/0011, "measurement discipline"): alternate the two binaries in one loop
# (min-of-K, default 5) rather than timing one leg fully then the other — this machine drifts
# between absolute-time states session-to-session, and interleaving is what makes a ratio trustworthy
# despite that. Wall-clock and the self-check are measured with `PURVASM_STATS`/`PURVASM_HEAP_WORDS`
# both explicitly cleared from the ambient environment (a stray export in the caller's shell must not
# silently change what "stats unset" / the baked-in `--heap-words` claim in the summary actually
# means); the counters snapshot is a SEPARATE pair of runs with `PURVASM_STATS=1` and
# `PURVASM_HEAP_WORDS` still cleared (ADR-0102 §5: never timed together with the runs above, and at
# the same heap size those runs used).
set -u

cd "$(dirname "$0")/.."

PURVM=./boot/_build/default/bin/main.exe
TIMER=benchmarks/time-run.pl
OUT=benchmarks/out/paired-ab

LIB_A=""
LIB_B=""
MODULE=""
ARG=""
HEAP_WORDS=""
LABEL_A="a"
LABEL_B="b"
REPS=5

while [ $# -gt 0 ]; do
  case "$1" in
    --lib-a) shift; LIB_A="$1" ;;
    --lib-b) shift; LIB_B="$1" ;;
    --module) shift; MODULE="$1" ;;
    --arg) shift; ARG="$1" ;;
    --heap-words) shift; HEAP_WORDS="$1" ;;
    --label-a) shift; LABEL_A="$1" ;;
    --label-b) shift; LABEL_B="$1" ;;
    --reps) shift; REPS="$1" ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
  shift
done

for name in LIB_A LIB_B MODULE ARG HEAP_WORDS; do
  eval "val=\${$name}"
  if [ -z "$val" ]; then
    echo "error: --$(echo "$name" | tr '[:upper:]' '[:lower:]' | tr '_' '-') is required" >&2
    exit 2
  fi
done

if [ ! -x "$PURVM" ]; then
  echo "error: $PURVM not found — run \`dune build\` in boot/ first" >&2
  exit 2
fi
for lib in "$LIB_A" "$LIB_B"; do
  [ -f "$lib" ] || { echo "error: runtime staticlib not found: $lib" >&2; exit 2; }
done

mkdir -p "$OUT"
summary="$OUT/summary.txt"
: >"$summary"

log() { printf '%s\n' "$1" | tee -a "$summary"; }

# Run a benchmark binary with NO ambient `PURVASM_STATS`/`PURVASM_HEAP_WORDS` interference — every
# measured invocation (self-check, timing, and the stats snapshot below, which re-adds `PURVASM_STATS`
# on top) goes through this, so a stray export in the caller's shell cannot silently invalidate the
# "stats unset" / baked-in "heap-words=$HEAP_WORDS" claims this script prints.
run_clean() {
  env -u PURVASM_STATS -u PURVASM_HEAP_WORDS "$@"
}

# Build one leg into a FIXED internal directory (`leg-a`/`leg-b`, never the display label — two legs
# sharing a label must not silently collide into one build). Prints the run-command prefix (the arg
# is appended by callers).
build_leg() {
  local legid=$1 lib=$2
  local bdir="$OUT/leg-$legid"
  mkdir -p "$bdir"
  "$PURVM" native --backend llvm --runtime-lib "$lib" --heap-words "$HEAP_WORDS" \
    --corefn-dir output --ulib dist/ulib -m "$MODULE" -o "$bdir" >"$bdir/build.log" 2>&1 \
    || { echo "error: build failed for leg $legid (see $bdir/build.log)" >&2; exit 1; }
  echo "$bdir/app"
}

# Capture exactly one `purvasm-stats:v1` line from a `PURVASM_STATS=1` run, fail-closed: a nonzero
# exit, zero matching lines, or more than one matching line are all errors, not "empty string and
# carry on" (ADR-0102 §3: exactly one line is emitted per freed context).
capture_stats() {
  local bin=$1 arg=$2 legid=$3
  local tmp
  tmp=$(mktemp)
  env -u PURVASM_HEAP_WORDS PURVASM_STATS=1 "$bin" "$arg" >/dev/null 2>"$tmp"
  local rc=$?
  local n
  n=$(grep -c '^purvasm-stats:v1 ' "$tmp")
  if [ "$rc" -ne 0 ]; then
    echo "error: stats run failed for leg $legid (exit $rc); stderr:" >&2
    cat "$tmp" >&2
    rm -f "$tmp"
    return 1
  fi
  if [ "$n" -ne 1 ]; then
    echo "error: expected exactly one purvasm-stats:v1 line from leg $legid, got $n; stderr:" >&2
    cat "$tmp" >&2
    rm -f "$tmp"
    return 1
  fi
  grep '^purvasm-stats:v1 ' "$tmp"
  rm -f "$tmp"
}

log "== paired-ab: $MODULE @ n=$ARG, heap-words=$HEAP_WORDS =="
log "  $LABEL_A: $LIB_A"
log "  $LABEL_B: $LIB_B"

bin_a=$(build_leg a "$LIB_A") || exit 1
bin_b=$(build_leg b "$LIB_B") || exit 1

# --- self-check: both legs must agree before any timing is trusted --------------------------------
out_a=$(run_clean "$bin_a" "$ARG" 2>/dev/null)
rc_a=$?
out_b=$(run_clean "$bin_b" "$ARG" 2>/dev/null)
rc_b=$?
if [ "$rc_a" -ne 0 ] || [ "$rc_b" -ne 0 ]; then
  log "SELF-CHECK: a run failed (rc $LABEL_A=$rc_a, rc $LABEL_B=$rc_b) — refusing to time a crash"
  exit 1
fi
if [ "$out_a" != "$out_b" ]; then
  log "SELF-CHECK DIVERGED: $LABEL_A='$out_a' vs $LABEL_B='$out_b' — refusing to time a wrong answer"
  exit 1
fi
log "self-check: $LABEL_A and $LABEL_B agree ($out_a)"

# --- interleaved wall-clock, stats disabled --------------------------------------------------------
raw="$OUT/interleaved.csv"
: >"$raw"
echo "rep,leg,seconds" >>"$raw"
best_a=""
best_b=""
i=1
while [ "$i" -le "$REPS" ]; do
  ta=$(perl "$TIMER" env -u PURVASM_STATS -u PURVASM_HEAP_WORDS "$bin_a" "$ARG") \
    || { log "  rep $i: $LABEL_A run failed"; i=$((i + 1)); continue; }
  echo "$i,$LABEL_A,$ta" >>"$raw"
  tb=$(perl "$TIMER" env -u PURVASM_STATS -u PURVASM_HEAP_WORDS "$bin_b" "$ARG") \
    || { log "  rep $i: $LABEL_B run failed"; i=$((i + 1)); continue; }
  echo "$i,$LABEL_B,$tb" >>"$raw"
  if [ -z "$best_a" ]; then best_a=$ta; else best_a=$(awk -v x="$best_a" -v y="$ta" 'BEGIN { print (y < x) ? y : x }'); fi
  if [ -z "$best_b" ]; then best_b=$tb; else best_b=$(awk -v x="$best_b" -v y="$tb" 'BEGIN { print (y < x) ? y : x }'); fi
  i=$((i + 1))
done

if [ -z "$best_a" ] || [ -z "$best_b" ]; then
  log "error: every timed rep failed for at least one leg — see $OUT/leg-*/build.log"
  exit 1
fi

ratio=$(awk -v a="$best_a" -v b="$best_b" 'BEGIN { if (b > 0) printf "%.3f", a / b; else print "n/a" }')
log ""
log "wall-clock (min of $REPS interleaved reps, PURVASM_STATS/PURVASM_HEAP_WORDS cleared):"
log "  $LABEL_A: ${best_a}s"
log "  $LABEL_B: ${best_b}s"
log "  ratio ($LABEL_A / $LABEL_B): ${ratio}x"

# --- separate stats-enabled snapshot (ADR-0102 §5: never timed together with the above) ------------
stats_a=$(capture_stats "$bin_a" "$ARG" a) || exit 1
stats_b=$(capture_stats "$bin_b" "$ARG" b) || exit 1
log ""
log "counters snapshot ($LABEL_A):"
log "  $stats_a"
log "counters snapshot ($LABEL_B):"
log "  $stats_b"

log ""
log "per-key delta ($LABEL_B - $LABEL_A):"
{
  echo "$stats_a" | tr ' ' '\n' | grep '=' | sed 's/^/A /'
  echo "$stats_b" | tr ' ' '\n' | grep '=' | sed 's/^/B /'
} | awk -F'[ =]' '
  $1=="A" { a[$2] = $3 }
  $1=="B" { b[$2] = $3; order[++n] = $2 }
  END { for (i = 1; i <= n; i++) { k = order[i]; d = b[k] - a[k]; printf "  %-28s %14s -> %14s  (%+d)\n", k, a[k], b[k], d } }
' | tee -a "$summary"

log ""
log "raw interleaved timings -> $raw"
log "summary -> $summary"
