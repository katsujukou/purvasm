#!/usr/bin/env bash
# ADR-0108 §3 dynamic apply profile + its reconciliation gate.
#
# The static census (`tools/apply-census.sh`) ranks call SITES. This ranks EXECUTIONS: the same
# classifier that labels a site at emission time bumps a per-`(form × reason)` counter at run time,
# so the two rankings are the same partition measured against different weights. They are expected
# to disagree — a rare site in a hot loop outweighs a thousand cold ones — and that disagreement is
# the reason this step exists (§3: "static ranks code, not execution").
#
# What makes the dynamic numbers trustworthy is that they are checked against counters the runtime
# was already keeping for its own reasons, and the check is EXACT — not a tolerance:
#
#   Σ generic-apply/<reason> + structural-apply == pv_apply_entries    (every generic dispatch that
#                                                                       actually entered pv_apply)
#   Σ generic-tail/<reason>                     == pv_tailcall_writes  (every trampoline store)
#
# An off-by-anything means the instrumentation and the emitter disagree about what was emitted —
# a mis-slotted event, a bump on a path with no call, or a call on a path with no bump. Two
# independently-derived numbers landing on the same integer is the whole assurance argument, so
# neither side may be relaxed into a range.
#
# The gate also asserts what instrumentation must NOT do: an instrumented run's output must equal
# the uninstrumented one. A profile that perturbs the program it measures is measuring a different
# program.
#
# TWO WORKLOADS, and the difference matters:
#
#   fixtures (default)  — behavioural fixtures; fast, and the identities are gated per fixture.
#                         Their ranking describes THOSE programs, not the compiler.
#   --selfhost          — THE workload this ADR is about: the compiler itself, built instrumented,
#                         compiling the same pinned closure `apply-census.sh` censused. Only this
#                         leg's ranking may be compared against the static one — same corpus, same
#                         emitter options, sites vs executions being the only difference.
#
# The `--selfhost` leg has TWO mode axes and they are not the same axis:
#
#   --build-mode  how the INSTRUMENTED COMPILER was compiled → decides which call sites exist in the
#                 running binary, i.e. the corpus. Must match the census's mode (`--opt`) for the
#                 site-vs-execution comparison to be about weights rather than about two corpora.
#   --work-mode   the mode the running compiler compiles its workload in → decides the execution
#                 weights. Defaults per leg: `--no-opt` under `--selfhost` (the fixpoint gate's
#                 `smoke` profile — a native `--opt` whole-closure compile is still under the
#                 ADR-0104 §2 waiver), `--opt` for fixtures (the shipped path, which they can run).
#                 The mode moves the ranking a long way, so a reported ranking must name it.
#
# Usage (from the repo root, inside `nix develop`):
#   tools/apply-profile.sh [MODULE ...]
#   tools/apply-profile.sh --selfhost [--build-mode opt|no-opt] [--work-mode opt|no-opt]
#   tools/apply-profile.sh --self-test    (assert the drill gate fails on injected faults; no build)
#
# Prerequisites (located, not built): the staged ulib (`dist/ulib`), the RELEASE runtime staticlib
# (or $PURVASM_RT_A), `clang`, `node`, and fixture CoreFn in `output/` (workspace `spago build`).
set -uo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."
ROOT="$PWD"

: "${PURVASM_RT_A:=$ROOT/runtime/target/release/libpurvasm_rt.a}"
: "${PURVASM_LIB:=$ROOT/dist/ulib}"

SELFHOST=0
SELF_TEST=0
BUILD_FLAG=            # --opt
WORK_FLAG=
WORK_MODE_SET=0
ENTRY_MODULE=Purvasm.CLI.Native
ENTRY_NAME=main
MODULES=

mode_flag() { case "$1" in opt) echo "" ;; no-opt) echo "--no-opt" ;;
  *) echo "apply-profile.sh: mode must be opt|no-opt, got $1" >&2; exit 2 ;; esac; }

while [ $# -gt 0 ]; do
  case "$1" in
    --selfhost) SELFHOST=1; shift ;;
    --self-test) SELF_TEST=1; shift ;;
    --build-mode) BUILD_FLAG=$(mode_flag "$2") || exit 2; shift 2 ;;
    --work-mode) WORK_FLAG=$(mode_flag "$2") || exit 2; WORK_MODE_SET=1; shift 2 ;;
    --entry) ENTRY_MODULE="$2"; shift 2 ;;
    -*) echo "apply-profile.sh: unknown argument $1" >&2; exit 2 ;;
    *) MODULES="$MODULES $1"; shift ;;
  esac
done

# The work-mode default is per-leg, because the two legs are constrained differently: a native
# whole-closure `--opt` compile is the profile still under the ADR-0104 §2 waiver, while the
# fixtures compile happily in `--opt` — which is the shipped path, so it is what their ranking
# should describe. An explicit --work-mode always wins.
if [ "$WORK_MODE_SET" = "0" ]; then
  if [ "$SELFHOST" = "1" ]; then WORK_FLAG=--no-opt; else WORK_FLAG=; fi
fi

# The dispatch-heavy fixtures by default: the population this census is about is generic dispatch,
# so a corpus of straight-line arithmetic would report a green gate over nothing.
[ -n "$MODULES" ] || MODULES="Gate.DictDispatch Gate.Mixed Gate.GcChurn Gate.ByNeedCell"

# Token-bounded numeric extraction from a named schema line; empty/non-numeric → "".
field_of() { # $1=file  $2=schema prefix  $3=field name
  local v
  v="$(grep "^$2 " "$1" | tr ' ' '\n' | sed -n "s/^$3=\([0-9][0-9]*\)\$/\1/p")"
  case "$v" in *[!0-9]* | "") echo "" ;; *) echo "$v" ;; esac
}

# ADR-0108 §4: the drill's keys, `<key>\t<count>`. Absent when nothing drilled ran — which is a
# fact about the run, not an error, so this does not fail on an empty line.
drill_of() { # $1=stderr file  $2=out tsv
  : >"$2"
  grep '^purvasm-applyprofile-keys:v1 ' "$1" | tr ' ' '\n' \
    | sed -n 's/^\([^=]*\)=\([0-9][0-9]*\)$/\1\t\2/p' >>"$2"
}

# The §4 cross-mechanism identity: the keyed counters and the fixed slots are written by different
# code down different paths, so their agreement is evidence, not arithmetic.
#
#   Σ keys == slot[generic-apply/callee-foreign] + slot[generic-tail/callee-foreign]
#
# A drilled dispatch that bumped a slot but no key (or vice versa) breaks it — which is exactly the
# mistake a per-site emission is prone to, since the two bumps are emitted by different call sites
# in the emitter.
reconcile_drill() { # $1=slots tsv  $2=keys tsv
  local slots keys
  slots=$(awk -F'\t' '$1 ~ /\/callee-foreign$/ { s += $2 } END { print s + 0 }' "$1")
  keys=$(awk -F'\t' '{ s += $2 } END { print s + 0 }' "$2")
  if [ "$slots" = "$keys" ]; then echo "$keys == $slots"; else echo "$keys != $slots"; return 1; fi
}

# Parse the profile line of a run into `<slot>\t<count>`; fails if it is absent or duplicated.
slots_of() { # $1=stderr file  $2=out tsv
  local n
  n="$(grep -c '^purvasm-applyprofile:v1 ' "$1" || true)"
  [ "$n" = "1" ] || { echo "PROFILE-SCHEMA(x$n)"; return 1; }
  grep '^purvasm-applyprofile:v1 ' "$1" | tr ' ' '\n' \
    | sed -n 's/^\([a-z-]*\/*[a-z-]*\)=\([0-9][0-9]*\)$/\1\t\2/p' >"$2"
  return 0
}

# The two identities. Echoes two verdict strings; returns non-zero if either fails.
reconcile() { # $1=slots tsv  $2=stderr file
  local sum_apply sum_tail rt_apply rt_tail rcl=0
  sum_apply=$(awk -F'\t' 'index($1,"generic-apply/")==1 || $1=="structural-apply" { s += $2 } END { print s+0 }' "$1")
  sum_tail=$(awk -F'\t' 'index($1,"generic-tail/")==1 { s += $2 } END { print s+0 }' "$1")
  rt_apply="$(field_of "$2" 'purvasm-stats:v1' pv_apply_entries)"
  rt_tail="$(field_of "$2" 'purvasm-stats:v1' pv_tailcall_writes)"
  if [ -z "$rt_apply" ] || [ -z "$rt_tail" ]; then
    echo "STATS-SCHEMA"; echo "STATS-SCHEMA"; return 1
  fi
  # A run that dispatched nothing reconciles vacuously.
  if [ "$rt_apply" -eq 0 ] && [ "$rt_tail" -eq 0 ]; then
    echo "VACUOUS(0)"; echo "VACUOUS(0)"; return 1
  fi
  if [ "$sum_apply" = "$rt_apply" ]; then echo "$sum_apply == $rt_apply"; else echo "$sum_apply != $rt_apply"; rcl=1; fi
  if [ "$sum_tail" = "$rt_tail" ]; then echo "$sum_tail == $rt_tail"; else echo "$sum_tail != $rt_tail"; rcl=1; fi
  return "$rcl"
}

ranking() { # $1=slots tsv (aggregated: slot, count)
  echo
  printf '%-42s %14s %8s\n' "slot" "executions" "share"
  awk -F'\t' '{ n[$1] += $2; tot += $2 }
    END { for (s in n) if (n[s] > 0) printf "%-42s %14d %7.1f%%\n", s, n[s], 100 * n[s] / tot }' "$1" \
    | sort -k2 -rn
  echo
  printf '%-24s %14s %8s\n' "reason (both forms)" "executions" "share"
  awk -F'\t' '{ split($1, p, "/"); if (p[1] == "structural-apply") next; r[p[2]] += $2; tot += $2 }
    END { for (k in r) printf "%-24s %14d %7.1f%%\n", k, r[k], 100 * r[k] / tot }' "$1" \
    | sort -k2 -rn
}

# --- self-test of the drill gate ----------------------------------------------------------------
# A gate is only worth its runtime if it FAILS on the thing it claims to catch. The case that
# motivates this one — key emission or the third schema line vanishing entirely — produces an empty
# key file, which an earlier version of this script reported as "nothing to check". So the failure
# modes are injected here and the gate's verdict asserted.
#
# It runs BEFORE the prerequisite checks, the toolchain build and the snapshot, and uses its own
# temp dir: the whole test is TSV fixtures against the real `reconcile_drill`, so requiring a built
# compiler, a staged ulib or a runtime staticlib to run it would be a cost with no coverage —
# and would stop it being usable as a cheap standing regression test.
if [ "$SELF_TEST" = "1" ]; then
  t="$(mktemp -d)"; trap 'rm -rf "$t"' EXIT; st_rc=0
  check() { # $1=label  $2=expected pass|fail  $3=slots content  $4=keys content
    printf '%b' "$3" >"$t/slots.tsv"; printf '%b' "$4" >"$t/keys.tsv"
    if reconcile_drill "$t/slots.tsv" "$t/keys.tsv" >/dev/null 2>&1; then got=pass; else got=fail; fi
    if [ "$got" = "$2" ]; then printf '  ok    %-46s (%s)\n' "$1" "$got"
    else printf '  FAIL  %-46s (expected %s, got %s)\n' "$1" "$2" "$got"; st_rc=1; fi
  }
  echo "== self-test: the drill gate ======================================="
  check "keys agree with slots"            pass "generic-apply/callee-foreign\t5\n" "a|apply|known-match\t5\n"
  check "keys short of slots"              fail "generic-apply/callee-foreign\t5\n" "a|apply|known-match\t4\n"
  check "SCHEMA GONE: no keys, live slots" fail "generic-apply/callee-foreign\t5\n" ""
  check "no foreign at all: 0 vs 0"        pass "generic-apply/local-unknown-fn\t9\n" ""
  check "both forms summed"                pass "generic-apply/callee-foreign\t5\ngeneric-tail/callee-foreign\t2\n" "a|apply|known-match\t5\nb|tail|known-match\t2\n"
  check "a non-foreign slot is not counted" fail "generic-apply/callee-foreign\t5\ngeneric-apply/local-unknown-fn\t9\n" "a|apply|known-match\t14\n"
  [ "$st_rc" -eq 0 ] && echo "OK: the drill gate fails on every injected fault" ||
    echo "FAIL: the drill gate did not catch an injected fault" >&2
  exit "$st_rc"
fi

for tool in "$PURVASM_RT_A" "$PURVASM_LIB" "$ROOT/output/$ENTRY_MODULE/corefn.json"; do
  [ -e "$tool" ] || { echo "missing prerequisite: $tool" >&2; exit 2; }
done
for cmd in node clang; do
  command -v "$cmd" >/dev/null || { echo "missing prerequisite: $cmd on PATH (run inside nix develop)" >&2; exit 2; }
done

WORK="${PROFILE_WORK:-$ROOT/_build/apply-profile}"
rm -rf "$WORK"; mkdir -p "$WORK"

# --- pin the inputs ---------------------------------------------------------------------------
# Every leg runs from the snapshot, never from the tree. Without this a concurrent `spago build`
# or ulib re-stage between legs leaves each fixture's identity green while the AGGREGATE ranking
# mixes runs of different programs — and the aggregate is what gets reported. `output/` is BOTH the
# compiler's own compiled JS and the default CoreFn closure, so it is snapshotted once and used as
# both (the same trap `byneed-census.sh` documents).
# The CLASSIFIER is an input too, and this harness runs for HOURS: the census that produces the
# site numbers must be the same compiler that produced the profiled binary, not whatever the tree
# holds by the time the earlier legs finish. So everything is built ONCE here, snapshotted ONCE
# below, and the census leg is handed that snapshot (`--toolchain`) instead of rebuilding.
echo "== building the toolchain once (compiler + census) =================="
spago build -p census >"$WORK/spago.log" 2>&1 ||
  { echo "apply-profile.sh: spago build failed; see $WORK/spago.log" >&2; exit 1; }

echo "== snapshotting inputs (compiler JS, CoreFn, wrappers, ulib, rt .a, traces) ="
cp -R "$ROOT/output" "$WORK/output"
mkdir -p "$WORK/cli" "$WORK/census" "$WORK/rt"
cp "$ROOT/cli/index.node.js" "$WORK/cli/index.node.js"
cp "$ROOT/census/index.js" "$WORK/census/index.js"
cp -R "$PURVASM_LIB" "$WORK/ulib"
cp "$PURVASM_RT_A" "$WORK/rt/libpurvasm_rt.a"
cp -R "$ROOT/test-fixtures/l2-behavioural/expected" "$WORK/expected"
export PURVASM_LIB="$WORK/ulib"
export PURVASM_RT_A="$WORK/rt/libpurvasm_rt.a"
CLI="$WORK/cli/index.node.js"
COREFN="$WORK/output"

# Measurement knobs are harness-owned. An ambient PURVASM_PROFILE_APPLY=1 would make the
# "uninstrumented" reference instrumented too, so the comparison would be against itself; an ambient
# PURVASM_BYNEED_OFF=1 is worse in a subtler way — the profiled legs would measure a lattice-OFF
# compiler while the census leg (which unsets it) described a lattice-ON one, so the two halves of
# the "one corpus" table would come from different emitters, and neither would be the shipped path.
unset PURVASM_PROFILE_APPLY PURVASM_BYNEED_OFF PURVASM_EMIT_DEBUG_ABI PURVASM_GC_STRESS PURVASM_STATS PURVASM_HEAP_WORDS

# The running binaries' heap, pinned so both legs of any comparison see one allocator regime.
: "${PROFILE_HEAP_WORDS:=134217728}"

rc=0


if [ "$SELFHOST" = "1" ]; then
  BUILD_LABEL=${BUILD_FLAG:---opt}; WORK_LABEL=${WORK_FLAG:---opt}
  echo "== --selfhost: compiler built ${BUILD_LABEL/--/} , compiling $ENTRY_MODULE ${WORK_LABEL/--/} ="
  echo "   (this is the ADR's headline workload; both legs are whole-closure compiles)"

  # leg 1: the reference — the node-hosted compiler emits the closure, uninstrumented.
  echo "== leg 1/4: reference emission (node-hosted, uninstrumented) ========"
  # shellcheck disable=SC2086
  node "$CLI" build --corefn-dir "$COREFN" --entry "$ENTRY_MODULE" --entry-name "$ENTRY_NAME" \
    --outdir "$WORK/ref" --emit-llvm $WORK_FLAG >"$WORK/ref.log" 2>&1 ||
    { echo "apply-profile.sh: reference leg failed; see $WORK/ref.log" >&2; exit 1; }

  # leg 2: build the compiler itself, instrumented.
  echo "== leg 2/4: building the instrumented compiler (long) ==============="
  # shellcheck disable=SC2086
  PURVASM_PROFILE_APPLY=1 node "$CLI" build --corefn-dir "$COREFN" \
    --entry "$ENTRY_MODULE" --entry-name "$ENTRY_NAME" \
    --outdir "$WORK/compiler" $BUILD_FLAG >"$WORK/compiler.log" 2>&1 ||
    { echo "apply-profile.sh: instrumented compiler build failed; see $WORK/compiler.log" >&2; exit 1; }
  [ -x "$WORK/compiler/app" ] ||
    { echo "apply-profile.sh: no instrumented binary at $WORK/compiler/app" >&2; exit 1; }

  # leg 3: run it over the SAME closure, and profile that.
  echo "== leg 3/4: instrumented compiler compiles the closure (long) ======="
  # shellcheck disable=SC2086
  PURVASM_STATS=1 PURVASM_HEAP_WORDS="$PROFILE_HEAP_WORDS" \
    "$WORK/compiler/app" build --corefn-dir "$COREFN" --entry "$ENTRY_MODULE" \
    --entry-name "$ENTRY_NAME" --outdir "$WORK/prof" --emit-llvm $WORK_FLAG \
    >"$WORK/prof.log" 2>"$WORK/prof.err" ||
    { echo "apply-profile.sh: profiled run failed; see $WORK/prof.err" >&2; exit 1; }

  # Behaviour-neutrality for THIS workload: the program's output is the emitted `.ll` set, so the
  # instrumented compiler must emit exactly what the reference did. (This also re-asserts the
  # node/native emission equality the fixpoint gate owns — if it ever diverges, this leg says so
  # rather than silently profiling a different program.)
  echo "== reconciliation =================================================="
  if diff -r "$WORK/ref/_build" "$WORK/prof/_build" >"$WORK/emission.diff" 2>&1; then
    echo "OK: emitted .ll set identical to the uninstrumented reference ($(find "$WORK/prof/_build" -name '*.ll' | wc -l | tr -d ' ') objects)"
  else
    echo "FAIL: instrumented compiler emitted DIFFERENT artifacts (see $WORK/emission.diff)" >&2
    head -5 "$WORK/emission.diff" >&2
    rc=1
  fi

  if ! verdicts="$(slots_of "$WORK/prof.err" "$WORK/slots.tsv")"; then
    echo "FAIL: $verdicts" >&2; exit 1
  fi
  # bash 3.2 (macOS) has no `mapfile` — read the two verdict lines positionally.
  verd="$(reconcile "$WORK/slots.tsv" "$WORK/prof.err")" || rc=1
  v0="$(echo "$verd" | sed -n 1p)"
  v1="$(echo "$verd" | sed -n 2p)"
  printf 'Σ generic-apply + structural vs pv_apply_entries : %s\n' "$v0"
  printf 'Σ generic-tail               vs pv_tailcall_writes: %s\n' "$v1"
  case "$v0$v1" in *"!="* | *VACUOUS* | *SCHEMA*) rc=1 ;; esac

  ranking "$WORK/slots.tsv"

  # --- ADR-0108 §4: the drill -------------------------------------------------------------------
  # The reconciliation is UNCONDITIONAL. Skipping it when no keys were parsed would make the gate
  # vacuous exactly when it matters most: if key emission, the third schema line, or the parse
  # regressed away entirely, an empty file would be reported as "nothing to check" while the slots
  # still counted hundreds of millions of foreign dispatches. Absent keys are Σ = 0, and 0 is only
  # correct against a slot total of 0.
  drill_of "$WORK/prof.err" "$WORK/keys.tsv"
  dverd="$(reconcile_drill "$WORK/slots.tsv" "$WORK/keys.tsv")" || rc=1
  echo
  printf 'Σ drill keys vs the callee-foreign slots        : %s\n' "$dverd"
  case "$dverd" in *"!="*) rc=1 ;; esac

  if [ -s "$WORK/keys.tsv" ]; then
    echo
    echo "== the drill: foreign dispatches by (symbol × form × arity status) =="
    printf '%-52s %14s %8s\n' "key" "executions" "share"
    sort -k2 -rn -t $'\t' "$WORK/keys.tsv" \
      | awk -F'\t' -v tot="$(awk -F'\t' '{s+=$2} END {print s+0}' "$WORK/keys.tsv")" \
          'NR <= 25 { printf "%-52s %14d %7.1f%%\n", $1, $2, 100 * $2 / tot }'
    printf '(top 25 of %s keys)\n' "$(wc -l <"$WORK/keys.tsv" | tr -d ' ')"

    # The number the ADR turns on: how much of the foreign mass is at an arity the emitter already
    # knows AND that matches the call. That is the population a direct lowering could capture; the
    # rest cannot be captured by that lever no matter how it is written.
    echo
    printf '%-16s %14s %8s\n' "arity status" "executions" "share"
    awk -F'\t' '{ n = split($1, p, "|"); st[p[n]] += $2; tot += $2 }
      END { for (k in st) printf "%-16s %14d %7.1f%%\n", k, st[k], 100 * st[k] / tot }' \
      "$WORK/keys.tsv" | sort -k2 -rn
  else
    echo
    echo "NOTE: no drill keys recorded — reconciled above as 0, which passes only against 0 slots."
  fi

  # --- the static census, over THIS run's snapshot ----------------------------------------------
  # The site-vs-execution comparison is only meaningful if both sides describe the same corpus, and
  # "both harnesses snapshot `output/`" does NOT give that: they snapshot at different times, and a
  # `spago build` in between changes the compiler's own CoreFn (verified: 85 files differed between
  # two such snapshots). So the census is run HERE, from `$COREFN` — the very bytes the profiled
  # compiler was built from — and in `--build-mode`, because the sites that exist in the running
  # binary are the sites of the compiler as it was BUILT, not of the workload it compiles.
  #
  # `apply-census.sh` is invoked rather than reimplemented: it owns the six-column accounting gate
  # and the reason-axis gate, so the site numbers arrive gated rather than merely produced.
  echo
  echo "== leg 4/4: static census, SAME snapshot + SAME toolchain ($BUILD_LABEL) ="
  if APPLY_WORK="$WORK/census-work" "$ROOT/tools/apply-census.sh" "$BUILD_LABEL" \
      --toolchain "$WORK" --corefn-dir "$COREFN" \
      --entry "$ENTRY_MODULE" --entry-name "$ENTRY_NAME" \
      --out sites.tsv >"$WORK/census.log" 2>&1; then
    echo "OK: census gates green (see $WORK/census.log)"
  else
    echo "FAIL: static census leg failed or its gate is red (see $WORK/census.log)" >&2
    tail -5 "$WORK/census.log" >&2
    rc=1
  fi

  if [ -f "$WORK/census-work/sites.tsv" ]; then
    # sites per reason (both forms), from the census's `reason` rows
    awk -F'\t' '/^#/ { next } $3 == "reason" { split($4, p, "/"); s[p[2]] += $5 }
      END { for (k in s) printf "%s\t%d\n", k, s[k] }' "$WORK/census-work/sites.tsv" | sort >"$WORK/sites-by-reason.tsv"
    # executions per reason (both forms), from this run's slots
    awk -F'\t' '{ split($1, p, "/"); if (p[1] == "structural-apply") next; e[p[2]] += $2 }
      END { for (k in e) printf "%s\t%d\n", k, e[k] }' "$WORK/slots.tsv" | sort >"$WORK/execs-by-reason.tsv"

    echo
    echo "== ONE corpus, two measurements ===================================="
    printf '%-22s %10s %8s %16s %8s %8s\n' "reason" "sites" "share" "executions" "share" "exec/site"
    join -t $'\t' -a1 -a2 -e 0 -o 0,1.2,2.2 "$WORK/sites-by-reason.tsv" "$WORK/execs-by-reason.tsv" \
      | awk -F'\t' '{ r[$1] = 1; s[$1] = $2; e[$1] = $3; ts += $2; te += $3 }
          END {
            for (k in r) {
              ss = ts ? 100 * s[k] / ts : 0; es = te ? 100 * e[k] / te : 0
              printf "%-22s %10d %7.1f%% %16d %7.1f%% %7.2fx\n", k, s[k], ss, e[k], es, (ss ? es / ss : 0)
            }
          }' | sort -k4 -rn
    echo
    echo "(exec/site > 1 = the class runs hotter than its share of the code; < 1 = colder.)"
  fi

  echo
  echo "workload: $ENTRY_MODULE ($WORK_LABEL) compiled by a compiler built $BUILD_LABEL"
  echo "corpus:   the compiler as built $BUILD_LABEL — sites and executions from ONE snapshot"
  echo "inputs:   $WORK (snapshot), heap $PROFILE_HEAP_WORDS words"
else
  printf '%-18s %-8s %-20s %-20s %-16s\n' MODULE STDOUT "Σapply vs pv_apply" "Σtail vs pv_tailcall" "drill vs slots"
  : >"$WORK/slots.tsv"; : >"$WORK/keys.tsv"
  for mod in $MODULES; do
    base="$WORK/$(echo "$mod" | tr . _)"
    mkdir -p "$base"
    expected="$WORK/expected/$mod.out"
    if [ ! -f "$expected" ]; then
      printf '%-18s %s\n' "$mod" "MISSING-EXPECTED($mod.out)"; rc=1; continue
    fi

    # shellcheck disable=SC2086
    if ! PURVASM_PROFILE_APPLY=1 node "$CLI" build $WORK_FLAG \
        --entry "$mod" --entry-name main --corefn-dir "$COREFN" --outdir "$base/prof" \
        >"$base/build.log" 2>&1 || [ ! -x "$base/prof/app" ]; then
      printf '%-18s %s\n' "$mod" "BUILD-FAIL (see $base/build.log)"; rc=1; continue
    fi

    if ! PURVASM_STATS=1 PURVASM_HEAP_WORDS="$PROFILE_HEAP_WORDS" \
        "$base/prof/app" >"$base/out" 2>"$base/err"; then
      printf '%-18s %s\n' "$mod" "RUN-FAIL (see $base/err)"; rc=1; continue
    fi

    # behaviour-neutrality: the instrumented program is still the program.
    stdout_verdict=OK
    diff -q "$expected" "$base/out" >/dev/null || { stdout_verdict=DIVERGED; rc=1; }

    if ! msg="$(slots_of "$base/err" "$base/slots.tsv")"; then
      printf '%-18s %-10s %s\n' "$mod" "$stdout_verdict" "$msg"; rc=1; continue
    fi
    verd="$(reconcile "$base/slots.tsv" "$base/err")" || rc=1
    v0="$(echo "$verd" | sed -n 1p)"
    v1="$(echo "$verd" | sed -n 2p)"
    case "$v0$v1" in *"!="* | *VACUOUS* | *SCHEMA*) rc=1 ;; esac
    # ADR-0108 §4: the drill reconciles against the very slot it drills, per fixture —
    # unconditionally, for the reason given in the `--selfhost` leg: an empty key file must be
    # compared as 0, never reported as "nothing to check".
    drill_of "$base/err" "$base/keys.tsv"
    dv="$(reconcile_drill "$base/slots.tsv" "$base/keys.tsv")" || rc=1
    case "$dv" in *"!="*) rc=1 ;; esac
    cat "$base/keys.tsv" >>"$WORK/keys.tsv"
    printf '%-18s %-8s %-20s %-20s %-16s\n' "$mod" "$stdout_verdict" "$v0" "$v1" "$dv"
    cat "$base/slots.tsv" >>"$WORK/slots.tsv"
  done

  echo
  echo "== executions by (form × reason), all fixtures ======================"
  ranking "$WORK/slots.tsv"
  if [ -s "$WORK/keys.tsv" ]; then
    echo
    echo "== the drill: foreign dispatches by arity status, all fixtures ======"
    awk -F'\t' '{ n = split($1, p, "|"); st[p[n]] += $2; tot += $2 }
      END { for (k in st) printf "%-16s %14d %7.1f%%\n", k, st[k], 100 * st[k] / tot }' \
      "$WORK/keys.tsv" | sort -k2 -rn
  fi

  echo
  echo "NOTE: this ranking describes THESE FIXTURES. The ADR's ranking — the one comparable with"
  echo "      the static census — comes from \`--selfhost\`, which profiles the compiler itself."
fi

echo
if [ "$rc" -eq 0 ]; then
  echo "OK: output unperturbed and both counter identities hold EXACTLY"
else
  echo "FAIL: see the rows above (work dir: $WORK)" >&2
fi
exit "$rc"
