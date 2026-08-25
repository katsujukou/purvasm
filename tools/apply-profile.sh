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
#   Σ generic-apply/<reason> + Σ local-deferred-apply/<kind> + foreign-deferred-apply
#     + structural-apply                          == pv_apply_entries    (every generic dispatch that
#                                                                       actually entered pv_apply)
#   Σ generic-tail/<reason> + Σ local-deferred-tail/<kind> + foreign-deferred-tail
#                                                 == pv_tailcall_writes  (every trampoline store)
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
#   tools/apply-profile.sh --paired <closure|apply|tail> [--build-mode …] [--work-mode …]
#                                         (ADR-0109 §5.1: ONE snapshot, ONE toolchain, the chosen
#                                          AXIS's two stages built and run in their own subdirs, then
#                                          the integer verdicts + the IR deltas. Two separate runs
#                                          cannot give this — each would snapshot its own inputs, and
#                                          the default workdir would overwrite the first.
#
#                                          The three axes are the ADR's three slices, and every knob
#                                          NOT on the axis is FIXED at the same value in both legs:
#                                            closure  PerUse → Hoisted        (call: via-apply both)
#                                            apply    ViaApply → DirectApplyOnly       (closure: hoisted)
#                                            tail     DirectApplyOnly → DirectApplyAndTail (closure: hoisted)
#                                          A run that moved two knobs would net two slices together,
#                                          which is exactly what the separately pinned endpoints —
#                                          "apply moves, tail does not" and its mirror — exist to
#                                          make checkable.)
#   tools/apply-profile.sh [--foreign-closure hoisted|per-use] ...
#                                         (ADR-0109 §5.2: which leg of the slice-A pair to build.
#                                          Default `hoisted` = the shipped path. The knob is
#                                          HARNESS-OWNED — an ambient value is scrubbed below, so a
#                                          leg is only ever what this flag says.)
#   tools/apply-profile.sh --alloc-identity BEFORE.err AFTER.err
#                                         (ADR-0109 §5.1 as a VERDICT over two captured runs)
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
ALLOC_IDENTITY=
FOREIGN_CLOSURE=hoisted
PAIRED=0
PAIRED_AXIS=
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
    --paired)
      case "${2:-}" in
        closure | apply | tail) PAIRED_AXIS="$2" ;;
        *) echo "apply-profile.sh: --paired takes an axis: closure|apply|tail" >&2; exit 2 ;;
      esac
      PAIRED=1; SELFHOST=1; shift 2 ;;
    --self-test) SELF_TEST=1; shift ;;
    --alloc-identity) ALLOC_IDENTITY="$2 $3"; shift 3 ;;
    --foreign-closure)
      case "$2" in hoisted | per-use) FOREIGN_CLOSURE="$2" ;;
        *) echo "apply-profile.sh: --foreign-closure must be hoisted|per-use, got $2" >&2; exit 2 ;; esac
      shift 2 ;;
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
  # ADR-0109: the drill fires at every foreign-callee occurrence, so it must be compared against
  # ALL THREE families — the arity-mismatch residue (`…/callee-foreign`), the direct calls, and the
  # knob-off deferred ones. Summing only the residue would make the identity vacuous the moment
  # slice B moved the mass out of it.
  slots=$(awk -F'\t' '$1 ~ /\/callee-foreign$/ || index($1, "foreign-direct-") == 1 || index($1, "foreign-deferred-") == 1 { s += $2 } END { print s + 0 }' "$1")
  keys=$(awk -F'\t' '{ s += $2 } END { print s + 0 }' "$2")
  if [ "$slots" = "$keys" ]; then echo "$keys == $slots"; else echo "$keys != $slots"; return 1; fi
}

# Parse the profile line of a run into `<slot>\t<count>`; fails if it is absent or duplicated.
#
# The row name pattern is deliberately GENERAL (any `/`-separated lowercase path): the line carries
# the ADR-0108 §5 allocation census (`alloc/kind/*`) and the ADR-0109 §5.1 allocation sites
# (`alloc/site/*`) beside the dispatch slots, and an over-tight pattern would drop them SILENTLY —
# a measurement vanishing without a verdict is the failure mode this harness exists to refuse.
# Family separation is by NAME downstream (`dispatch_rows`/`alloc_rows`), never by this parse.
slots_of() { # $1=stderr file  $2=out tsv
  local n
  n="$(grep -c '^purvasm-applyprofile:v1 ' "$1" || true)"
  [ "$n" = "1" ] || { echo "PROFILE-SCHEMA(x$n)"; return 1; }
  grep '^purvasm-applyprofile:v1 ' "$1" | tr ' ' '\n' \
    | sed -n 's/^\([a-z0-9/-]*\)=\([0-9][0-9]*\)$/\1\t\2/p' >"$2"
  # Every `name=value` token on the line must have parsed. A row the pattern cannot read would
  # otherwise be dropped here and reported nowhere.
  local tokens parsed
  tokens="$(grep '^purvasm-applyprofile:v1 ' "$1" | tr ' ' '\n' | grep -c '=' || true)"
  parsed="$(wc -l <"$2" | tr -d ' ')"
  [ "$tokens" = "$parsed" ] || { echo "PROFILE-PARSE($parsed/$tokens)"; return 1; }
  return 0
}

# The dispatch slots (everything that is not an allocation row) — the ONLY family the ADR-0108 §3
# identities and the reason ranking may see.
dispatch_rows() { awk -F'\t' 'index($1,"alloc/") != 1' "$1"; }

# The census schema, stated INDEPENDENTLY of the thing it checks (ADR-0107's rule: an expectation
# derived from the measurement is not a gate). These names are a deliberate second statement of
# `Kind::census_name` / `CallClass.allocSiteName`; adding a kind or a site means editing here too,
# which is the point — a row family that vanishes must not be able to pass.
ALLOC_KINDS="adt record closure pap str number ref byneed array rawids strslice"
ALLOC_SITES="foreign-materialise foreign-clo-init"

# Verify one profile line carries the WHOLE census: every kind row exactly once, every required
# site row exactly once. The presence-only check this replaces was satisfiable by the
# compiler-owned `alloc/site/*` rows alone, so the runtime's entire `alloc/kind/*` family could
# regress away and the leg would still print (and pass) with `Kind::Closure=0`.
check_alloc_schema() { # $1=slots tsv
  local bad=0 n
  for k in $ALLOC_KINDS; do
    n=$(awk -F'\t' -v r="alloc/kind/$k" '$1 == r { n++ } END { print n+0 }' "$1")
    [ "$n" = "1" ] || { echo "ALLOC-SCHEMA: alloc/kind/$k appears $n time(s), expected 1" >&2; bad=1; }
  done
  for s in $ALLOC_SITES; do
    n=$(awk -F'\t' -v r="alloc/site/$s" '$1 == r { n++ } END { print n+0 }' "$1")
    [ "$n" = "1" ] || { echo "ALLOC-SCHEMA: alloc/site/$s appears $n time(s), expected 1" >&2; bad=1; }
  done
  # nothing else may wear the prefix: an unknown row means the two schemas have drifted.
  n=$(awk -F'\t' 'index($1,"alloc/") == 1 { n++ } END { print n+0 }' "$1")
  local want=$(( $(echo "$ALLOC_KINDS" | wc -w) + $(echo "$ALLOC_SITES" | wc -w) ))
  [ "$n" = "$want" ] || { echo "ALLOC-SCHEMA: $n alloc/* rows, expected exactly $want" >&2; bad=1; }
  return "$bad"
}

# The ADR-0109 §5.1 identity, as a VERDICT across two legs rather than three printed numbers:
#   Kind::Closure(before) − Kind::Closure(after) == materialisations(before) − hoisted inits(after)
# Both legs must carry a complete census first, or the subtraction is of unknown quantities.
alloc_identity() { # $1=before slots tsv  $2=after slots tsv
  local rc=0 cb ca mb ma ia lhs rhs
  check_alloc_schema "$1" || rc=1
  check_alloc_schema "$2" || rc=1
  [ "$rc" = "0" ] || { echo "ALLOC-IDENTITY: skipped, a leg's census is incomplete" >&2; return 1; }
  cb=$(awk -F'\t' '$1 == "alloc/kind/closure" { s += $2 } END { print s+0 }' "$1")
  ca=$(awk -F'\t' '$1 == "alloc/kind/closure" { s += $2 } END { print s+0 }' "$2")
  mb=$(awk -F'\t' '$1 == "alloc/site/foreign-materialise" { s += $2 } END { print s+0 }' "$1")
  ia=$(awk -F'\t' '$1 == "alloc/site/foreign-clo-init" { s += $2 } END { print s+0 }' "$2")
  # LEG-INVARIANCE first (ADR-0109 §5.1): the materialisation counter sits at the SITE, so both
  # legs must count the same occurrences — that is what makes the closure delta attributable to the
  # change rather than to the two legs having executed different work. Checked, not assumed.
  ma=$(awk -F'\t' '$1 == "alloc/site/foreign-materialise" { s += $2 } END { print s+0 }' "$2")
  if [ "$mb" != "$ma" ]; then
    printf 'ADR-0109 §5.1 identity: materialisations differ between legs (%d vs %d) — the legs did not run the same work  FAIL\n' "$mb" "$ma" >&2
    return 1
  fi
  lhs=$(( cb - ca ))
  rhs=$(( mb - ia ))
  if [ "$lhs" = "$rhs" ]; then
    printf 'ADR-0109 §5.1 identity: ΔKind::Closure %d == materialisations %d − hoisted-inits %d = %d  OK\n' "$lhs" "$mb" "$ia" "$rhs"
  else
    printf 'ADR-0109 §5.1 identity: ΔKind::Closure %d != materialisations %d − hoisted-inits %d = %d  FAIL\n' "$lhs" "$mb" "$ia" "$rhs" >&2
    return 1
  fi
}

# The allocation rows: the runtime-owned per-`Kind` census and the compiler-owned instrumented
# sites (ADR-0108 §5 / ADR-0109 §5.1). Reported, never summed into a dispatch identity.
alloc_census() { # $1=slots tsv
  awk -F'\t' 'index($1,"alloc/") == 1 { n++ } END { exit !(n > 0) }' "$1" || return 0
  echo
  echo "== guest-heap allocation census (ADR-0108 §5) — mutator allocations, NOT collector copies =="
  printf '%-38s %14s\n' "row" "count"
  # Aggregated by row name, exactly as `ranking` aggregates the dispatch slots: the fixtures leg
  # concatenates one profile per fixture into this file, so a per-row sum is the only reading that
  # is a total rather than a list of legs.
  awk -F'\t' 'index($1,"alloc/site/") == 1 { n[$1] += $2 }
    END { for (r in n) printf "%-38s %14d\n", r, n[r] }' "$1" | sort -k2 -rn
  awk -F'\t' 'index($1,"alloc/kind/") == 1 { n[$1] += $2 }
    END { for (r in n) if (n[r] > 0) printf "%-38s %14d\n", r, n[r] }' "$1" | sort -k2 -rn
  awk -F'\t' 'index($1,"alloc/kind/") == 1 { n[$1] += $2 }
    END { for (r in n) if (n[r] == 0) z = z " " substr(r, 12); if (z != "") printf "(zero:%s)\n", z }' "$1"
  # The three terms of the ADR-0109 §5.1 identity, printed side by side. NOT a ratio: what the
  # materialisation site costs differs between the legs being compared (that is the whole point of
  # the change), so the only meaningful arithmetic is ACROSS two legs —
  #   ΔKind::Closure == materialisations(before) − hoisted inits(after)
  # — and one leg cannot compute it.
  awk -F'\t' 'index($1,"alloc/kind/closure") == 1 { c += $2 }
    index($1,"alloc/site/foreign-materialise") == 1 { m += $2 }
    index($1,"alloc/site/foreign-clo-init") == 1 { i += $2 }
    END { printf "\nADR-0109 §5.1 terms for this leg: materialisations=%d hoisted-inits=%d Kind::Closure=%d\n", m+0, i+0, c+0 }' "$1"
}

# --- the joined table's key derivation, ONE rule, shared by the report and its self-test ---------
#
# These two functions are the report's key space. They are functions and not inline awk because a
# self-test that re-implements them tests its own copy: the `structural-apply` rows below were
# first written against `reconcile` alone, and `reconcile` had counted structural since ADR-0108 —
# so deleting the report's structural branches left the suite green. A gate over a second spelling
# is not a gate over the thing that ships.
#
# The rule, applied identically to both sides:
#   generic-<form>/<reason…>         -> <reason…>            (a reason may contain "/", so the
#                                                             split takes the FIRST separator only:
#                                                             ADR-0113's local-unknown-fn/<origin>)
#   local-deferred-<form>/<kind>     -> candidate/<kind>     (its own namespace: "capture" is both
#                                                             an origin and a kind, and merging them
#                                                             would sum a population the emitter can
#                                                             act on with one it cannot)
#   foreign-deferred-<form>          -> foreign/<class>
#   foreign-direct-<form>            -> DROPPED: a direct call is not a dispatch
#   structural-apply                 -> itself (no reason axis; its SITES come from a `class` row)

# sites per population, from a census TSV
sites_by_population() { # $1 = census tsv
  awk -F'\t' '/^#/ { next }
      $3 == "reason" { slash = index($4, "/"); if (slash) s[substr($4, slash + 1)] += $5 }
      $3 == "kind"   { slash = index($4, "/"); if (slash) s["candidate/" substr($4, slash + 1)] += $5 }
      $3 == "class" && $4 == "structural-apply" { s["structural-apply"] += $5 }
    END { for (k in s) printf "%s\t%d\n", k, s[k] }' "$1" | sort
}

# executions per population, from dispatch slot rows on stdin
execs_by_population() {
  awk -F'\t' '{
      if ($1 == "structural-apply") { e[$1] += $2; next }
      slash = index($1, "/")
      if (slash == 0) { cls = $1; rest = "" } else { cls = substr($1, 1, slash - 1); rest = substr($1, slash + 1) }
      if (index(cls, "foreign-direct-") == 1) next
      if (index(cls, "local-deferred-") == 1) k = "candidate/" rest
      else if (index(cls, "foreign-") == 1) k = "foreign/" cls
      else if (rest == "") next
      else k = rest
      e[k] += $2
    }
    END { for (k in e) printf "%s\t%d\n", k, e[k] }' | sort
}

# The two identities. Echoes two verdict strings; returns non-zero if either fails.
reconcile() { # $1=slots tsv  $2=stderr file
  local sum_apply sum_tail rt_apply rt_tail rcl=0
  # ADR-0109 §7: the DEFERRED foreign classes lower to the generic dispatch, so they are part of
  # these sums — the direct ones are not (they are a call to the leaf's own entry, not a dispatch).
  # Leaving them out is what made the first self-host `--paired apply` run report a false failure:
  # the via-apply leg summed 208,673,409 against `pv_apply_entries` 638,915,068, and the missing
  # 430,241,659 were exactly the deferred applies. The identity held; the harness could not see it.
  #
  # ADR-0113 §3 adds the LOCAL-deferred pair for the same reason and with the same consequence: a
  # candidate is lowered as today's generic dispatch byte for byte, so its executions are inside
  # `pv_apply_entries`/`pv_tailcall_writes`. Omitting them would report a shortfall of exactly the
  # candidate population — the ADR-0109 false failure above, repeated one class later.
  sum_apply=$(awk -F'\t' 'index($1,"generic-apply/")==1 || $1=="structural-apply" || $1=="foreign-deferred-apply" || index($1,"local-deferred-apply/")==1 { s += $2 } END { print s+0 }' "$1")
  sum_tail=$(awk -F'\t' 'index($1,"generic-tail/")==1 || $1=="foreign-deferred-tail" || index($1,"local-deferred-tail/")==1 { s += $2 } END { print s+0 }' "$1")
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

# --- the paired verdicts' row helpers (top level so `--self-test` can drive them) ---------------
# ONE slot row's value, from a leg's parsed profile.
slot_of() { awk -F'\t' -v r="$2" '$1 == r { print $2 } END { }' "$1"; }
# …and the REQUIRED form: an absent row is `MISSING`, never 0. A slot that stopped being emitted
# and a slot that counted zero are different findings, and only one of them is a pass.
slot_req() { awk -F'\t' -v r="$2" '$1 == r { print $2; f = 1 } END { if (!f) print "MISSING" }' "$1"; }
# Assert a named row is IDENTICAL across the two legs.
same_row() { # $1=row
  local b a
  b=$(slot_of "$BEFORE/slots.tsv" "$1"); a=$(slot_of "$AFTER/slots.tsv" "$1")
  if [ "${b:-x}" = "${a:-y}" ]; then printf '  %-40s %14s == %-14s OK\n' "$1" "${b:-MISSING}" "${a:-MISSING}"
  else printf '  %-40s %14s != %-14s FAIL\n' "$1" "${b:-MISSING}" "${a:-MISSING}" >&2; rc=1; fi
}
# The ADR-0109 §5.1 completion condition for a direct-lowering slice, as a VERDICT.
#
# Printing "before -> after" is not a gate: an absent row reads as 0, so a partial transfer — or
# BOTH rows vanishing — would still print and still pass. Five conditions, each fail-closed, and a
# MISSING row is a failure rather than a zero:
#
#   before/deferred > 0        there was something to transfer (no vacuous pass)
#   before/direct  == 0        the counterfactual leg really is the counterfactual
#   after/deferred == 0        nothing was left behind
#   after/direct   == before/deferred      the transfer is EXACT, not merely large
#   Δ(runtime dispatch) == before/deferred the runtime's own counter agrees, to the unit
#
# The last one is the cross-mechanism check: the first four are all the compiler's classification
# talking to itself, and only the runtime counter is written down a different path.
transfer_verdict() { # $1=before tsv $2=after tsv $3=before err $4=after err $5=deferred row $6=direct row $7=stats field
  local bd bdir ad adir rb ra ok=0
  bd=$(slot_req "$1" "$5"); bdir=$(slot_req "$1" "$6")
  ad=$(slot_req "$2" "$5"); adir=$(slot_req "$2" "$6")
  rb=$(field_of "$3" 'purvasm-stats:v1' "$7"); ra=$(field_of "$4" 'purvasm-stats:v1' "$7")
  say() { # $1=label $2=verdict(0 ok) $3=detail
    if [ "$2" = "0" ]; then printf '  %-46s %s  OK\n' "$1" "$3"
    else printf '  %-46s %s  FAIL\n' "$1" "$3" >&2; ok=1; fi
  }
  # Per input, explicitly: a concatenated `case` with an empty alternative matches EVERYTHING
  # (`*""` is just `*`), which made this guard fire on every pair — caught by the self-test's
  # "a COMPLETE transfer passes" row, which is why that row exists.
  for v in "$bd" "$bdir" "$ad" "$adir" "$rb" "$ra"; do
    case "$v" in
      "" | MISSING | *[!0-9]*)
        say "every input present and numeric" 1 "got \"$v\""; rc=1; return 1 ;;
    esac
  done
  say "before: something to transfer" "$([ "$bd" -gt 0 ] && echo 0 || echo 1)" "$5=$bd > 0"
  say "before: no direct calls yet" "$([ "$bdir" = "0" ] && echo 0 || echo 1)" "$6=$bdir == 0"
  say "after: nothing left deferred" "$([ "$ad" = "0" ] && echo 0 || echo 1)" "$5=$ad == 0"
  say "transfer is EXACT" "$([ "$adir" = "$bd" ] && echo 0 || echo 1)" "$6=$adir == $bd"
  say "runtime counter agrees ($7)" "$([ "$(( rb - ra ))" = "$bd" ] && echo 0 || echo 1)" "$rb-$ra=$(( rb - ra )) == $bd"
  [ "$ok" = "0" ] || rc=1
  return "$ok"
}

ranking() { # $1=slots tsv (aggregated: slot, count)
  # DISPATCH rows only: an allocation row shares the line but is a different measurement, and
  # bucketing it by its second path segment would invent a reason called "site"/"kind".
  local d="$1.dispatch"
  dispatch_rows "$1" >"$d"
  echo
  printf '%-42s %14s %8s\n' "slot" "executions" "share"
  awk -F'\t' '{ n[$1] += $2; tot += $2 }
    END { for (s in n) if (n[s] > 0) printf "%-42s %14d %7.1f%%\n", s, n[s], 100 * n[s] / tot }' "$d" \
    | sort -k2 -rn
  echo
  printf '%-30s %14s %8s\n' "population (both forms)" "executions" "share"
  # The key is <class>/<tail>, and for a generic row the tail is a MissReason that may itself
  # contain "/" (local-unknown-fn/<origin>), so only the FIRST separator is a field boundary —
  # `split` on every "/" would fold the seven origins into one bucket named "local-unknown-fn".
  # Candidate rows are keyed by CandidateKind, in a namespace of their own, because a kind is not a
  # reason: merging the two would report one "capture" line that is the sum of a population the
  # emitter CAN act on and one it cannot.
  awk -F'\t' '{
      if ($1 == "structural-apply") next
      slash = index($1, "/")
      if (slash == 0) next
      cls = substr($1, 1, slash - 1); rest = substr($1, slash + 1)
      if (index(cls, "local-deferred-") == 1) k = "candidate/" rest; else k = rest
      r[k] += $2; tot += $2
    }
    END { for (k in r) printf "%-30s %14d %7.1f%%\n", k, r[k], 100 * r[k] / tot }' "$d" \
    | sort -k2 -rn
  alloc_census "$1"
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
# ADR-0109 §5.1 over two CAPTURED runs (a before leg and an after leg): parse each, require a
# complete census in both, and verdict the identity. No build, no prerequisites — the inputs are
# two files someone already produced, which is what makes it usable across a rebuild.
if [ -n "$ALLOC_IDENTITY" ]; then
  set -- $ALLOC_IDENTITY
  t="$(mktemp -d)"; trap 'rm -rf "$t"' EXIT
  n=0
  for src in "$@"; do
    n=$(( n + 1 ))
    if ! msg="$(slots_of "$src" "$t/leg$n.tsv")"; then
      echo "apply-profile.sh: $src: $msg" >&2; exit 1
    fi
  done
  [ "$n" = "2" ] || { echo "apply-profile.sh: --alloc-identity takes exactly BEFORE and AFTER" >&2; exit 2; }
  alloc_identity "$t/leg1.tsv" "$t/leg2.tsv" || exit 1
  exit 0
fi

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

  # --- the profile-line parse and the family split (ADR-0108 §5 / ADR-0109 §5.1) ----------------
  # The allocation census rides the dispatch line. Two properties are load-bearing and both fail
  # SILENTLY if wrong: a row the parse cannot read would vanish with no verdict, and an allocation
  # row summed into a dispatch identity would corrupt the very number the A/B turns on.
  echo
  echo "== self-test: the profile-line parse and the family split =========="
  pcheck() { # $1=label  $2=expected pass|fail  $3=profile line body
    printf 'purvasm-applyprofile:v1 %b\n' "$3" >"$t/prof.err"
    if slots_of "$t/prof.err" "$t/pslots.tsv" >/dev/null 2>&1; then got=pass; else got=fail; fi
    if [ "$got" = "$2" ]; then printf '  ok    %-46s (%s)\n' "$1" "$got"
    else printf '  FAIL  %-46s (expected %s, got %s)\n' "$1" "$2" "$got"; st_rc=1; fi
  }
  pcheck "three-segment census rows parse"  pass "generic-apply/callee-foreign=5 alloc/kind/closure=9 alloc/site/foreign-materialise=9"
  pcheck "an unreadable row FAILS, not drops" fail "generic-apply/callee-foreign=5 Weird/Row=3"
  # the absent/duplicated line is `slots_of`'s own schema check — asserted here without the
  # `purvasm-applyprofile:v1 ` prefix `pcheck` prepends.
  printf 'purvasm-stats:v1 pv_apply_entries=0\n' >"$t/prof.err"
  if slots_of "$t/prof.err" "$t/pslots.tsv" >/dev/null 2>&1; then got=pass; else got=fail; fi
  if [ "$got" = fail ]; then printf '  ok    %-46s (%s)\n' "a missing profile line fails" "$got"
  else printf '  FAIL  %-46s (expected fail, got %s)\n' "a missing profile line fails" "$got"; st_rc=1; fi

  printf 'purvasm-applyprofile:v1 generic-apply/callee-foreign=5 structural-apply=1 generic-tail/callee-foreign=2 alloc/kind/closure=900 alloc/site/foreign-materialise=900\n' >"$t/prof.err"
  printf 'purvasm-stats:v1 pv_apply_entries=6 pv_tailcall_writes=2\n' >>"$t/prof.err"
  slots_of "$t/prof.err" "$t/pslots.tsv" >/dev/null
  if reconcile "$t/pslots.tsv" "$t/prof.err" >/dev/null 2>&1; then got=pass; else got=fail; fi
  if [ "$got" = pass ]; then printf '  ok    %-46s (%s)\n' "census rows stay out of the dispatch sums" "$got"
  else printf '  FAIL  %-46s (expected pass, got %s)\n' "census rows stay out of the dispatch sums" "$got"; st_rc=1; fi
  n="$(dispatch_rows "$t/pslots.tsv" | grep -c 'alloc/' || true)"
  if [ "$n" = "0" ]; then printf '  ok    %-46s (%s)\n' "the dispatch family excludes every alloc row" "pass"
  else printf '  FAIL  %-46s (%s alloc rows leaked)\n' "the dispatch family excludes every alloc row" "$n"; st_rc=1; fi

  # --- ADR-0113 §3: the LOCAL-deferred rows must be inside the identities -------------------------
  # A gate that can be satisfied by the ABSENCE of its own input is not a gate, so each of the three
  # candidate kinds is injected in each form and the identity asserted. If `reconcile` ever stops
  # summing these rows, every one of these cases fails — which is what the ADR-0109 false failure
  # cost when the same omission happened one class earlier.
  echo
  echo "== self-test: the ADR-0113 candidate rows are inside the identities ="
  rcheck() { # $1=label  $2=expected pass|fail  $3=profile body  $4=stats body
    printf 'purvasm-applyprofile:v1 %b\n' "$3" >"$t/prof.err"
    printf 'purvasm-stats:v1 %b\n' "$4" >>"$t/prof.err"
    slots_of "$t/prof.err" "$t/pslots.tsv" >/dev/null 2>&1
    if reconcile "$t/pslots.tsv" "$t/prof.err" >/dev/null 2>&1; then got=pass; else got=fail; fi
    if [ "$got" = "$2" ]; then printf '  ok    %-52s (%s)\n' "$1" "$got"
    else printf '  FAIL  %-52s (expected %s, got %s)\n' "$1" "$2" "$got"; st_rc=1; fi
  }
  for kind in capture alias-local alias-global; do
    rcheck "local-deferred-apply/$kind counts toward pv_apply_entries" pass \
      "generic-apply/local-unknown-fn/param=3 local-deferred-apply/$kind=7" \
      "pv_apply_entries=10 pv_tailcall_writes=0"
    rcheck "local-deferred-tail/$kind counts toward pv_tailcall_writes" pass \
      "generic-tail/local-unknown-fn/param=2 local-deferred-tail/$kind=5" \
      "pv_apply_entries=0 pv_tailcall_writes=7"
  done
  # …and the mirror: if the runtime total does NOT include them, the identity must FAIL. This is the
  # row that would have caught the ADR-0109 omission, so it is stated for this class too.
  rcheck "a candidate row missing from the runtime total FAILS" fail \
    "generic-apply/local-unknown-fn/param=3 local-deferred-apply/capture=7" \
    "pv_apply_entries=3 pv_tailcall_writes=0"
  rcheck "a candidate row counted in the WRONG form FAILS" fail \
    "local-deferred-apply/capture=7" \
    "pv_apply_entries=0 pv_tailcall_writes=7"
  # a three-level generic key must survive the parse intact (local-unknown-fn/<origin>)
  pcheck "a three-level generic reason row parses" pass "generic-apply/local-unknown-fn/match-binder=4"
  # `structural-apply` has no reason axis, so it is the row most easily dropped from a table keyed
  # by reason — and dropping it leaves the footnote claiming a total the rows do not add up to.
  # It is a dispatch and must be inside the identity.
  rcheck "structural-apply counts toward pv_apply_entries" pass \
    "generic-apply/local-unknown-fn/param=3 structural-apply=4" \
    "pv_apply_entries=7 pv_tailcall_writes=0"
  rcheck "omitting structural-apply from the runtime total FAILS" fail \
    "generic-apply/local-unknown-fn/param=3 structural-apply=4" \
    "pv_apply_entries=3 pv_tailcall_writes=0"

  # --- the REPORT's key derivation, over the functions the report itself calls -------------------
  # The two rows above exercise `reconcile`, which has counted `structural-apply` since ADR-0108 —
  # so they stay green even if the joined table drops it. These exercise `sites_by_population` and
  # `execs_by_population`, which is where it was actually lost, using the real corpus's numbers.
  echo
  echo "== self-test: the report's key derivation (both sides) ============="
  cat >"$t/sites.tsv" <<'CENSUS'
#index	object	row	key	count
0	M	class	structural-apply	98
0	M	class	generic-apply	3
0	M	reason	generic-apply/local-unknown-fn/param	3
0	M	kind	local-deferred-apply/capture	696
CENSUS
  printf 'structural-apply	2314702
generic-apply/local-unknown-fn/param	81252445
local-deferred-apply/capture	16869596
foreign-direct-apply	442522201
' >"$t/dslots.tsv"

  kcheck() { # $1=label  $2=file  $3=expected line
    if grep -qxF "$3" "$2"; then printf '  ok    %-52s (%s)\n' "$1" "present"
    else printf '  FAIL  %-52s (missing: %s)\n' "$1" "$3"; st_rc=1; fi
  }
  kmissing() { # $1=label  $2=file  $3=key that must NOT appear
    if grep -q "^$3	" "$2"; then printf '  FAIL  %-52s (%s leaked)\n' "$1" "$3"; st_rc=1
    else printf '  ok    %-52s (%s)\n' "$1" "absent"; fi
  }
  sites_by_population "$t/sites.tsv" >"$t/s.tsv"
  execs_by_population <"$t/dslots.tsv" >"$t/e.tsv"
  kcheck "structural-apply has SITES (from its class row)"   "$t/s.tsv" "$(printf 'structural-apply\t98')"
  kcheck "structural-apply has EXECUTIONS"                   "$t/e.tsv" "$(printf 'structural-apply\t2314702')"
  kcheck "a three-level origin survives on the sites side"   "$t/s.tsv" "$(printf 'local-unknown-fn/param\t3')"
  kcheck "a three-level origin survives on the execs side"   "$t/e.tsv" "$(printf 'local-unknown-fn/param\t81252445')"
  kcheck "a candidate keeps its own namespace (sites)"       "$t/s.tsv" "$(printf 'candidate/capture\t696')"
  kcheck "a candidate keeps its own namespace (execs)"       "$t/e.tsv" "$(printf 'candidate/capture\t16869596')"
  kmissing "a DIRECT call is not in the dispatch denominator" "$t/e.tsv" "foreign/foreign-direct-apply"
  kmissing "…and does not leak under a truncated key"         "$t/e.tsv" "local-unknown-fn"
  # the sides must agree on their key SPACE: every key one produces, the other can produce too.
  if [ "$(cut -f1 "$t/s.tsv" | sort)" = "$(cut -f1 "$t/e.tsv" | sort)" ]; then
    printf '  ok    %-52s (%s)\n' "both sides derive the SAME key space" "equal"
  else
    printf '  FAIL  %-52s\n' "both sides derive the SAME key space"; st_rc=1
  fi

  # --- the TRANSFER verdict (ADR-0109 §5.1's completion condition for a direct-lowering slice) ---
  # This is the gate the first self-host `--paired apply` run did NOT have: its predecessor only
  # PRINTED "before -> after", so a partial transfer, or both rows vanishing, would have passed.
  echo
  echo "== self-test: the transfer verdict =================================="
  tcheck() { # $1=label  $2=expected pass|fail  $3=before rows  $4=before stats  $5=after rows  $6=after stats
    printf 'purvasm-applyprofile:v1 %b\n%b\n' "$3" "$4" >"$t/tb.err"
    printf 'purvasm-applyprofile:v1 %b\n%b\n' "$5" "$6" >"$t/ta.err"
    slots_of "$t/tb.err" "$t/tb.tsv" >/dev/null 2>&1
    slots_of "$t/ta.err" "$t/ta.tsv" >/dev/null 2>&1
    if ( rc=0; transfer_verdict "$t/tb.tsv" "$t/ta.tsv" "$t/tb.err" "$t/ta.err" \
           foreign-deferred-apply foreign-direct-apply pv_apply_entries ) >/dev/null 2>&1
    then got=pass; else got=fail; fi
    if [ "$got" = "$2" ]; then printf '  ok    %-46s (%s)\n' "$1" "$got"
    else printf '  FAIL  %-46s (expected %s, got %s)\n' "$1" "$2" "$got"; st_rc=1; fi
  }
  # 100 deferred applies become 100 direct calls, and the runtime's own counter drops by 100.
  tcheck "a COMPLETE transfer passes" pass \
    "foreign-deferred-apply=100 foreign-direct-apply=0" "purvasm-stats:v1 pv_apply_entries=150 pv_tailcall_writes=0" \
    "foreign-deferred-apply=0 foreign-direct-apply=100" "purvasm-stats:v1 pv_apply_entries=50 pv_tailcall_writes=0"
  tcheck "ONE dispatch left deferred fails" fail \
    "foreign-deferred-apply=100 foreign-direct-apply=0" "purvasm-stats:v1 pv_apply_entries=150 pv_tailcall_writes=0" \
    "foreign-deferred-apply=1 foreign-direct-apply=99" "purvasm-stats:v1 pv_apply_entries=51 pv_tailcall_writes=0"
  tcheck "direct short by ONE fails" fail \
    "foreign-deferred-apply=100 foreign-direct-apply=0" "purvasm-stats:v1 pv_apply_entries=150 pv_tailcall_writes=0" \
    "foreign-deferred-apply=0 foreign-direct-apply=99" "purvasm-stats:v1 pv_apply_entries=50 pv_tailcall_writes=0"
  # the rows transfer perfectly, but the runtime disagrees about how many dispatches went away —
  # the cross-mechanism half of the condition, and the only one not written by the classifier.
  tcheck "rows OK but the runtime delta differs fails" fail \
    "foreign-deferred-apply=100 foreign-direct-apply=0" "purvasm-stats:v1 pv_apply_entries=150 pv_tailcall_writes=0" \
    "foreign-deferred-apply=0 foreign-direct-apply=100" "purvasm-stats:v1 pv_apply_entries=51 pv_tailcall_writes=0"
  tcheck "a MISSING row is a failure, not a zero" fail \
    "foreign-deferred-apply=100 foreign-direct-apply=0" "purvasm-stats:v1 pv_apply_entries=150 pv_tailcall_writes=0" \
    "generic-apply/local-unknown-fn=1" "purvasm-stats:v1 pv_apply_entries=50 pv_tailcall_writes=0"
  tcheck "a VACUOUS pair (nothing to transfer) fails" fail \
    "foreign-deferred-apply=0 foreign-direct-apply=0" "purvasm-stats:v1 pv_apply_entries=10 pv_tailcall_writes=0" \
    "foreign-deferred-apply=0 foreign-direct-apply=0" "purvasm-stats:v1 pv_apply_entries=10 pv_tailcall_writes=0"

  # --- the §3 identities over the ADR-0109 classes ----------------------------------------------
  # A deferred foreign dispatch IS a `pv_apply`/`pv_tailcall`; a direct one is NOT. Both directions
  # are injected, because getting either wrong turns a holding identity into a reported failure (or,
  # worse, the reverse).
  echo
  echo "== self-test: the §3 identities over the foreign classes ==========="
  rcheck() { # $1=label  $2=expected pass|fail  $3=slot rows  $4=stats line
    printf 'purvasm-applyprofile:v1 %b\n' "$3" >"$t/r.err"
    printf '%b\n' "$4" >>"$t/r.err"
    slots_of "$t/r.err" "$t/r.tsv" >/dev/null 2>&1
    if reconcile "$t/r.tsv" "$t/r.err" >/dev/null 2>&1; then got=pass; else got=fail; fi
    if [ "$got" = "$2" ]; then printf '  ok    %-46s (%s)\n' "$1" "$got"
    else printf '  FAIL  %-46s (expected %s, got %s)\n' "$1" "$2" "$got"; st_rc=1; fi
  }
  rcheck "a DEFERRED apply counts toward pv_apply_entries" pass \
    "generic-apply/local-unknown-fn=10 foreign-deferred-apply=7 structural-apply=1" \
    "purvasm-stats:v1 pv_apply_entries=18 pv_tailcall_writes=0"
  rcheck "…and omitting it is a MISMATCH, not a pass" fail \
    "generic-apply/local-unknown-fn=10 foreign-deferred-apply=7 structural-apply=1" \
    "purvasm-stats:v1 pv_apply_entries=11 pv_tailcall_writes=0"
  rcheck "a DEFERRED tail counts toward pv_tailcall_writes" pass \
    "generic-apply/local-unknown-fn=1 generic-tail/local-unknown-fn=4 foreign-deferred-tail=6" \
    "purvasm-stats:v1 pv_apply_entries=1 pv_tailcall_writes=10"
  rcheck "a DIRECT call is NOT a dispatch and must not count" pass \
    "generic-apply/local-unknown-fn=3 foreign-direct-apply=99 foreign-direct-tail=99" \
    "purvasm-stats:v1 pv_apply_entries=3 pv_tailcall_writes=0"

  # --- the allocation census's own gate (ADR-0108 §5 / ADR-0109 §5.1) ---------------------------
  # The failure this replaces was silent: a presence-only check passed on the compiler-owned site
  # rows alone, so the runtime's whole kind family could vanish and the leg would report
  # Kind::Closure=0 as if it had measured it.
  echo
  echo "== self-test: the allocation census schema and the §5.1 identity ==="
  full_census() { # $1=extra rows  -> a complete census line body
    local out=""
    for k in $ALLOC_KINDS; do out="$out alloc/kind/$k=1"; done
    for s2 in $ALLOC_SITES; do out="$out alloc/site/$s2=1"; done
    printf 'generic-apply/callee-foreign=1%s%s' "$out" "$1"
  }
  acheck() { # $1=label  $2=expected pass|fail  $3=line body
    printf 'purvasm-applyprofile:v1 %b\n' "$3" >"$t/prof.err"
    slots_of "$t/prof.err" "$t/aslots.tsv" >/dev/null 2>&1
    if check_alloc_schema "$t/aslots.tsv" >/dev/null 2>&1; then got=pass; else got=fail; fi
    if [ "$got" = "$2" ]; then printf '  ok    %-46s (%s)\n' "$1" "$got"
    else printf '  FAIL  %-46s (expected %s, got %s)\n' "$1" "$2" "$got"; st_rc=1; fi
  }
  acheck "a complete census passes"            pass "$(full_census "")"
  acheck "THE KIND FAMILY GONE: sites only"    fail "generic-apply/callee-foreign=1 alloc/site/foreign-materialise=1 alloc/site/foreign-clo-init=1"
  acheck "one kind row gone (closure)"         fail "$(full_census "" | sed 's| alloc/kind/closure=1||')"
  acheck "a site row gone"                     fail "$(full_census "" | sed 's| alloc/site/foreign-clo-init=1||')"
  acheck "a duplicated kind row"               fail "$(full_census " alloc/kind/closure=7")"
  acheck "an unknown alloc/* row"              fail "$(full_census " alloc/kind/quantum=1")"

  icheck() { # $1=label  $2=expected pass|fail  $3=before body  $4=after body
    printf 'purvasm-applyprofile:v1 %b\n' "$3" >"$t/before.err"
    printf 'purvasm-applyprofile:v1 %b\n' "$4" >"$t/after.err"
    slots_of "$t/before.err" "$t/b.tsv" >/dev/null 2>&1
    slots_of "$t/after.err" "$t/a.tsv" >/dev/null 2>&1
    if alloc_identity "$t/b.tsv" "$t/a.tsv" >/dev/null 2>&1; then got=pass; else got=fail; fi
    if [ "$got" = "$2" ]; then printf '  ok    %-46s (%s)\n' "$1" "$got"
    else printf '  FAIL  %-46s (expected %s, got %s)\n' "$1" "$2" "$got"; st_rc=1; fi
  }
  # before: 100 closures, 68 of them foreign materialisations, no hoisted init
  # after:  35 closures ( = 100 − 68 + 3 hoisted ), same 68 materialisation SITES executed
  bcens=""; acens=""
  for k in $ALLOC_KINDS; do
    case "$k" in
      closure) bcens="$bcens alloc/kind/closure=100"; acens="$acens alloc/kind/closure=35" ;;
      *) bcens="$bcens alloc/kind/$k=0"; acens="$acens alloc/kind/$k=0" ;;
    esac
  done
  icheck "the identity holds across the two legs" pass \
    "x=1$bcens alloc/site/foreign-materialise=68 alloc/site/foreign-clo-init=0" \
    "x=1$acens alloc/site/foreign-materialise=68 alloc/site/foreign-clo-init=3"
  icheck "an off-by-one is a FAILED verdict"      fail \
    "x=1$bcens alloc/site/foreign-materialise=68 alloc/site/foreign-clo-init=0" \
    "x=1$acens alloc/site/foreign-materialise=67 alloc/site/foreign-clo-init=3"
  icheck "an incomplete census refuses to verdict" fail \
    "x=1 alloc/site/foreign-materialise=68 alloc/site/foreign-clo-init=0" \
    "x=1$acens alloc/site/foreign-materialise=68 alloc/site/foreign-clo-init=3"

  [ "$st_rc" -eq 0 ] && echo "OK: every gate fails on its injected fault" ||
    echo "FAIL: a gate did not catch an injected fault" >&2
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
unset PURVASM_PROFILE_APPLY PURVASM_BYNEED_OFF PURVASM_EMIT_DEBUG_ABI PURVASM_GC_STRESS PURVASM_STATS PURVASM_HEAP_WORDS PURVASM_FOREIGN_CLOSURE PURVASM_FOREIGN_CALL
# …then set the ONE knob this run owns, explicitly, so the leg is what the flag says and never what
# the caller's shell happened to hold (ADR-0109 §5.2).
export PURVASM_FOREIGN_CLOSURE="$FOREIGN_CLOSURE"

# The running binaries' heap, pinned so both legs of any comparison see one allocator regime.
: "${PROFILE_HEAP_WORDS:=134217728}"

rc=0


# =================================================================================================
# ADR-0109 §5.1: the slice-A PAIR.
#
# Both legs come from the ONE snapshot taken above — the same CoreFn, the same staged ulib, the same
# runtime staticlib, the same toolchain build. That is the property two separate invocations cannot
# have: each would snapshot at its own moment (and `output/` carries the compiler's own JS, so a
# `spago build` between them changes the program being measured), and both would write the same
# default workdir.
#
# The knob applies to the BUILD of the measured compiler and to nothing else (see the ownership
# note in the loop): both legs emit the workload — reference and profiled alike — in the SHIPPED
# mode, so the two compilers do identical work and their artifacts must come out byte-identical.
# That is a verdict below, not an assumption.
if [ "$PAIRED" = "1" ]; then
  BUILD_LABEL=${BUILD_FLAG:---opt}; WORK_LABEL=${WORK_FLAG:---opt}
  echo "== --paired: slice-A pair, compiler built ${BUILD_LABEL/--/} , compiling $ENTRY_MODULE ${WORK_LABEL/--/} ="

  # WHICH LEG THE KNOB APPLIES TO (corrected 2026-08-16, by the first paired run failing).
  #
  # The knob is a BUILD-mode axis in exactly ADR-0108 §3's sense: it decides how the MEASURED BINARY
  # is lowered, not what work that binary performs. Setting it for the workload compile as well made
  # the two legs emit DIFFERENT `.ll` for the workload — so the compilers did different work, and
  # every dispatch counter legitimately differed (callee-foreign 428,084,567 vs 427,910,970 &c.).
  # That is not a measurement of slice A; it is a measurement of two different runs.
  #
  # So: leg 2 (building the compiler) carries the mode; legs 1 and 3 (emitting the workload) run in
  # the SHIPPED mode in both legs. The two compilers then do identical work, and their emitted
  # artifacts must come out byte-identical — which becomes a verdict below rather than an assumption.
  # The axis's two stages, and the value every OTHER knob is pinned to in both legs.
  case "$PAIRED_AXIS" in
    closure) STAGES="per-use hoisted"; FIX_CLOSURE=""; FIX_CALL=via-apply ;;
    apply)   STAGES="via-apply direct-apply-only"; FIX_CLOSURE=hoisted; FIX_CALL="" ;;
    tail)    STAGES="direct-apply-only direct-apply-and-tail"; FIX_CLOSURE=hoisted; FIX_CALL="" ;;
  esac
  echo "   axis: $PAIRED_AXIS   stages: $STAGES   (closure=${FIX_CLOSURE:-<axis>} call=${FIX_CALL:-<axis>})"

  for mode in $STAGES; do
    d="$WORK/$mode"; mkdir -p "$d"
    # legs 1 and 3 (the workload) always run in the SHIPPED configuration, so both legs do identical
    # work; the axis moves only in leg 2, which builds the measured compiler.
    export PURVASM_FOREIGN_CLOSURE=hoisted
    unset PURVASM_FOREIGN_CALL
    echo
    echo "== leg [$mode] 1/3: reference emission of the workload (node-hosted) "
    # shellcheck disable=SC2086
    node "$CLI" build --corefn-dir "$COREFN" --entry "$ENTRY_MODULE" --entry-name "$ENTRY_NAME" \
      --outdir "$d/ref" --emit-llvm $WORK_FLAG >"$d/ref.log" 2>&1 ||
      { echo "apply-profile.sh: [$mode] reference leg failed; see $d/ref.log" >&2; exit 1; }

    echo "== leg [$mode] 2/3: building the instrumented compiler in $mode (long) "
    # shellcheck disable=SC2086
    # the leg's knobs: the axis one takes the stage, the others their fixed value.
  case "$PAIRED_AXIS" in
      closure) LEG_CLOSURE="$mode"; LEG_CALL="$FIX_CALL" ;;
      *) LEG_CLOSURE="$FIX_CLOSURE"; LEG_CALL="$mode" ;;
    esac
    PURVASM_FOREIGN_CLOSURE="$LEG_CLOSURE" PURVASM_FOREIGN_CALL="$LEG_CALL" PURVASM_PROFILE_APPLY=1 node "$CLI" build --corefn-dir "$COREFN" \
      --entry "$ENTRY_MODULE" --entry-name "$ENTRY_NAME" \
      --outdir "$d/compiler" $BUILD_FLAG >"$d/compiler.log" 2>&1 ||
      { echo "apply-profile.sh: [$mode] instrumented compiler build failed; see $d/compiler.log" >&2; exit 1; }
    [ -x "$d/compiler/app" ] ||
      { echo "apply-profile.sh: [$mode] no instrumented binary at $d/compiler/app" >&2; exit 1; }

    echo "== leg [$mode] 3/3: instrumented compiler compiles the closure ======"
    # shellcheck disable=SC2086
    PURVASM_STATS=1 PURVASM_HEAP_WORDS="$PROFILE_HEAP_WORDS" \
      "$d/compiler/app" build --corefn-dir "$COREFN" --entry "$ENTRY_MODULE" \
      --entry-name "$ENTRY_NAME" --outdir "$d/prof" --emit-llvm $WORK_FLAG \
      >"$d/prof.log" 2>"$d/prof.err" ||
      { echo "apply-profile.sh: [$mode] profiled run failed; see $d/prof.err" >&2; exit 1; }

    # per-leg: instrumentation is inert WITHIN this mode, and this leg's own identities hold.
    if diff -r "$d/ref/_build" "$d/prof/_build" >"$d/emission.diff" 2>&1; then
      echo "  [$mode] OK: emission identical to the reference ($(find "$d/prof/_build" -name '*.ll' | wc -l | tr -d ' ') objects)"
    else
      echo "  [$mode] FAIL: instrumented compiler emitted DIFFERENT artifacts (see $d/emission.diff)" >&2
      head -5 "$d/emission.diff" >&2; rc=1
    fi
    if ! msg="$(slots_of "$d/prof.err" "$d/slots.tsv")"; then
      echo "  [$mode] FAIL: $msg" >&2; exit 1
    fi
    check_alloc_schema "$d/slots.tsv" || { echo "  [$mode] FAIL: incomplete allocation census" >&2; rc=1; }
    verd="$(reconcile "$d/slots.tsv" "$d/prof.err")" || rc=1
    printf '  [%s] Σ generic-apply + structural vs pv_apply_entries : %s\n' "$mode" "$(echo "$verd" | sed -n 1p)"
    printf '  [%s] Σ generic-tail               vs pv_tailcall_writes: %s\n' "$mode" "$(echo "$verd" | sed -n 2p)"
    case "$verd" in *"!="* | *VACUOUS* | *SCHEMA*) rc=1 ;; esac
    drill_of "$d/prof.err" "$d/keys.tsv"
    dv="$(reconcile_drill "$d/slots.tsv" "$d/keys.tsv")" || rc=1
    printf '  [%s] Σ drill keys vs the callee-foreign slots        : %s\n' "$mode" "$dv"
    case "$dv" in *"!="*) rc=1 ;; esac
  done

  BEFORE="$WORK/$(echo $STAGES | cut -d' ' -f1)"; AFTER="$WORK/$(echo $STAGES | cut -d' ' -f2)"
  echo
  echo "== PAIRED VERDICTS ($PAIRED_AXIS: $(echo $STAGES | tr ' ' '>')) ====="

  case "$PAIRED_AXIS" in
    closure)
      # 1. the §5.1 three-way identity (this also checks materialisation leg-invariance)
      alloc_identity "$BEFORE/slots.tsv" "$AFTER/slots.tsv" || rc=1

      # 2. every DISPATCH slot identical, as a vector — slice A must not move dispatch at all, and a
      #    sum could hide two slots moving in opposite directions.
      dispatch_rows "$BEFORE/slots.tsv" | sort >"$WORK/disp-before.tsv"
      dispatch_rows "$AFTER/slots.tsv" | sort >"$WORK/disp-after.tsv"
      if diff -q "$WORK/disp-before.tsv" "$WORK/disp-after.tsv" >/dev/null; then
        echo "dispatch slot vector: IDENTICAL ($(wc -l <"$WORK/disp-before.tsv" | tr -d ' ') slots)  OK"
      else
        echo "dispatch slot vector: DIFFERS  FAIL" >&2
        diff "$WORK/disp-before.tsv" "$WORK/disp-after.tsv" >&2 || true; rc=1
      fi
      ;;
    apply)
      # SLICE B. The apply form moves and the TAIL form does not — separately, so the two slices
      # stay separable. Stated as row-level verdicts rather than a total: a total could net a tail
      # regression against an apply win.
      echo "TRANSFER (the slice's own axis, fail-closed):"
      transfer_verdict "$BEFORE/slots.tsv" "$AFTER/slots.tsv" "$BEFORE/prof.err" "$AFTER/prof.err" \
        foreign-deferred-apply foreign-direct-apply pv_apply_entries || true
      echo "INVARIANT (slice C's axis, and the dispatch the slice does not touch):"
      same_row foreign-deferred-tail
      same_row foreign-direct-tail
      same_row generic-tail/callee-foreign
      same_row generic-apply/callee-foreign
      same_row alloc/site/foreign-materialise
      same_row alloc/site/foreign-clo-init
      same_row alloc/kind/closure
      rt_tail_b=$(field_of "$BEFORE/prof.err" 'purvasm-stats:v1' pv_tailcall_writes)
      rt_tail_a=$(field_of "$AFTER/prof.err" 'purvasm-stats:v1' pv_tailcall_writes)
      if [ "$rt_tail_b" = "$rt_tail_a" ]; then
        printf '  %-40s %14s == %-14s OK\n' "pv_tailcall_writes (runtime)" "$rt_tail_b" "$rt_tail_a"
      else
        printf '  %-40s %14s != %-14s FAIL\n' "pv_tailcall_writes (runtime)" "$rt_tail_b" "$rt_tail_a" >&2; rc=1
      fi
      ;;
    tail)
      # SLICE C, the mirror: the tail form moves, the apply form does not.
      echo "TRANSFER (the slice's own axis, fail-closed):"
      transfer_verdict "$BEFORE/slots.tsv" "$AFTER/slots.tsv" "$BEFORE/prof.err" "$AFTER/prof.err" \
        foreign-deferred-tail foreign-direct-tail pv_tailcall_writes || true
      echo "INVARIANT (slice B's axis, and the dispatch the slice does not touch):"
      same_row foreign-deferred-apply
      same_row foreign-direct-apply
      same_row generic-apply/callee-foreign
      same_row generic-tail/callee-foreign
      same_row alloc/site/foreign-materialise
      # the closure axis is pinned `hoisted` in BOTH legs here, so the hoisted-init count is an
      # invariant of this pair too — checked for symmetry with the apply axis, where it already was.
      same_row alloc/site/foreign-clo-init
      same_row alloc/kind/closure
      rt_apply_b=$(field_of "$BEFORE/prof.err" 'purvasm-stats:v1' pv_apply_entries)
      rt_apply_a=$(field_of "$AFTER/prof.err" 'purvasm-stats:v1' pv_apply_entries)
      if [ "$rt_apply_b" = "$rt_apply_a" ]; then
        printf '  %-40s %14s == %-14s OK\n' "pv_apply_entries (runtime)" "$rt_apply_b" "$rt_apply_a"
      else
        printf '  %-40s %14s != %-14s FAIL\n' "pv_apply_entries (runtime)" "$rt_apply_b" "$rt_apply_a" >&2; rc=1
      fi
      ;;
  esac

  # 3-4 are slice A's own: the cells exist only on the closure axis.
  if [ "$PAIRED_AXIS" = "closure" ]; then
  # 3. the per-use leg builds no hoisted closures at all.
  ib=$(awk -F'\t' '$1 == "alloc/site/foreign-clo-init" { print $2 }' "$BEFORE/slots.tsv")
  if [ "${ib:-x}" = "0" ]; then echo "per-use hoisted-inits: 0  OK"
  else echo "per-use hoisted-inits: ${ib:-MISSING} (expected 0)  FAIL" >&2; rc=1; fi

  # 4. the hoisted leg's init count, from FOUR independent places: the runtime counter, the entry
  #    object's cell definitions, the permanent-root stores inside the init, and the leaf symbols it
  #    references (the reachable-key set the compiler derived).
  ia=$(awk -F'\t' '$1 == "alloc/site/foreign-clo-init" { print $2 }' "$AFTER/slots.tsv")
  # the COMPILER's own entry object — the running binary is what holds the cells and executes the
  # init the runtime counter counted; the workload emission has its own, unrelated, entry object.
  entry_ll="$AFTER/compiler/_build/entry.ll"
  cells=$(grep -c '\$fclo = global i64 0' "$entry_ll" || true)
  init_body=$(awk '/^define void @pv_fclo_init\(/ { on = 1 } on { print } on && /^}/ { exit }' "$entry_ll")
  stores=$(printf '%s\n' "$init_body" | grep -c 'store i64 .*, ptr @pvf_.*\$fclo' || true)
  syms=$(printf '%s\n' "$init_body" | grep -c 'ptrtoint ptr @pvf_' || true)
  if [ "${ia:-x}" = "$cells" ] && [ "$cells" = "$stores" ] && [ "$stores" = "$syms" ]; then
    echo "hoisted inits == cells == stores == leaf symbols: $ia  OK"
  else
    echo "hoisted inits ${ia:-MISSING} / cells $cells / stores $stores / symbols $syms  FAIL" >&2; rc=1
  fi

  fi

  # 5. the two compilers DID THE SAME WORK: byte-identical workload artifacts, not merely the same
  #    object set. This is what licenses reading every counter difference above as an effect of the
  #    lowering rather than of the two legs having compiled different things.
  # A MISSING artifact set is a different finding from a differing one, and must not be reported as
  # "the legs did not do the same work" (a profiled run that emitted nothing is a broken leg, not a
  # divergence).
  if [ ! -d "$BEFORE/prof/_build" ] || [ ! -d "$AFTER/prof/_build" ]; then
    echo "workload emission: MISSING — a profiled leg emitted no artifacts  FAIL" >&2; rc=1
  elif diff -r "$BEFORE/prof/_build" "$AFTER/prof/_build" >"$WORK/workload.diff" 2>&1; then
    echo "workload emission: BYTE-IDENTICAL ($(find "$AFTER/prof/_build" -name '*.ll' | wc -l | tr -d ' ') objects)  OK"
  else
    echo "workload emission: DIFFERS — the legs did not do the same work  FAIL" >&2
    head -5 "$WORK/workload.diff" >&2; rc=1
  fi

  # 6. the IR delta, over the CORPUS THAT CHANGED — each leg's own compiler object set
  #    (`compiler/_build`, kept by the build even without `--emit-llvm`), NOT the workload emission,
  #    which verdict 5 has just pinned byte-identical. Each row names the EXACT needle it counts;
  #    reload and frame counts are deliberately absent — under the release inline ABI both are bare
  #    load/store on the ctx header with no distinguishing text, so there is no honest grep for
  #    them (the root-chain count below is the rooting proxy, and it IS exact).
  echo
  printf '%-34s %14s %14s %12s\n' "IR measure (needle)" "$(echo $STAGES | cut -d' ' -f1)" "$(echo $STAGES | cut -d' ' -f2)" "delta"
  # ANCHORED counts (ADR-0108 §2's self-reference trap, hit again here): the corpus IS the compiler,
  # so a module that emits LLVM carries emitted syntax as string constants — the unanchored `$fclo`
  # needle read 1 on the per-use leg, which builds no cells at all, because `ForeignRef.purs`'s
  # `c"$fclo"` constant is in the compiler's own object. Every measure below is therefore anchored to
  # its INSTRUCTION form and to the two-space instruction indent; a declaration, a definition or a
  # string constant sits at column 0 (or in quotes) and cannot match.
  ir_count() { # $1=dir  $2=anchored ERE
    find "$1" -name '*.ll' -exec grep -c -E -- "$2" {} + | awk -F: '{ s += $NF } END { print s+0 }'
  }
  ir_lines() { find "$1" -name '*.ll' -exec cat {} + | wc -l | tr -d ' '; }
  ir_bytes() { find "$1" -name '*.ll' -exec cat {} + | wc -c | tr -d ' '; }
  row() { # $1=label  $2=before  $3=after
    printf '%-34s %14s %14s %12s\n' "$1" "$2" "$3" "$(( $3 - $2 ))"
  }
  row ".ll lines" "$(ir_lines "$BEFORE/compiler/_build")" "$(ir_lines "$AFTER/compiler/_build")"
  row ".ll bytes" "$(ir_bytes "$BEFORE/compiler/_build")" "$(ir_bytes "$AFTER/compiler/_build")"
  ir_row() { # $1=label  $2=anchored ERE
    row "$1" "$(ir_count "$BEFORE/compiler/_build" "$2")" "$(ir_count "$AFTER/compiler/_build" "$2")"
  }
  # a label line, already at column 0 — the only measure that is not an instruction
  ir_row "root chains (^rchk label)" '^rchk'
  ir_row "pv_root (root slow path)" '^  %[^ ]+ = call i64 @pv_root\('
  ir_row "pv_make_closure" '^  %[^ ]+ = call i64 @pv_make_closure\('
  ir_row 'cell READS (load @…$fclo)' '^  %[^ ]+ = load i64, ptr @pvf_[A-Za-z0-9_]+[$]fclo$'
  ir_row "pv_apply call sites" '^  %[^ ]+ = call i64 @pv_apply\('
  ir_row "pv_tailcall call sites" '^  call void @pv_tailcall\('

  echo
  if [ "$rc" = "0" ]; then
    echo "OK: the $PAIRED_AXIS pair reconciles — one snapshot, two stages, every integer accounted"
  else
    echo "FAIL: see the rows above (work dir: $WORK)" >&2
  fi
  exit "$rc"
fi

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
  check_alloc_schema "$WORK/slots.tsv" || { echo "FAIL: the allocation census is incomplete on this leg" >&2; rc=1; }
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
    # BOTH sides derive their key by the SAME rule, and it is not `split(…, "/")`: a generic key is
    # <class>/<reason> where the reason may ITSELF contain a slash (ADR-0113's
    # `local-unknown-fn/<origin>`), so `p[2]` silently truncates seven origins to one bucket — and a
    # candidate key is <class>/<kind>, a different namespace whose `p[2]` collides with a reason of
    # the same spelling ("capture" is both an origin and a kind). Keying them apart is not cosmetic:
    # merged, one line would be the sum of a population the emitter CAN act on and one it cannot.
    #
    # The rule, shared by the two producers below:
    #   generic-<form>/<reason…>        -> <reason…>              (origin kept)
    #   local-deferred-<form>/<kind>    -> candidate/<kind>
    #   foreign-{direct,deferred}-<form>-> foreign/<that class>
    #   structural-apply                -> dropped (no reason axis)
    #
    # sites per population (both forms), from the census's `reason` and `kind` rows
    sites_by_population "$WORK/census-work/sites.tsv" >"$WORK/sites-by-reason.tsv"
    # executions per population, from this run's DISPATCH slots (an allocation row is a different
    # measurement on the same line and has no reason axis).
    dispatch_rows "$WORK/slots.tsv" | execs_by_population >"$WORK/execs-by-reason.tsv"
    # The DIRECT classes, counted separately. They are calls, not dispatches: no pv_apply entry, no
    # trampoline write. Keeping them out of the denominator matters on THIS run, where ADR-0109 had
    # just converted 442.5 M dispatches into direct calls — leaving them in made every dispatch
    # share read about 2.3x smaller than it is.
    direct_calls=$(dispatch_rows "$WORK/slots.tsv" | awk -F'\t' 'index($1,"foreign-direct-")==1 { d += $2 } END { print d+0 }')

    echo
    echo "== ONE corpus, two measurements ===================================="
    printf '%-30s %10s %8s %16s %8s %8s\n' "population" "sites" "share" "executions" "share" "exec/site"
    join -t $'\t' -a1 -a2 -e 0 -o 0,1.2,2.2 "$WORK/sites-by-reason.tsv" "$WORK/execs-by-reason.tsv" \
      | awk -F'\t' '{ r[$1] = 1; s[$1] = $2; e[$1] = $3; ts += $2; te += $3 }
          END {
            for (k in r) {
              ss = ts ? 100 * s[k] / ts : 0; es = te ? 100 * e[k] / te : 0
              printf "%-30s %10d %7.1f%% %16d %7.1f%% %7.2fx\n", k, s[k], ss, e[k], es, (ss ? es / ss : 0)
            }
          }' | sort -k4 -rn
    echo
    echo "(exec/site > 1 = the class runs hotter than its share of the code; < 1 = colder.)"
    echo "(the executions column counts DISPATCHES only, so its total is pv_apply_entries +"
    echo " pv_tailcall_writes. A direct call is a different operation and is excluded:"
    echo " $direct_calls foreign-direct calls in this run, the population ADR-0109 moved.)"
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
    # per FIXTURE, not on the concatenated file: "exactly one row per kind" is a property of one
    # profile line, and the aggregate legitimately holds one per fixture.
    check_alloc_schema "$base/slots.tsv" || { echo "  $m: ALLOC-SCHEMA incomplete" >&2; rc=1; }
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
