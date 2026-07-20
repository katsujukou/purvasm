#!/usr/bin/env bash
# The ADR-0104 §2 **self-host fixpoint gate** — the level-to-level byte identity that survives
# boot's retirement (the old manual "488/488" boot-vs-L2 diff, re-anchored so that no leg's
# meaning depends on boot's emission staying representative).
#
# Pinned procedure (ADR-0104 §2):
#
#   1. boot builds the Level-2 compiler → the stage-2 binary `C2` (bootstrap seed; its own bytes
#      are NOT part of the comparison — boot's emission may diverge freely post-§3).
#   2. `C2` compiles the pinned CoreFn closure (this repo's `output/` + staged `dist/ulib`) →
#      **stage-3 artifacts** (`--emit-llvm`), from which the stage-3 binary `C3` is linked.
#      Link vehicle: the purvasm-native CLI cannot exec `clang` yet (ADR-0045 — `Process: exec`
#      has no native leaf), so the NODE-hosted CLI (the same compiler) performs the full build and
#      its link; the script then asserts the Node run's emitted artifacts are BYTE-IDENTICAL to
#      stage-3 before adopting its binary as `C3` — so `C3` is linked from stage-3's bytes, with a
#      free native≡Node compiler cross-check on the side. (Duplicating the CLI's provider-map/link
#      pipeline in shell was rejected as a second linker to keep correct.)
#   3. `C3` compiles the *same* pinned closure → **stage-4 artifacts** (`--emit-llvm`; linking a
#      C4 would add nothing — linked binaries are excluded from the identity).
#   4. Gate: **stage-3 ≡ stage-4, byte-for-byte** over every per-module `.ll`, `entry.ll`, and
#      every `.pmi`. Both compile legs run the same mode and the same heap settings.
#
# Input pinning (review P1): "identical input" is a property of the RUN, not of repo quiescence —
# the CoreFn closure (`output/`, which also carries the Node-hosted compiler's own compiled JS),
# the CLI wrapper, the staged ulib, and the runtime staticlib are snapshotted into the workdir up
# front, and every leg (boot, C2, the Node link leg's compiler CODE included, C3) reads ONLY the
# snapshots — a concurrent `spago build` / ulib restaging / `cargo build` cannot change any
# leg's input (or the link leg's executable) mid-run.
#
# Comparison-set completeness (review P1): before any byte comparison, each build is validated to
# actually CONTAIN the pinned comparison set — exactly one `entry.ll`, ≥ 1 module `.ll`, ≥ 1
# `.pmi`, module-`.ll`/`.pmi` counts equal — and BOTH build kinds enforce an exact filename
# allowlist (emit: the comparison classes only; link: + the known `.o` byproducts), so a class
# missing from both sides, or an unknown artifact class, fails the gate instead of silently
# shrinking it.
#
# Mode profiles (ADR-0104 §2):
#   smoke      — `--no-opt` (default per the 2026-07-19 measurements: end-to-end ≈ 8–20 min,
#                dominated by clang and strongly cache-dependent — observed C2 ≈ 3–10 min,
#                stage-3 emit ≈ 1.5–3 min, C3 link ≈ 1–7 min, stage-4 emit ≈ 2–3 min; fine as a
#                milestone-cadence gate, not per-PR; re-measure the default when the perf track
#                moves)
#   milestone  — `--opt` (the recommended self-host path; also pins the optimiser's determinism;
#                the required milestone profile). Currently **blocked and non-blocking under an
#                explicit maintainer waiver** (ADR-0104 §2 amendment): stall CONFIRMED
#                2026-07-19 on an unbounded attempt — the native `--opt` stage-3 leg clears
#                281/298 modules in ≈ 4 min, then sits on the next tail module (mod_282,
#                ~`LLVM.Emit`, as first recorded by the §5-1 Progress note) at ≈ 100 % CPU for
#                54+ min before the attempt was abandoned; a prior observation exceeded 48 min
#                the same way (the ADR-0102 apply-count class on the native runtime; the
#                Node-hosted `--opt` build of the same corpus takes minutes, and is NOT a
#                substitute — it exercises neither C2 nor C3). While the waiver stands, smoke is
#                the operative fixpoint gate and each checkpoint records a BOUNDED milestone
#                re-attempt; the waiver expires on the first successful native milestone run or
#                when the tracked perf blocker is declared resolved.
#
# Usage (from the repo root, inside `nix develop`, after `spago build` + ulib staging):
#   tools/selfhost-fixpoint-diff.sh [smoke|milestone]
#
# Knobs: $FIXPOINT_HEAP_WORDS (semi-space words for the C2/C3 compile legs; default 128Mi words
# = 1 GiB per semi-space), $PURVASM_RT_A, $PURVASM_LIB.
set -u

cd "$(dirname "${BASH_SOURCE[0]}")/.."
ROOT="$(pwd)"

BOOT="$ROOT/boot/_build/default/bin/main.exe"
: "${PURVASM_RT_A:=$ROOT/runtime/target/release/libpurvasm_rt.a}"
: "${PURVASM_LIB:=$ROOT/dist/ulib}"
export PURVASM_RT_A PURVASM_LIB

# The running compiler binaries' heap (PURVASM_HEAP_WORDS overrides the baked size, ADR-0102 §4).
# The whole-closure self-compile needs a real heap; the baked default targets small programs.
: "${FIXPOINT_HEAP_WORDS:=134217728}"

# The NATIVE CLI entry (ADR-0045/0056): `Purvasm.CLI.Main` is the Node-hosted entry and its
# closure pulls the `node-*` foreigns, which have no native leaves by design.
ENTRY_MODULE=Purvasm.CLI.Native
ENTRY_NAME=main

PROFILE="${1:-smoke}"
case "$PROFILE" in
  smoke) MODE_FLAG="--no-opt" ;;
  milestone) MODE_FLAG="" ;;
  *) echo "usage: tools/selfhost-fixpoint-diff.sh [smoke|milestone]" >&2; exit 2 ;;
esac

for tool in "$BOOT" "$PURVASM_RT_A" "$PURVASM_LIB" "$ROOT/output/$ENTRY_MODULE/corefn.json"; do
  [ -e "$tool" ] || { echo "missing prerequisite: $tool" >&2; exit 2; }
done
for cmd in clang node; do
  command -v "$cmd" >/dev/null || { echo "missing prerequisite: $cmd on PATH (run inside nix develop)" >&2; exit 2; }
done

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

echo "profile: $PROFILE (${MODE_FLAG:---opt})"

STAMP=0
stamp() { # $1=label — print the elapsed seconds since the previous stamp
  local now; now="$(date +%s)"
  [ "$STAMP" -ne 0 ] && echo "    ($1: $((now - STAMP))s)"
  STAMP="$now"
}

# --- pin the inputs: snapshot everything every leg reads (see the header) ----------------------
# `output/` doubles as BOTH the pinned CoreFn closure and the Node-hosted compiler's own compiled
# JS, and `cli/index.node.js` imports the latter by the relative path `../output/…` — so the
# snapshot reproduces that relative layout and the link leg runs the SNAPSHOT's wrapper. Without
# this, a concurrent `spago build` could swap the link leg's compiler code mid-run even though
# its corefn input was pinned.
echo "snapshotting inputs (CoreFn+compiler-JS output/, cli wrapper, staged ulib, runtime .a) ..."
stamp start
cp -R "$ROOT/output" "$WORK/output"
mkdir "$WORK/cli"
cp "$ROOT/cli/index.node.js" "$WORK/cli/index.node.js"
cp -R "$PURVASM_LIB" "$WORK/ulib"
cp "$PURVASM_RT_A" "$WORK/libpurvasm_rt.a"
COREFN="$WORK/output"
NODE_CLI="$WORK/cli/index.node.js"
PURVASM_LIB="$WORK/ulib"
PURVASM_RT_A="$WORK/libpurvasm_rt.a"
export PURVASM_LIB PURVASM_RT_A
stamp "input snapshot"

# Comparison-set completeness (see the header): validate a build actually contains the pinned set
# before any byte comparison — a class missing from BOTH sides must FAIL, not shrink the gate.
# BOTH modes enforce an exact filename allowlist, so nothing is silently excluded from
# comparison: `emit` admits only the comparison classes; `link` additionally admits the link
# byproducts the CLI is known to write (`mod_*`/`entry`/`foreign_*` objects — an unknown class
# fails loudly and is added here deliberately, never ignored).
check_build() { # $1=label  $2=_build dir  $3=mode: emit|link
  local label="$1" d="$2" mode="$3" entries mods pmis f bad=0 allowed
  entries="$(ls "$d" | grep -c '^entry\.ll$')"
  mods="$(ls "$d" | grep -cE '^mod_[0-9]+\.ll$')"
  pmis="$(ls "$d" | grep -c '\.pmi$')"
  if [ "$entries" -ne 1 ] || [ "$mods" -lt 1 ] || [ "$pmis" -lt 1 ] || [ "$mods" -ne "$pmis" ]; then
    echo "✗ $label: comparison set incomplete — entry.ll=$entries (want 1), module .ll=$mods," >&2
    echo "  .pmi=$pmis (want ≥1 each and equal counts)" >&2
    return 1
  fi
  case "$mode" in
    emit) allowed='^(entry\.ll|mod_[0-9]+\.ll|.+\.pmi)$' ;;
    link) allowed='^(entry\.(ll|o)|mod_[0-9]+\.(ll|o)|foreign_[0-9]+\.o|.+\.pmi)$' ;;
  esac
  for f in $(ls "$d"); do
    if ! printf '%s\n' "$f" | grep -qE "$allowed"; then
      echo "✗ $label: unexpected artifact class in a $mode build: $f" >&2
      bad=1
    fi
  done
  [ "$bad" -eq 0 ] || return 1
  echo "  $label: comparison set OK ($mods module .ll + entry.ll + $pmis .pmi)"
}

# stage-3 ≡ stage-4 (and C3-link ≡ stage-3) share one comparator: union of both sides' artifact
# basenames, so a file present on only one side (module-count drift) fails loudly. Set
# completeness is guaranteed by check_build before this runs.
compare_builds() { # $1=label  $2=dirA  $3=dirB  — returns 0 iff identical; prints DIVERGED lines
  local label="$1" a="$2" b="$3" total=0 bad=0 names f
  names="$( (cd "$a" && ls) ; (cd "$b" && ls) )"
  for f in $(printf '%s\n' "$names" | grep -E '\.(ll|pmi)$' | sort -u); do
    total=$((total + 1))
    if ! cmp -s "$a/$f" "$b/$f"; then
      echo "DIVERGED ($label): $f"
      bad=$((bad + 1))
    fi
  done
  if [ "$total" -eq 0 ]; then
    echo "✗ $label: no artifacts found to compare (empty _build?)" >&2
    return 1
  fi
  echo "  $label: $((total - bad))/$total artifacts byte-identical"
  [ "$bad" -eq 0 ]
}

# --- stage 2: boot builds C2 (bootstrap seed; bytes excluded from the identity) ----------------
# boot runs its own default (full) optimiser here — only C2's *behaviour* matters, so the faster
# binary is the right one.
echo "[1/4] boot → C2 (stage-2 binary) ..."
if ! "$BOOT" native --backend llvm --corefn-dir "$COREFN" --ulib "$PURVASM_LIB" \
    -m "$ENTRY_MODULE" -e "$ENTRY_NAME" --heap-words "$FIXPOINT_HEAP_WORDS" \
    -o "$WORK/c2" > "$WORK/c2.log" 2>&1; then
  echo "✗ boot C2 build failed — log: $WORK/c2.log" >&2
  trap - EXIT
  exit 1
fi
C2="$WORK/c2/app"
stamp "C2 build"

# --- stage 3: C2 emits the pinned closure's artifacts ------------------------------------------
echo "[2/4] C2 → stage-3 artifacts ..."
if ! PURVASM_HEAP_WORDS="$FIXPOINT_HEAP_WORDS" "$C2" build $MODE_FLAG --emit-llvm \
    --entry "$ENTRY_MODULE" --entry-name "$ENTRY_NAME" \
    --corefn-dir "$COREFN" --outdir "$WORK/stage3" > "$WORK/stage3.log" 2>&1; then
  echo "✗ stage-3 (C2) build failed — log: $WORK/stage3.log" >&2
  trap - EXIT
  exit 1
fi
stamp "stage-3 emit (C2, native)"
check_build "stage-3" "$WORK/stage3/_build" emit || { trap - EXIT; echo "  workdir kept: $WORK" >&2; exit 1; }

# --- link C3 (Node-hosted CLI; see the header note) --------------------------------------------
echo "[3/4] link C3 from stage-3 (Node-hosted full build, artifacts asserted ≡ stage-3) ..."
if ! node "$NODE_CLI" build $MODE_FLAG \
    --entry "$ENTRY_MODULE" --entry-name "$ENTRY_NAME" \
    --corefn-dir "$COREFN" --outdir "$WORK/c3link" > "$WORK/c3link.log" 2>&1; then
  echo "✗ C3 link build failed — log: $WORK/c3link.log" >&2
  trap - EXIT
  exit 1
fi
stamp "C3 link build (Node)"
check_build "C3-link" "$WORK/c3link/_build" link || { trap - EXIT; echo "  workdir kept: $WORK" >&2; exit 1; }
if ! compare_builds "C3-link ≡ stage-3" "$WORK/stage3/_build" "$WORK/c3link/_build"; then
  echo "✗ the Node-hosted compiler diverged from C2 (native) on identical input — C3's link" >&2
  echo "  input would not be stage-3's bytes; workdir kept: $WORK" >&2
  trap - EXIT
  exit 1
fi
C3="$WORK/c3link/app"

# --- stage 4: C3 compiles the same closure (emission only) -------------------------------------
echo "[4/4] C3 → stage-4 artifacts ..."
if ! PURVASM_HEAP_WORDS="$FIXPOINT_HEAP_WORDS" "$C3" build $MODE_FLAG --emit-llvm \
    --entry "$ENTRY_MODULE" --entry-name "$ENTRY_NAME" \
    --corefn-dir "$COREFN" --outdir "$WORK/stage4" > "$WORK/stage4.log" 2>&1; then
  echo "✗ stage-4 (C3) build failed — log: $WORK/stage4.log" >&2
  trap - EXIT
  exit 1
fi
stamp "stage-4 emit (C3, native)"
check_build "stage-4" "$WORK/stage4/_build" emit || { trap - EXIT; echo "  workdir kept: $WORK" >&2; exit 1; }

# --- the gate: stage-3 ≡ stage-4 over {mod_*.ll, entry.ll, *.pmi} ------------------------------
if compare_builds "stage-3 ≡ stage-4" "$WORK/stage3/_build" "$WORK/stage4/_build"; then
  echo "★ self-host fixpoint holds ($PROFILE profile)"
  exit 0
else
  echo "✗ fixpoint broken — workdir kept: $WORK" >&2
  trap - EXIT
  exit 1
fi
