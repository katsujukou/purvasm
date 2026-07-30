#!/usr/bin/env bash
# ADR-0105 §1/§2 source audit for the Level-2 LLVM backend, exact by file × construction shape
# × expected count. Layered with the emission-time guards (`Monad.emit`/`emitModule`/
# `emitDefine` crash on ` call ` text): the guards catch assembled call text at the first
# emission regardless of how the source spells it; this audit pins the *escape hatches* —
# every `unsafeEmitRawCall`/`unsafeEmitRawModule` use site, every raw call-text construction,
# and every `popFrame` reference — so a new bypass cannot appear without editing this allowlist.
#
# The audit self-tests on every run: each embedded violation class is injected into a scratch
# copy of the backend and must be REJECTED before the real tree is checked (a degenerating
# audit fails itself).
set -euo pipefail
cd "$(dirname "$0")/.."

BACKEND=compiler/src/Purvasm/Compiler/Backend/LLVM
CALL_RE='"[^"]*call (i64|void|ptr|tailcc)'

# Count non-comment-line matches of extended-regex $2 (or fixed string with $3=F) in file $1.
count_matches() {
  local flag="${3:-E}"
  grep -vE '^[[:space:]]*--' "$1" | grep -c"$flag" -- "$2" || true
}

# One-pass per-file counts for every pinned identifier (line counts, comment lines skipped) —
# the self-test runs the whole audit once per violation class, so per-identifier grep spawns
# would dominate its wall time.
counts_for() {
  awk '
    /^[[:space:]]*--/ { next }
    {
      if ($0 ~ /"[^"]*call (i64|void|ptr|tailcc)/) c["call"]++
      split("unsafeEmitRawCall unsafeEmitRawModule popFrame openFrame bumpEpoch verifyAt mintAt keyOf unsafeUseVal unsafeMintFresh machineryHandleCall", ids, " ")
      for (i = 1; i <= 11; i++) if (index($0, ids[i]) > 0) c[ids[i]]++
      if (index($0, "unsafeTestVal") > 0 || index($0, "unsafeValText") > 0) c["testesc"]++
    }
    END { for (k in c) print k "=" c[k] }
  ' "$1"
}

# Look a count up in counts_for output (0 when absent).
cnt() { printf '%s
' "$1" | awk -F= -v k="$2" '$1 == k { print $2; found = 1 } END { if (!found) print 0 }'; }

# Audit one backend directory; prints violations, returns non-zero if any.
audit_dir() {
  local dir="$1" bad=0 f base n
  for req in Safepoint.purs Monad.purs Root.purs Program.purs; do
    [ -f "$dir/$req" ] || { echo "seam-audit: missing pinned file $req" >&2; bad=1; }
  done
  # RECURSIVE walk keyed on the path RELATIVE to the backend root (round 3): a nested
  # Backend/LLVM/Internal/Evil.purs must fall to the zero-use catch-all, and a nested file
  # merely NAMED Monad.purs must not inherit the root Monad.purs allowlist.
  local files
  files=$(find "$dir" -name '*.purs' | LC_ALL=C sort)
  for f in $files; do
    base=${f#"$dir"/}
    C=$(counts_for "$f")

    expect() { # expect <name> <count> <message>
      local got
      got=$(cnt "$C" "$1")
      [ "$got" -eq "$2" ] || { echo "seam-audit: $base: $3 (expected $2; found $got)" >&2; bad=1; }
    }

    case "$base" in
      Safepoint.purs)
        expect call 5 "call-renderer count drifted (rtCallWith/rtCallVoid/machineryHandleCall/guestDirect/guestMusttail)"
        expect unsafeEmitRawCall 6 "unsafeEmitRawCall count drifted"
        expect unsafeEmitRawModule 0 "unsafeEmitRawModule outside the allowlist"
        expect popFrame 0 "popFrame outside Root"
        expect openFrame 0 "openFrame outside its pinned minting sites"
        expect bumpEpoch 5 "bumpEpoch count drifted"
        expect verifyAt 0 "verifyAt outside the Monad wrapper"
        expect mintAt 0 "mintAt outside the Monad wrapper"
        expect keyOf 0 "keyOf outside the bind-time key stamps"
        expect unsafeUseVal 6 "unsafeUseVal count drifted"
        expect unsafeMintFresh 4 "unsafeMintFresh count drifted"
        expect testesc 0 "test-only token escape used in src"
        expect machineryHandleCall 4 "machineryHandleCall count drifted"
        ;;
      Monad.purs)
        expect call 0 "raw call text outside the seam"
        expect unsafeEmitRawCall 4 "unsafeEmitRawCall count drifted"
        expect unsafeEmitRawModule 6 "unsafeEmitRawModule count drifted"
        expect popFrame 0 "popFrame outside Root"
        expect openFrame 0 "openFrame outside its pinned minting sites"
        expect bumpEpoch 4 "bumpEpoch count drifted"
        expect verifyAt 2 "verifyAt count drifted (import + the tracked-epoch wrapper)"
        expect mintAt 2 "mintAt count drifted"
        expect keyOf 0 "keyOf outside the bind-time key stamps"
        expect unsafeUseVal 3 "unsafeUseVal count drifted"
        expect unsafeMintFresh 3 "unsafeMintFresh count drifted"
        expect testesc 0 "test-only token escape used in src"
        expect machineryHandleCall 0 "machineryHandleCall outside Root"
        ;;
      Program.purs)
        expect call 2 "raw call-construction count drifted"
        [ "$(count_matches "$f" 'unsafeEmitRawCall ("  %ctx = call ptr @pv_runtime_new(i64 ' F)" -eq 1 ] \
          || { echo "seam-audit: $base: the ctx-birth construction drifted from its pinned shape" >&2; bad=1; }
        [ "$(count_matches "$f" '"  call void @" <> mangle (gdefInitKey g) <> "$init(ptr %ctx)"' F)" -eq 1 ] \
          || { echo "seam-audit: $base: the pv_init_all \$init-skeleton construction drifted from its pinned shape" >&2; bad=1; }
        expect unsafeEmitRawCall 2 "unsafeEmitRawCall count drifted"
        expect unsafeEmitRawModule 2 "unsafeEmitRawModule count drifted"
        expect popFrame 0 "popFrame outside Root"
        expect openFrame 2 "openFrame count drifted (import + the entry stub)"
        expect bumpEpoch 0 "bumpEpoch outside the seam"
        expect verifyAt 0 "verifyAt outside the Monad wrapper"
        expect mintAt 0 "mintAt outside the Monad wrapper"
        expect keyOf 0 "keyOf outside the bind-time key stamps"
        expect unsafeUseVal 0 "unsafeUseVal outside the seam/prim renderers"
        expect unsafeMintFresh 0 "unsafeMintFresh outside the seam/prim renderers"
        expect testesc 0 "test-only token escape used in src"
        expect machineryHandleCall 0 "machineryHandleCall outside Root"
        ;;
      Root.purs)
        expect call 0 "raw call text outside the seam"
        expect unsafeEmitRawCall 0 "unsafeEmitRawCall outside the allowlist"
        expect unsafeEmitRawModule 0 "unsafeEmitRawModule outside the allowlist"
        expect openFrame 4 "openFrame count drifted (export, signature, definition, framed-init wrapper)"
        expect bumpEpoch 0 "bumpEpoch outside the seam"
        expect verifyAt 0 "verifyAt outside the Monad wrapper"
        expect mintAt 0 "mintAt outside the Monad wrapper"
        expect keyOf 0 "keyOf outside the bind-time key stamps"
        expect unsafeUseVal 0 "unsafeUseVal outside the seam/prim renderers"
        expect unsafeMintFresh 0 "unsafeMintFresh outside the seam/prim renderers"
        expect testesc 0 "test-only token escape used in src"
        expect machineryHandleCall 4 "machineryHandleCall count drifted (import + frame + two root arms)"
        ;;
      Emit.purs)
        expect call 0 "raw call text outside the seam"
        expect unsafeEmitRawCall 0 "unsafeEmitRawCall outside the allowlist"
        expect unsafeEmitRawModule 0 "unsafeEmitRawModule outside the allowlist"
        expect popFrame 0 "popFrame outside Root"
        expect openFrame 2 "openFrame count drifted (import + the plan-driven activation open)"
        expect bumpEpoch 0 "bumpEpoch outside the seam"
        expect verifyAt 0 "verifyAt outside the Monad wrapper"
        expect mintAt 0 "mintAt outside the Monad wrapper"
        expect keyOf 0 "keyOf outside the bind-time key stamps"
        expect unsafeUseVal 0 "unsafeUseVal outside the seam/prim renderers"
        expect unsafeMintFresh 0 "unsafeMintFresh outside the seam/prim renderers"
        expect testesc 0 "test-only token escape used in src"
        expect machineryHandleCall 0 "machineryHandleCall outside Root"
        ;;
      Prim.purs)
        expect call 0 "raw call text outside the seam"
        expect unsafeUseVal 2 "unsafeUseVal count drifted"
        expect unsafeMintFresh 2 "unsafeMintFresh count drifted"
        expect verifyAt 0 "verifyAt outside the Monad wrapper"
        expect mintAt 0 "mintAt outside the Monad wrapper"
        expect keyOf 0 "keyOf outside the bind-time key stamps"
        expect testesc 0 "test-only token escape used in src"
        expect popFrame 0 "popFrame outside Root"
        expect openFrame 0 "openFrame outside its pinned minting sites"
        expect bumpEpoch 0 "bumpEpoch outside the seam"
        expect machineryHandleCall 0 "machineryHandleCall outside Root"
        expect unsafeEmitRawCall 0 "unsafeEmitRawCall outside the allowlist"
        expect unsafeEmitRawModule 0 "unsafeEmitRawModule outside the allowlist"
        ;;
      Value.purs)
        expect call 0 "raw call text outside the seam"
        expect verifyAt 4 "verifyAt count drifted"
        expect mintAt 3 "mintAt count drifted"
        expect keyOf 4 "keyOf count drifted"
        expect testesc 6 "test-escape count drifted"
        expect unsafeUseVal 0 "unsafeUseVal outside the seam/prim renderers"
        expect unsafeMintFresh 0 "unsafeMintFresh outside the seam/prim renderers"
        expect popFrame 0 "popFrame outside Root"
        expect openFrame 0 "openFrame outside its pinned minting sites"
        expect bumpEpoch 0 "bumpEpoch outside the seam"
        expect machineryHandleCall 0 "machineryHandleCall outside Root"
        expect unsafeEmitRawCall 0 "unsafeEmitRawCall outside the allowlist"
        expect unsafeEmitRawModule 0 "unsafeEmitRawModule outside the allowlist"
        ;;
      Types.purs)
        expect call 0 "raw call text outside the seam"
        expect keyOf 3 "keyOf count drifted (import + the two direct-bind key stamps)"
        expect verifyAt 0 "verifyAt outside the Monad wrapper"
        expect mintAt 0 "mintAt outside the Monad wrapper"
        expect testesc 0 "test-only token escape used in src"
        expect unsafeUseVal 0 "unsafeUseVal outside the seam/prim renderers"
        expect unsafeMintFresh 0 "unsafeMintFresh outside the seam/prim renderers"
        expect popFrame 0 "popFrame outside Root"
        expect openFrame 0 "openFrame outside its pinned minting sites"
        expect bumpEpoch 0 "bumpEpoch outside the seam"
        expect machineryHandleCall 0 "machineryHandleCall outside Root"
        expect unsafeEmitRawCall 0 "unsafeEmitRawCall outside the allowlist"
        expect unsafeEmitRawModule 0 "unsafeEmitRawModule outside the allowlist"
        ;;
      *)
        expect call 0 "raw call text outside the seam"
        expect unsafeEmitRawCall 0 "unsafeEmitRawCall outside the allowlist"
        expect unsafeEmitRawModule 0 "unsafeEmitRawModule outside the allowlist"
        expect popFrame 0 "popFrame outside Root"
        expect openFrame 0 "openFrame outside its pinned minting sites"
        expect bumpEpoch 0 "bumpEpoch outside the seam"
        expect verifyAt 0 "verifyAt outside the Monad wrapper"
        expect mintAt 0 "mintAt outside the Monad wrapper"
        expect keyOf 0 "keyOf outside the bind-time key stamps"
        expect unsafeUseVal 0 "unsafeUseVal outside the seam/prim renderers"
        expect unsafeMintFresh 0 "unsafeMintFresh outside the seam/prim renderers"
        expect testesc 0 "test-only token escape used in src"
        expect machineryHandleCall 0 "machineryHandleCall outside Root"
        ;;
    esac
  done
  return "$bad"
}

# The unsafe emitters and `openFrame` are exported (the seam, the ctx-birth line and the
# pinned minting sites need them), so a module ANYWHERE under compiler/src could import them
# and pass a backend-only audit — scan the whole source tree recursively; outside the backend
# directory the identifiers must not appear at all.
audit_wide() {
  local srcdir="$1" backendsub="$2" bad=0 hits
  hits=$(grep -rn 'unsafeEmitRawCall\|unsafeEmitRawModule\|openFrame\|bumpEpoch\|verifyAt\|mintAt\|keyOf\|unsafeUseVal\|unsafeMintFresh\|unsafeTestVal\|unsafeValText\|machineryHandleCall' "$srcdir" --include='*.purs' \
    | grep -v "^$srcdir/$backendsub/" \
    | grep -vE ':[[:space:]]*--' || true)
  if [ -n "$hits" ]; then
    echo "seam-audit: unsafe emitter, openFrame or bumpEpoch referenced outside the LLVM backend directory:" >&2
    echo "$hits" >&2
    bad=1
  fi
  return "$bad"
}

# ---- self-test: every violation class must be rejected on a scratch copy -----------------------
selftest() {
  local scratch base ok=0
  inject() { # $1 = description, $2 = file to write/append, $3 = content
    scratch=$(mktemp -d)
    cp "$BACKEND"/*.purs "$scratch"/
    mkdir -p "$(dirname "$scratch/$2")"
    printf '%s\n' "$3" >> "$scratch/$2"
    if audit_dir "$scratch" > /dev/null 2>&1; then
      echo "seam-audit: SELF-TEST FAILED — not rejected: $1" >&2
      ok=1
    fi
    rm -rf "$scratch"
  }
  inject "raw call text in a new file" "Evil.purs" 'x = emit ("  %t1 = call i64 @evil(ptr %ctx)")'
  inject "column-zero raw call text in a new file" "Evil.purs" 'x = emit ("call i64 @evil(ptr %ctx)")'
  inject "raw call text appended to Emit.purs" "Emit.purs" 'evil = emit ("  %t1 = call i64 @evil(ptr %ctx)")'
  inject "unsafeEmitRawCall outside the allowlist" "Evil.purs" 'x = unsafeEmitRawCall "  whatever"'
  inject "extra unsafeEmitRawCall in Program.purs" "Program.purs" 'evil = unsafeEmitRawCall "  whatever"'
  inject "unsafeEmitRawModule outside the allowlist" "Evil.purs" 'x = unsafeEmitRawModule "chunk"'
  inject "popFrame use outside Root" "Emit.purs" 'evil tok = popFrame tok'
  inject "openFrame outside its pinned minting sites" "Liveness.purs" 'evil = openFrame'
  inject "an extra openFrame in Program.purs" "Program.purs" 'evil = openFrame'
  inject "bumpEpoch outside the seam" "Emit.purs" 'evil = bumpEpoch'
  inject "verifyAt with a spoofable epoch outside Monad" "Emit.purs" 'evil v = verifyAt 0 v'
  inject "a free-form mintAt outside Monad" "Emit.purs" 'evil s = mintAt 0 s'
  inject "unsafeUseVal outside the seam renderers" "Emit.purs" 'evil v = unsafeUseVal v'
  inject "a test-only token escape in src" "Emit.purs" 'evil s = unsafeTestVal s'
  inject "machineryHandleCall outside Root" "Emit.purs" 'evil = machineryHandleCall'
  inject "a nested backend submodule smuggling a caged identifier" "Internal/Evil.purs" 'evil v = unsafeUseVal v'
  inject "a nested file named after an allowlisted root file" "Internal/Monad.purs" 'evil s = mintAt 0 s'
  inject "a second \$init-skeleton construction in Program.purs" "Program.purs" 'evil g = "  call void @" <> mangle (gdefInitKey g) <> "$init(ptr %ctx)"'

  # the wide scan must reject unsafe-emitter and openFrame imports outside the backend directory
  for wide_bad in 'x = unsafeEmitRawCall "  smuggled"' 'x = openFrame'; do
    scratch=$(mktemp -d)
    mkdir -p "$scratch/src/Other" "$scratch/src/Backend/LLVM"
    cp "$BACKEND"/*.purs "$scratch/src/Backend/LLVM/"
    printf '%s\n' "$wide_bad" > "$scratch/src/Other/Evil.purs"
    if audit_wide "$scratch/src" "Backend/LLVM" > /dev/null 2>&1; then
      echo "seam-audit: SELF-TEST FAILED — not rejected outside the backend directory: $wide_bad" >&2
      ok=1
    fi
    rm -rf "$scratch"
  done

  # and the clean tree must pass its own copy
  scratch=$(mktemp -d)
  cp "$BACKEND"/*.purs "$scratch"/
  if ! audit_dir "$scratch" > /dev/null 2>&1; then
    echo "seam-audit: SELF-TEST FAILED — clean copy rejected" >&2
    audit_dir "$scratch" || true
    ok=1
  fi
  rm -rf "$scratch"
  return "$ok"
}

selftest || { echo "seam-audit: self-test failed" >&2; exit 1; }
status=0
audit_dir "$BACKEND" || status=1
audit_wide compiler/src "Purvasm/Compiler/Backend/LLVM" || status=1
[ "$status" -eq 0 ] || exit 1
echo "seam-audit: OK (self-test passed; call text, unsafe emitters, popFrame and openFrame all within the pinned allowlist)"
