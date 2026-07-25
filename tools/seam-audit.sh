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

# Audit one backend directory; prints violations, returns non-zero if any.
audit_dir() {
  local dir="$1" bad=0 f base n
  for req in Safepoint.purs Monad.purs Root.purs Program.purs; do
    [ -f "$dir/$req" ] || { echo "seam-audit: missing pinned file $req" >&2; bad=1; }
  done
  for f in "$dir"/*.purs; do
    base=$(basename "$f")

    # 1) raw call-text construction (string literals building a call instruction)
    n=$(count_matches "$f" "$CALL_RE")
    case "$base" in
      Safepoint.purs)
        [ "$n" -eq 4 ] || { echo "seam-audit: $base: expected exactly 4 call renderers (rtCallWith/rtCallVoid/guestDirect/guestMusttail), found $n" >&2; bad=1; } ;;
      Program.purs)
        [ "$n" -eq 2 ] || { echo "seam-audit: $base: expected exactly 2 raw call constructions, found $n" >&2; bad=1; }
        [ "$(count_matches "$f" 'unsafeEmitRawCall ("  %ctx = call ptr @pv_runtime_new(i64 ' F)" -eq 1 ] \
          || { echo "seam-audit: $base: the ctx-birth construction drifted from its pinned shape" >&2; bad=1; }
        [ "$(count_matches "$f" '"  call void @" <> mangle (gdefInitKey g) <> "$init(ptr %ctx)"' F)" -eq 1 ] \
          || { echo "seam-audit: $base: the pv_init_all \$init-skeleton construction drifted from its pinned shape" >&2; bad=1; } ;;
      *)
        [ "$n" -eq 0 ] || { echo "seam-audit: $base: raw call text outside the seam:" >&2; grep -nE -- "$CALL_RE" "$f" >&2 || true; bad=1; } ;;
    esac

    # 2) unsafeEmitRawCall use sites (non-comment lines, imports included)
    n=$(count_matches "$f" 'unsafeEmitRawCall')
    case "$base" in
      Monad.purs) [ "$n" -eq 4 ] || { echo "seam-audit: $base: unsafeEmitRawCall count drifted (expected 4: export, guard fallthrough, signature, definition; found $n)" >&2; bad=1; } ;;
      Safepoint.purs) [ "$n" -eq 5 ] || { echo "seam-audit: $base: unsafeEmitRawCall count drifted (expected 5: import + 4 renderers; found $n)" >&2; bad=1; } ;;
      Program.purs) [ "$n" -eq 2 ] || { echo "seam-audit: $base: unsafeEmitRawCall count drifted (expected 2: import + ctx birth; found $n)" >&2; bad=1; } ;;
      *) [ "$n" -eq 0 ] || { echo "seam-audit: $base: unsafeEmitRawCall outside the allowlist" >&2; bad=1; } ;;
    esac

    # 3) unsafeEmitRawModule use sites
    n=$(count_matches "$f" 'unsafeEmitRawModule')
    case "$base" in
      Monad.purs) [ "$n" -eq 6 ] || { echo "seam-audit: $base: unsafeEmitRawModule count drifted (expected 6: export, emitModule/emitDefine fallthroughs, doc reference in emit's crash text excluded, signature, definition; found $n)" >&2; bad=1; } ;;
      Program.purs) [ "$n" -eq 2 ] || { echo "seam-audit: $base: unsafeEmitRawModule count drifted (expected 2: import + pv_init_all skeleton; found $n)" >&2; bad=1; } ;;
      *) [ "$n" -eq 0 ] || { echo "seam-audit: $base: unsafeEmitRawModule outside the allowlist" >&2; bad=1; } ;;
    esac

    # 4) popFrame is Root-private (ADR-0105 §2 fused-pop discipline)
    if [ "$base" != "Root.purs" ]; then
      n=$(count_matches "$f" 'popFrame')
      [ "$n" -eq 0 ] || { echo "seam-audit: $base: popFrame outside Root (pops must stay fused with their continuations)" >&2; bad=1; }
    fi

    # 5) openFrame minting sites (ADR-0105 §2 round 4: a frameless init body could otherwise
    #    open a frame the wrapper never pops)
    n=$(count_matches "$f" 'openFrame')
    case "$base" in
      Root.purs) [ "$n" -eq 4 ] || { echo "seam-audit: $base: openFrame count drifted (expected 4: export, signature, definition, framed-init wrapper; found $n)" >&2; bad=1; } ;;
      Emit.purs) [ "$n" -eq 2 ] || { echo "seam-audit: $base: openFrame count drifted (expected 2: import + the plan-driven activation open; found $n)" >&2; bad=1; } ;;
      Program.purs) [ "$n" -eq 2 ] || { echo "seam-audit: $base: openFrame count drifted (expected 2: import + the entry stub; found $n)" >&2; bad=1; } ;;
      *) [ "$n" -eq 0 ] || { echo "seam-audit: $base: openFrame outside its pinned minting sites" >&2; bad=1; } ;;
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
  hits=$(grep -rn 'unsafeEmitRawCall\|unsafeEmitRawModule\|openFrame' "$srcdir" --include='*.purs' \
    | grep -v "^$srcdir/$backendsub/" \
    | grep -vE ':[[:space:]]*--' || true)
  if [ -n "$hits" ]; then
    echo "seam-audit: unsafe emitter or openFrame referenced outside the LLVM backend directory:" >&2
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
