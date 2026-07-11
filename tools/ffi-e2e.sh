#!/usr/bin/env bash
# The ADR-0091 user-defined native FFI end-to-end harness: build the app-C and app-Rust FFI fixtures to a
# native binary with the Level-2 `purvasm build`, run each, and assert the result. This is the regression
# guard the `tools/native-run-diff.sh` differential cannot be — that corpus is `Prim`-only with no foreign
# providers, whereas here the whole provider-map path is exercised: sibling `.c` discovery + `-DPVF_MODULE`
# + `PVF_EXPORT` (app-C), and the `--rust-ffi` crate → `purvasm-bundle` cargo build + `nm` audit (app-Rust).
#
# Usage (from the repo root, inside `nix develop`):
#   tools/ffi-e2e.sh
#
# Prerequisites (located, not built): the runtime staticlib (`$PURVASM_RT_A` or `cargo build --release` in
# `runtime/`), the staged ulib (`$PURVASM_LIB`), `purvasm.h` (`runtime/include`), `clang`, `purs`, `node`.
# The **app-Rust** leg additionally needs `cargo` and the runtime crate source (`$PURVASM_RT_CRATE`, else
# `runtime/`); it is SKIPPED (not failed) when `cargo` is absent, so the C leg still guards without Rust.
set -u

cd "$(dirname "${BASH_SOURCE[0]}")/.."
ROOT="$(pwd)"

FIX="$ROOT/compiler/test/fixtures/ffi-e2e"
: "${PURVASM_RT_A:=$ROOT/runtime/target/release/libpurvasm_rt.a}"
: "${PURVASM_LIB:=$ROOT/purvasm_lib}"
: "${PURVASM_INCLUDE:=$ROOT/runtime/include}"
: "${PURVASM_RT_CRATE:=$ROOT/runtime}"
export PURVASM_RT_A PURVASM_LIB PURVASM_INCLUDE PURVASM_RT_CRATE

for tool in "$PURVASM_RT_A" "$PURVASM_LIB"; do
  [ -e "$tool" ] || { echo "missing prerequisite: $tool" >&2; exit 2; }
done

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# Compile each fixture's CoreFn into its OWN workspace (the sibling `.js` FFI stubs let `purs` accept the
# foreign imports). Separate workspaces because C-xor-Rust is a *workspace* policy (ADR-0091 §Addendum): a
# `--rust-ffi` build errors if the same workspace also holds any C sibling, so the app-C fixture (`AppC.c`)
# must not share a workspace with the app-Rust one.
mkdir -p "$WORK/c" "$WORK/rust" "$WORK/mixed"
( cd "$WORK/c" && purs compile --codegen corefn "$FIX"/AppC.purs >/dev/null 2>&1 ) \
  && ( cd "$WORK/rust" && purs compile --codegen corefn "$FIX"/AppRust.purs >/dev/null 2>&1 ) \
  && ( cd "$WORK/mixed" && purs compile --codegen corefn "$FIX"/AppC.purs "$FIX"/AppRust.purs >/dev/null 2>&1 ) \
  || { echo "purs compile of the FFI fixtures failed" >&2; exit 2; }

rc=0
build_run () { # <label> <module> <expected> <corefn-dir> [extra purvasm args…]
  local label="$1" mod="$2" expect="$3" corefn="$4"; shift 4
  local out dir="$WORK/out-$mod"
  if node "$ROOT/cli/index.node.js" build --value --entry "$mod" --entry-name answer \
       --corefn-dir "$corefn" --outdir "$dir" "$@" >"$dir.log" 2>&1 && [ -x "$dir/app" ]; then
    out="$("$dir/app" 2>/dev/null)"
  else
    out="BUILD-FAILED (see $dir.log)"
  fi
  if [ "$out" = "$expect" ]; then
    printf '  %-8s %-10s -> %-8s OK\n' "$label" "$mod" "$out"
  else
    printf '  %-8s %-10s -> %-8s FAIL (expected %s)\n' "$label" "$mod" "$out" "$expect"; rc=1
  fi
}

build_expect_error () { # <label> <module> <corefn-dir> <needle> [extra purvasm args…]
  local label="$1" mod="$2" corefn="$3" needle="$4"; shift 4
  local dir="$WORK/err-$mod-$$"
  if node "$ROOT/cli/index.node.js" build --value --entry "$mod" --entry-name answer \
       --corefn-dir "$corefn" --outdir "$dir" "$@" >"$dir.log" 2>&1; then
    printf '  %-22s -> BUILD-SUCCEEDED (expected an error) FAIL\n' "$label"; rc=1
  elif grep -qF -e "$needle" "$dir.log"; then
    printf '  %-22s -> errored as expected OK\n' "$label"
  else
    printf '  %-22s -> wrong error (want %q; see %s) FAIL\n' "$label" "$needle" "$dir.log"; rc=1
  fi
}

echo "== app-C (sibling .c, PVF_EXPORT) =="
build_run app-C AppC 42 "$WORK/c/output"

echo "== app-Rust (--rust-ffi crate, purvasm-bundle) =="
if command -v cargo >/dev/null 2>&1; then
  # The checked-in fixture crate carries only its source; write its Cargo.toml with the repo-absolute
  # `purvasm-foreign` path into a scratch copy (never mutating the fixture).
  CRATE="$WORK/rust-crate"; mkdir -p "$CRATE/src"
  cp "$FIX/rust-crate/src/lib.rs" "$CRATE/src/lib.rs"
  cat > "$CRATE/Cargo.toml" <<EOF
[package]
name = "ffi-e2e-crate"
version = "0.0.0"
edition = "2021"
publish = false

[dependencies]
purvasm-foreign = { path = "$ROOT/crates/purvasm-foreign" }
EOF
  build_run app-Rust AppRust 42 "$WORK/rust/output" --rust-ffi "$CRATE"
else
  echo "  app-Rust  AppRust    -> SKIPPED (cargo not found)"
fi

# Negative cases — no cargo needed: both errors fire before the bundle is built (the ambiguity scan and
# the `--runtime-lib` rejection precede `buildBundle`), so `--rust-ffi` may point at any directory here.
echo "== negative (C-xor-Rust workspace policy; --runtime-lib rejected) =="
# An unused C sibling (AppC.c) in the same workspace as the Rust build is still a mixing error.
build_expect_error "ambiguity(C+Rust)" AppRust "$WORK/mixed/output" "ambiguous foreign FFI" --rust-ffi "$WORK/mixed"
# --runtime-lib names a staticlib app-Rust never links.
build_expect_error "reject-runtime-lib" AppRust "$WORK/rust/output" "--runtime-lib is not used" \
  --rust-ffi "$WORK" --runtime-lib "$PURVASM_RT_A"

if [ "$rc" -eq 0 ]; then
  echo "★ user-FFI native binaries all produced the expected result"
else
  echo "✗ a user-FFI leg diverged — see the table above (logs in the temp workdir until exit)" >&2
fi
exit "$rc"
