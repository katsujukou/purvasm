#!/usr/bin/env bash
# The ADR-0111 slice-1 loader gate: build the owned VM (ADR-0110) as a native executable that hosts
# `dlopen`ed providers (`--host-foreign-api`), then load providers into it.
#
# Two claims are checked, and NEITHER is observable from a unit test — both live in the link and in
# `dlopen`, so they need a natively compiled host:
#
#   - **API coverage / retention** (§1.1). The VM links the runtime as a *static archive* and
#     dead-strips, and it calls almost none of the foreign-author API itself, so the default link
#     drops it. `ApiCoverage.c` references every `pv_*` in `purvasm.h`; loading with `RTLD_NOW` binds
#     every one of those references, so if the retention/export list missed a symbol the load fails
#     and names it. A build that dropped, say, `pv_new_record` passes every other test in the repo.
#   - **the foreign-ABI version contract** (§5). `Marker.c` is built twice from ONE source — against
#     the real `purvasm.h`, and against a copy with `PV_FOREIGN_ABI_VERSION` bumped — and announces
#     itself from a module initialiser. The current-version build must load *and print the marker*
#     (which is what makes the marker's absence mean something); the bumped build must fail to load
#     with the marker ABSENT, since `dlopen` runs initialisers before returning and a version read
#     after the fact would already be too late.
#
# Usage (from the repo root, inside `nix develop`):
#   tools/vm-loader-e2e.sh
#
# Prerequisites (located, not built): the runtime staticlib (`$PURVASM_RT_A` or `cargo build
# --release` in `runtime/`), the staged ulib (`$PURVASM_LIB`), `purvasm.h` (`$PURVASM_INCLUDE`), the
# VM's CoreFn under `$COREFN_DIR` (`spago build -p vm`), `clang`, `node`. macOS/Linux both.
set -u

cd "$(dirname "${BASH_SOURCE[0]}")/.."
ROOT="$(pwd)"

# Emission-affecting knobs are harness-owned (see tools/ffi-e2e.sh): an ambient one changes what the
# compiler emits, and this gate is about what the *link* keeps.
unset PURVASM_BYNEED_OFF PURVASM_EMIT_DEBUG_ABI PURVASM_PROFILE_APPLY

FIX="$ROOT/vm/test/fixtures/loader"
: "${PURVASM_RT_A:=$ROOT/runtime/target/release/libpurvasm_rt.a}"
: "${PURVASM_LIB:=$ROOT/purvasm_lib}"
: "${PURVASM_INCLUDE:=$ROOT/runtime/include}"
: "${COREFN_DIR:=$ROOT/output}"
export PURVASM_RT_A PURVASM_LIB PURVASM_INCLUDE

for f in "$PURVASM_RT_A" "$PURVASM_LIB" "$PURVASM_INCLUDE/purvasm.h" "$COREFN_DIR/Main/corefn.json"; do
  [ -e "$f" ] || { echo "missing prerequisite: $f" >&2; exit 2; }
done

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

case "$(uname -s)" in
  Darwin) SHARED=(-shared -undefined dynamic_lookup) ;;
  *)      SHARED=(-shared -fPIC) ;;
esac

echo "== building the VM with --host-foreign-api =="
if ! node "$ROOT/cli/index.node.js" build --entry Main --corefn-dir "$COREFN_DIR" \
       --outdir "$WORK/vm" --host-foreign-api >"$WORK/build.log" 2>&1; then
  echo "  the VM build failed — see $WORK/build.log" >&2
  tail -20 "$WORK/build.log" >&2
  exit 2
fi
VM="$WORK/vm/app"
[ -x "$VM" ] || { echo "  no executable at $VM" >&2; exit 2; }
grep -F "host-foreign-api: retaining" "$WORK/build.log" | sed 's/^ */  /'

rc=0

build_provider () { # <out.so> <source.c> <module> [include-dir]
  local out="$1" src="$2" module="$3" inc="${4:-$PURVASM_INCLUDE}"
  clang "${SHARED[@]}" -O2 "-DPVF_MODULE=$module" -I"$inc" "$src" -o "$out" 2>"$out.log" \
    || { echo "  clang failed for $src — see $out.log" >&2; tail -5 "$out.log" >&2; return 1; }
}

nm_undefined () { # <file> -> the pv_* symbols it references but does not define
  { llvm-nm -u "$1" 2>/dev/null || nm -u "$1" 2>/dev/null; } | grep -oE 'pv_[A-Za-z0-9_]+' | sort -u
}

# The executable's actual dynamic export set. Mach-O: `-exported_symbols_list` makes every unlisted
# global local, so the external defined symbols ARE the exports. ELF: `.dynsym`'s defined half.
exported_symbols () { # <exe> -> one symbol per line, sorted, `_`-stripped on Mach-O
  if [ "$(uname -s)" = Darwin ]; then
    { llvm-nm --extern-only --defined-only "$1" 2>/dev/null || nm -gU "$1" 2>/dev/null; } \
      | awk '{print $NF}' | sed 's/^_//' | sort -u
  else
    { nm -D --defined-only "$1" 2>/dev/null || llvm-nm --dynamic --defined-only "$1" 2>/dev/null; } \
      | awk '{print $NF}' | sort -u
  fi
}

# Symbols the LINKER defines in an executable, which no allowlist names and no provider may use.
# Listed exhaustively rather than pattern-matched: the point of the comparison below is that
# anything unexpected fails, so the escape hatch must not be able to grow by accident.
LINKER_DEFINED='_init
_fini
_end
_edata
__bss_start
__data_start
__dso_handle
_IO_stdin_used
__TMC_END__
_DYNAMIC
_GLOBAL_OFFSET_TABLE_
__libc_csu_init
__libc_csu_fini'

echo "== export allowlist ≡ what the executable actually exports (§1.1) =="
# This is the SECURITY direction of §1.1, and until now nothing asserted it: the earlier
# implementation exported 635 symbols including the VM's own `pvf_Purvasm_2eVM_2eLoader_2e*`, at
# which point a guest could `foreign import` the trusted loader and hold it (§6's whole purpose).
# Every other leg here is positive — a provider that loads proves nothing about what ELSE is
# reachable — so a regression that re-exported the loader would leave them all green.
#
# The comparison is EXACT in both directions, not a spot check: missing means providers break,
# extra means the allowlist is not the boundary it claims to be.
ALLOW="$WORK/vm/_build/exported_symbols.txt"
[ -f "$ALLOW" ] || ALLOW="$WORK/vm/_build/export.map"
if [ ! -f "$ALLOW" ]; then
  printf '  %-24s -> no export allowlist at %s FAIL\n' export-set "$ALLOW"; rc=1
else
  # No `\b` before the name: Mach-O's list spells each entry `_pv_apply`, and `_` is a word
  # character, so a boundary anchor there matches nothing and the allowlist would silently read as
  # empty — which would make this whole comparison vacuous rather than failing.
  allow_set="$(grep -oE 'pvf?_[A-Za-z0-9_]+' "$ALLOW" | sort -u)"
  actual_set="$(exported_symbols "$VM")"
  missing="$(comm -23 <(printf '%s\n' "$allow_set") <(printf '%s\n' "$actual_set"))"
  extra="$(comm -13 <(printf '%s\n' "$allow_set") <(printf '%s\n' "$actual_set") \
    | grep -vxF "$LINKER_DEFINED")"
  printf '  %-24s -> allowlist %s, exported %s\n' export-set \
    "$(printf '%s\n' "$allow_set" | grep -c .)" "$(printf '%s\n' "$actual_set" | grep -c .)"
  # A vacuous comparison passes both directions, so the sizes are asserted before the diff is read.
  if [ "$(printf '%s\n' "$allow_set" | grep -c .)" -lt 2 ]; then
    printf '  %-24s -> the allowlist parsed as empty (%s) FAIL\n' export-set "$ALLOW"; rc=1
  fi
  if [ -n "$missing" ]; then
    printf '  %-24s -> allowlisted but NOT exported: %s FAIL\n' export-set "$(echo "$missing" | tr '\n' ' ')"; rc=1
  fi
  # Named separately from the generic extra check even though it is a subset of it: this is the
  # failure with a security meaning, and it must say so rather than appear as one line of a diff.
  leaked_trusted="$(printf '%s\n' "$extra" | grep -E 'pvf_Purvasm_2eVM_2eLoader|^pv_g_' || true)"
  if [ -n "$leaked_trusted" ]; then
    printf '  %-24s -> TRUSTED SURFACE EXPORTED: %s FAIL\n' export-set "$(echo "$leaked_trusted" | tr '\n' ' ')"; rc=1
  fi
  if [ -n "$extra" ]; then
    printf '  %-24s -> exported but not allowlisted: %s FAIL\n' export-set "$(echo "$extra" | tr '\n' ' ' | cut -c1-200)"; rc=1
  fi
  [ -n "$missing$extra" ] || printf '  %-24s -> exactly the allowlist, nothing else OK\n' export-set
fi

echo "== API coverage: every pv_* the header declares resolves against the VM (§1.1) =="
if build_provider "$WORK/apicoverage.so" "$FIX/ApiCoverage.c" Test_2eLoader; then
  # "Loads" only proves the symbols the fixture *happens* to reference. The retained set is derived
  # from the header, so an entry added there and not to the fixture would be untested retention —
  # invisible for as long as no user calls it. Compare against the allowlist the build just wrote
  # (located above), which is that derivation's own output rather than a second parse of the header.
  if [ -f "$ALLOW" ]; then
    uncovered="$(comm -23 \
      <(grep -oE 'pv_[A-Za-z0-9_]+' "$ALLOW" | grep -v '^pv_foreign_abi_v' | sort -u) \
      <(nm_undefined "$WORK/apicoverage.so"))"
    if [ -n "$uncovered" ]; then
      printf '  %-24s -> the fixture never references %s FAIL\n' api-coverage "$(echo "$uncovered" | tr '\n' ' ')"; rc=1
    else
      printf '  %-24s -> references every exported pv_* OK\n' api-coverage
    fi
  else
    printf '  %-24s -> no export allowlist at %s FAIL\n' api-coverage "$ALLOW"; rc=1
  fi
  if out="$("$VM" --ffi "$WORK/apicoverage.so" 2>"$WORK/apicoverage.err")"; then
    if printf '%s' "$out" | grep -qF "loaded: $WORK/apicoverage.so"; then
      printf '  %-24s -> loaded OK\n' api-coverage
    else
      printf '  %-24s -> ran but did not report the load FAIL\n' api-coverage; rc=1
    fi
  else
    printf '  %-24s -> the VM refused it FAIL\n' api-coverage; rc=1
    sed 's/^/      /' "$WORK/apicoverage.err" >&2
  fi
else
  rc=1
fi

echo "== foreign-ABI version: the marker fires when it loads, and not when it is refused (§5) =="
# The bumped header is the real mechanism, not a fixture flag: a copy of the shipped `purvasm.h` with
# its version `#define` rewritten is exactly "a module built against a different foreign ABI".
mkdir -p "$WORK/stale-include"
sed 's/^#define PV_FOREIGN_ABI_VERSION .*/#define PV_FOREIGN_ABI_VERSION 99/' \
  "$PURVASM_INCLUDE/purvasm.h" >"$WORK/stale-include/purvasm.h"
if ! grep -qF "#define PV_FOREIGN_ABI_VERSION 99" "$WORK/stale-include/purvasm.h"; then
  echo "  purvasm.h no longer defines PV_FOREIGN_ABI_VERSION — the gate cannot bump what it cannot find" >&2
  exit 2
fi

if build_provider "$WORK/marker.so" "$FIX/Marker.c" Test_2eLoader; then
  if "$VM" --ffi "$WORK/marker.so" >"$WORK/marker.out" 2>"$WORK/marker.err"; then
    if grep -qF "MARKER: provider initialiser ran" "$WORK/marker.err"; then
      printf '  %-24s -> loaded, initialiser ran OK\n' current-version
    else
      printf '  %-24s -> loaded but the marker never fired (the negative below would prove nothing) FAIL\n' current-version; rc=1
    fi
  else
    printf '  %-24s -> the VM refused a current-version provider FAIL\n' current-version; rc=1
    sed 's/^/      /' "$WORK/marker.err" >&2
  fi
else
  rc=1
fi

if build_provider "$WORK/stale.so" "$FIX/Marker.c" Test_2eLoader "$WORK/stale-include"; then
  if "$VM" --ffi "$WORK/stale.so" >"$WORK/stale.out" 2>"$WORK/stale.err"; then
    printf '  %-24s -> loaded (expected a refusal) FAIL\n' stale-version; rc=1
  elif ! grep -qF "different foreign ABI" "$WORK/stale.err"; then
    printf '  %-24s -> refused, but not by version FAIL\n' stale-version; rc=1
    sed 's/^/      /' "$WORK/stale.err" >&2
  elif grep -qF "MARKER: provider initialiser ran" "$WORK/stale.err"; then
    printf '  %-24s -> refused AFTER its initialiser ran FAIL\n' stale-version; rc=1
  else
    printf '  %-24s -> refused before any module code ran OK\n' stale-version
  fi
else
  rc=1
fi

if [ "$rc" -eq 0 ]; then
  echo "★ the VM hosts providers: the whole foreign API resolves, and a stale module cannot run"
else
  echo "✗ a loader leg diverged — see the table above (logs under $WORK until exit)" >&2
fi
exit "$rc"
