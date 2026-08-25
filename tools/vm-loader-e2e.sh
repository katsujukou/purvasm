#!/usr/bin/env bash
# The ADR-0111 slice-1 loader gate: build the owned VM (ADR-0110) as a native executable that hosts
# `dlopen`ed providers (`--host-foreign-api`), then load providers into it.
#
# Three claims are checked, and NONE is observable from a unit test — they live in the link, in
# `dlopen`, and in a real runtime leaf, so they need a natively compiled host:
#
#   - **API coverage / retention** (§1.1). The VM links the runtime as a *static archive* and
#     dead-strips, and it calls almost none of the foreign-author API itself, so the default link
#     drops it. `ApiCoverage.c` references every `pv_*` in `purvasm.h`; loading with `RTLD_NOW` binds
#     every one of those references, so if the retention/export list missed a symbol the load fails
#     and names it. A build that dropped, say, `pv_new_record` passes every other test in the repo.
#   - **runtime leaves** (§1.1/§2). The corpus's own providers — `show`, stdio, FS, `argv` — live in
#     the runtime staticlib this executable already links, so they resolve through `host-runtime`
#     with no module loaded at all. The VM's guest program prints 42 through two of them.
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

# The VM build is SHARED, not owned: `$PURVASM_VM_DIR` names an existing `purvasm build --outdir`
# directory to reuse. A directory rather than an executable, because this gate inspects the link's
# own artifacts (the export allowlist) and not only the binary. Unset, the script builds its own and
# stays runnable alone — `tools/vm-e2e.sh` is what builds once and hands the same directory to both
# gates.
if [ -n "${PURVASM_VM_DIR:-}" ]; then
  VMDIR="$PURVASM_VM_DIR"
  echo "== using the VM build at $VMDIR =="
  [ -x "$VMDIR/app" ] || { echo "  no executable at $VMDIR/app" >&2; exit 2; }
else
  VMDIR="$WORK/vm"
  echo "== building the VM with --host-foreign-api =="
  if ! node "$ROOT/cli/index.node.js" build --entry Main --corefn-dir "$COREFN_DIR" \
         --outdir "$VMDIR" --host-foreign-api >"$WORK/build.log" 2>&1; then
    echo "  the VM build failed — see $WORK/build.log" >&2
    tail -20 "$WORK/build.log" >&2
    exit 2
  fi
  grep -F "host-foreign-api: retaining" "$WORK/build.log" | sed 's/^ */  /'
fi
VM="$VMDIR/app"

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
ALLOW="$VMDIR/_build/exported_symbols.txt"          # Mach-O: -exported_symbols_list
[ -f "$ALLOW" ] || ALLOW="$VMDIR/_build/dynamic.list" # ELF: --dynamic-list
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
  # failure with a security meaning, and it must say so rather than appear as one line of a diff. The
  # pattern covers the whole `Purvasm.VM.*` namespace, not just the loader: `Purvasm.VM.Foreign`'s
  # `applyImpl` calls an arbitrary runtime closure, so exporting it would hand a guest the same reach.
  leaked_trusted="$(printf '%s\n' "$extra" | grep -E 'pvf_Purvasm_2eVM_2e|^pv_g_' || true)"
  if [ -n "$leaked_trusted" ]; then
    printf '  %-24s -> TRUSTED SURFACE EXPORTED: %s FAIL\n' export-set "$(echo "$leaked_trusted" | tr '\n' ' ')"; rc=1
  fi
  if [ -n "$extra" ]; then
    printf '  %-24s -> exported but not allowlisted: %s FAIL\n' export-set "$(echo "$extra" | tr '\n' ' ' | cut -c1-200)"; rc=1
  fi
  [ -n "$missing$extra" ] || printf '  %-24s -> exactly the allowlist, nothing else OK\n' export-set

  # The HOST-CONTROL surface (`purvasm_host.h`, ADR-0110 §4(a) Correction), named on its own even
  # though the exact comparison above would already catch it. These entries configure the runtime
  # *for* a guest — `pv_runtime_set_guest_argv` rewrites what the guest's `argvImpl` reports — so one
  # of them appearing in the dynamic exports would let a loaded provider reach past its own program
  # into the runner's decisions. Every other leg here is positive and none would notice.
  host_control="$(printf '%s\n' "$actual_set" | grep -E '^pv_runtime_' || true)"
  if [ -n "$host_control" ]; then
    printf '  %-24s -> HOST-CONTROL API EXPORTED: %s FAIL\n' host-control "$(echo "$host_control" | tr '\n' ' ')"; rc=1
  else
    printf '  %-24s -> the runtime lifecycle and argv setter stay unexported OK\n' host-control
  fi
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

echo "== runtime leaves: the corpus's own providers, with NO module loaded (§1.1/§2) =="
# The slice-2 milestone, and the cheapest possible test of §1.1's retention: the runtime staticlib
# linked into this executable is itself a provider class, so `show` and stdio resolve through
# `host-runtime` with no `--ffi` and no manifest. The VM's built-in guest program applies `showIntImpl`
# to 42, hands the resulting CARRIER straight to `writeLineImpl` without decoding it (§3), and runs the
# effect thunk it gets back. So a printed 42 means resolution, firing, argument conversion, the carrier
# pass-through and the effect run all worked — and none of it is reachable from a unit test.
if out="$("$VM" 2>"$WORK/leaves.err")"; then
  # One assertion per boundary arm a leaf actually READS, because "it ran" is not the claim — the
  # claim is that a VM scalar already is a runtime value of the right representation, and only a leaf
  # reading it can say so. `floatBitsHi 1.0` is the sharpest of the three: 1072693248 is 0x3FF00000,
  # the high half of IEEE-754 1.0, which a wrong `Number` representation could not produce by chance.
  arms_ok=yes
  for expected in "boundary: a VM string" "42" "1072693248"; do
    printf '%s' "$out" | grep -qxF "$expected" || {
      printf '  %-24s -> a leaf never read: %s FAIL\n' runtime-leaves "$expected"; rc=1; arms_ok=no
    }
  done
  if [ "$arms_ok" = yes ]; then
    printf '  %-24s -> string/int/number read by real leaves OK\n' runtime-leaves
  else
    printf '%s\n' "$out" | sed 's/^/      /' >&2
  fi
  # The result of a leaf stays opaque: decoding it would break the invariant promotion exists to
  # protect, so the entry reports `<value>` rather than the string it holds (§3's "coming out").
  if printf '%s' "$out" | grep -qF "leaf result: <value>"; then
    printf '  %-24s -> the leaf result stayed a carrier OK\n' runtime-leaves
  else
    printf '  %-24s -> the leaf result was decoded, or absent FAIL\n' runtime-leaves; rc=1
  fi
else
  printf '  %-24s -> the VM could not run its own leaves FAIL\n' runtime-leaves; rc=1
  sed 's/^/      /' "$WORK/leaves.err" >&2
fi

# A resolution cache that answered without re-checking the arity would reuse the arity-1 closure for
# a later mention at arity 3, and a leaf indexes its argument vector by the arity its closure was
# built with — so this is the difference between a refused image and a native read past the end of
# the arguments. It runs as a SEPARATE process because a stuck run cannot be caught in-process:
# purvasm's `Effect.Exception` is a throw-only shadow (ADR-0074), so the refusal IS the exit.
if "$VM" --self-test arity-mismatch >"$WORK/mismatch.out" 2>"$WORK/mismatch.err"; then
  printf '  %-24s -> an arity disagreement was NOT refused FAIL\n' arity-mismatch; rc=1
  sed 's/^/      /' "$WORK/mismatch.out" >&2
elif grep -qF "but was resolved at arity" "$WORK/mismatch.err"; then
  printf '  %-24s -> refused, naming both arities OK\n' arity-mismatch
else
  printf '  %-24s -> refused for the wrong reason FAIL\n' arity-mismatch; rc=1
  sed 's/^/      /' "$WORK/mismatch.err" >&2
fi
echo "== loaded providers: both classes searched, exactly one may answer (§4) =="
# Slice 3's first half. A key the runtime does NOT define can only come from the loaded module, so a
# correct answer here is proof that resolution spans both provider classes — and `describeBoolImpl`
# reads the boundary's `Boolean`, the one supported arm no runtime leaf can exercise (nothing in
# runtime/src/leaf.rs reads `pv_bool_payload`).
if build_provider "$WORK/provider.so" "$FIX/Provider.c" Test_2eLoader; then
  if out="$("$VM" --self-test loaded-provider --ffi "$WORK/provider.so" 2>"$WORK/provider.err")"; then
    if printf '%s' "$out" | grep -qxF "provider read Boolean true"; then
      printf '  %-24s -> a loaded leaf ran, and read a Boolean OK\n' loaded-provider
    else
      printf '  %-24s -> the loaded leaf did not run FAIL\n' loaded-provider; rc=1
      printf '%s\n' "$out" | sed 's/^/      /' >&2
    fi
  else
    printf '  %-24s -> the VM refused a valid provider FAIL\n' loaded-provider; rc=1
    sed 's/^/      /' "$WORK/provider.err" >&2
  fi
else
  rc=1
fi

# The runtime-shadow collision (§4): a module exporting a key the runtime already defines. Neither may
# win — not by archive order, not by load order — because "which `show` am I running?" is not a
# question a user should have to ask. Built with -DPVF_MODULE=Data_2eShow so the symbol really is the
# runtime's own, and run as its own process because the refusal is a stuck run (ADR-0074).
if build_provider "$WORK/shadow.so" "$FIX/Shadow.c" Data_2eShow; then
  if "$VM" --self-test runtime-shadow --ffi "$WORK/shadow.so" >"$WORK/shadow.out" 2>"$WORK/shadow.err"; then
    printf '  %-24s -> a shadowed key resolved anyway FAIL\n' runtime-shadow; rc=1
    sed 's/^/      /' "$WORK/shadow.out" >&2
  elif grep -qF "provided by both host-runtime and" "$WORK/shadow.err"; then
    printf '  %-24s -> refused, naming both providers OK\n' runtime-shadow
  else
    printf '  %-24s -> refused for the wrong reason FAIL\n' runtime-shadow; rc=1
    sed 's/^/      /' "$WORK/shadow.err" >&2
  fi
else
  rc=1
fi

echo "== array promotion: one object, every alias (§3) =="
# The identity invariant, which is the reason arrays are promoted rather than converted. Every leg
# below would still pass under an elementwise copy EXCEPT these: a copy would leave the leaf writing
# to a corpse, and each alias reading the old element.
if "$VM" --self-test aliasing --ffi "$WORK/provider.so" >"$WORK/alias.out" 2>"$WORK/alias.err"; then
  written=$(grep -c "^written by the leaf$" "$WORK/alias.out" || true)
  if [ "$written" -eq 3 ]; then
    printf '  %-24s -> a leaf write is visible through all 3 aliases OK\n' aliasing
  else
    printf '  %-24s -> only %s of 3 aliases saw the write FAIL\n' aliasing "$written"; rc=1
    sed 's/^/      /' "$WORK/alias.out" >&2
  fi
  # And the reverse: the VM's own SetArray, on an array that has already crossed, must reach the same
  # object — otherwise promotion would be a one-way mirror.
  if grep -qxF "written by the VM" "$WORK/alias.out"; then
    printf '  %-24s -> a VM write is visible to the leaf OK\n' aliasing
  else
    printf '  %-24s -> the VM write never reached the leaf FAIL\n' aliasing; rc=1
  fi
else
  printf '  %-24s -> the aliasing program did not run FAIL\n' aliasing; rc=1
  sed 's/^/      /' "$WORK/alias.err" >&2
fi

# Steps 1 and 3 of the migration: an empty array (no blank-array constructor exists to build one) and
# an array containing itself. A cycle that did not terminate would HANG rather than fail, so this
# completing at all is the assertion; the timeout is the backstop that turns a hang into a failure.
if timeout 60 "$VM" --self-test cyclic --ffi "$WORK/provider.so" >"$WORK/cyclic.out" 2>"$WORK/cyclic.err"; then
  if grep -qxF "0" "$WORK/cyclic.out" && grep -qxF "1" "$WORK/cyclic.out"; then
    printf '  %-24s -> empty and self-referential arrays both promoted OK\n' cyclic-empty
  else
    printf '  %-24s -> promoted, but the leaf measured the wrong lengths FAIL\n' cyclic-empty; rc=1
    sed 's/^/      /' "$WORK/cyclic.out" >&2
  fi
else
  status=$?
  if [ "$status" -eq 124 ]; then
    printf '  %-24s -> HUNG: the cycle did not terminate FAIL\n' cyclic-empty; rc=1
  else
    printf '  %-24s -> refused (exit %s) FAIL\n' cyclic-empty "$status"; rc=1
    sed 's/^/      /' "$WORK/cyclic.err" >&2
  fi
fi

echo "== carrier elimination: leaf values consumed by ordinary bytecode (§3) =="
# Slice 4. Nothing here is a new instruction — each site meets a carrier where it used to meet a VM
# value and decodes it by demanding the shape it already required. The two array entrances are both
# exercised: an array the LEAF returned is a carrier from birth, so `SetArray` reaches it without any
# promotion having happened, which is a different path from the aliasing leg above.
#
# Note what is NOT claimed: the VM's own `describe` still prints a carrier as `<value>`, because it
# has no type to demand with. Making a carrier printable at the terminal is ADR-0110 §5's typed
# terminal demand, not this slice.
if "$VM" --self-test carrier-elimination --ffi "$WORK/provider.so" >"$WORK/elim.out" 2>"$WORK/elim.err"; then
  elim_ok=yes
  # 1072693249 = 0x3FF00001: a carrier Int decoded, then 1 added to the payload by an ordinary AddInt.
  # "2" is LengthArray over an array the leaf returned; "from the leaf" is IndexArray over the same.
  for expected in "1072693249" "2" "from the leaf"; do
    grep -qxF "$expected" "$WORK/elim.out" || {
      printf '  %-24s -> a site never consumed the carrier: %s FAIL\n' carrier-elim "$expected"; rc=1; elim_ok=no
    }
  done
  # Twice: once read back by the VM's own IndexArray, once by the LEAF off the same object. One
  # occurrence would mean the write and the read disagreed about which array they were talking about.
  if [ "$(grep -cxF "set on a leaf array" "$WORK/elim.out")" -ne 2 ]; then
    printf '  %-24s -> a SetArray on a leaf-returned array did not reach one object FAIL\n' carrier-elim; rc=1; elim_ok=no
  fi
  if [ "$elim_ok" = yes ]; then
    printf '  %-24s -> scalars, length, index and SetArray all decoded OK\n' carrier-elim
  else
    sed 's/^/      /' "$WORK/elim.out" >&2
  fi
else
  printf '  %-24s -> the elimination program did not run FAIL\n' carrier-elim; rc=1
  sed 's/^/      /' "$WORK/elim.err" >&2
fi

# The CONTROL sites, one arm each. A site that branches on a leaf's value is an elimination site
# just as much as one that computes with it, and none of them was covered until a review found
# `Guarded` reading a carrier as a non-boolean. Each expected line names its site, and each WRONG line
# is a branch that must never be taken — so a site that silently stopped decoding shows up as either a
# missing line or a wrong one, not as a pass.
if "$VM" --self-test carrier-control --ffi "$WORK/provider.so" >"$WORK/control.out" 2>"$WORK/control.err"; then
  control_ok=yes
  for expected in \
    "jumpUnless: took the true branch" \
    "guarded: true clause fired" \
    "guarded: false fell through" \
    "switchLit: matched the leaf's Int" \
    "switchLen: matched the leaf's array" \
    "projArray: read from the leaf's array"; do
    grep -qxF "$expected" "$WORK/control.out" || {
      printf '  %-24s -> a control site did not decode: %s FAIL\n' carrier-control "$expected"; rc=1; control_ok=no
    }
  done
  if grep -q "WRONG" "$WORK/control.out"; then
    printf '  %-24s -> a branch that must never be taken fired FAIL\n' carrier-control; rc=1; control_ok=no
  fi
  [ "$control_ok" = yes ] && printf '  %-24s -> jumpUnless, guarded, switchLit, switchLen, projArray OK\n' carrier-control
  [ "$control_ok" = yes ] || sed 's/^/      /' "$WORK/control.out" >&2
else
  printf '  %-24s -> the control program did not run FAIL\n' carrier-control; rc=1
  sed 's/^/      /' "$WORK/control.err" >&2
fi

echo "== data values: both directions, both constructor shapes (§3) =="
# Slice 5, and the reason `pv_adt_tag` was added to the ABI: a data value a leaf returned is opaque,
# so `SwitchCtor` compares the value's TAG against each arm's `ctorTag name` rather than comparing
# names. Before this a leaf could not return a `Maybe` at all.
#
# `Just` and `Nothing` are represented differently — a heap ADT and a bare immediate — and nothing in
# the VM can tell which it holds, so both arms have to be driven: a gate that only tried `Just` would
# pass with the accessor's immediate case broken.
if "$VM" --self-test data-leaves --ffi "$WORK/provider.so" >"$WORK/data.out" 2>"$WORK/data.err"; then
  data_ok=yes
  for expected in \
    "found by the leaf" \
    "dispatch: took Nothing" \
    "leaf received Just" \
    "leaf received Nothing"; do
    grep -qxF "$expected" "$WORK/data.out" || {
      printf '  %-24s -> a data path did not work: %s FAIL\n' data-leaves "$expected"; rc=1; data_ok=no
    }
  done
  # "WRONG" covers both a dispatch that took an impossible branch AND the fixture's third outcome —
  # a tag that is neither constructor, which is what an outbound nullary tag getting broken looks
  # like. Without that third outcome the leaf would report any wrong tag as a correct `Nothing`.
  if grep -q "WRONG" "$WORK/data.out"; then
    printf '  %-24s -> a wrong branch or an unrecognised tag FAIL\n' data-leaves; rc=1; data_ok=no
  fi
  [ "$data_ok" = yes ] && printf '  %-24s -> SwitchCtor, Proj and toPv over Just and Nothing OK\n' data-leaves
  [ "$data_ok" = yes ] || sed 's/^/      /' "$WORK/data.out" >&2
else
  printf '  %-24s -> the data program did not run FAIL\n' data-leaves; rc=1
  sed 's/^/      /' "$WORK/data.err" >&2
fi

# A negative field index must be refused identically whichever representation the data value has.
# The carrier path is the one with teeth: its accessor adds 1 to reach past the tag, so `-1` would
# address the tag slot and hand back a word that is not a value at all. Both runs are separate
# processes because a stuck run cannot be caught in-process (ADR-0074).
carrier_diag=$("$VM" --self-test negative-proj-carrier --ffi "$WORK/provider.so" 2>&1 >/dev/null)
local_diag=$("$VM" --self-test negative-proj-local --ffi "$WORK/provider.so" 2>&1 >/dev/null)
if [ "$carrier_diag" = "$local_diag" ] && printf '%s' "$carrier_diag" | grep -qF "out of range"; then
  printf '  %-24s -> refused identically for carrier and local OK\n' negative-proj
else
  printf '  %-24s -> the two representations disagree FAIL\n' negative-proj; rc=1
  printf '      carrier: %s\n      local:   %s\n' "$carrier_diag" "$local_diag" >&2
fi

echo "== the build-emitted manifest: eager where the build knows, lazy everywhere else (§4) =="
# Slice 6. The manifest names the keys the BUILD resolved as workspace-provided, and those are checked
# before the program runs. The scope is the whole design: a referenced key can sit in a branch that
# never executes, and the VM has no dead-strip to tell the difference, so checking everything eagerly
# would reject working programs (ADR-0091 §1's false positive). Both halves are asserted here.
printf 'purvasm-foreign-manifest:v1\nTest.Loader.describeBoolImpl\n' >"$WORK/manifest-ok"
printf 'purvasm-foreign-manifest:v1\nTest.Loader.neverBuiltImpl\n' >"$WORK/manifest-missing"
printf 'not-a-manifest\n' >"$WORK/manifest-bogus"

if "$VM" --self-test loaded-provider --ffi "$WORK/provider.so" --manifest "$WORK/manifest-ok" \
     >"$WORK/manifest.out" 2>"$WORK/manifest.err"; then
  printf '  %-24s -> a declared key with a provider runs OK\n' manifest
else
  printf '  %-24s -> a satisfiable manifest was refused FAIL\n' manifest; rc=1
  sed 's/^/      /' "$WORK/manifest.err" >&2
fi

# A declared key with NO provider fails before the program runs — which is the point of declaring it.
if "$VM" --self-test loaded-provider --ffi "$WORK/provider.so" --manifest "$WORK/manifest-missing" \
     >"$WORK/missing.out" 2>"$WORK/missing.err"; then
  printf '  %-24s -> a declared key with no provider was accepted FAIL\n' manifest; rc=1
elif grep -qF "no native provider for Test.Loader.neverBuiltImpl" "$WORK/missing.err"; then
  # And it must fail EARLY: the program's own output must never appear.
  if grep -q "provider read Boolean" "$WORK/missing.out"; then
    printf '  %-24s -> refused, but only after the program ran FAIL\n' manifest; rc=1
  else
    printf '  %-24s -> a declared key with no provider fails before the run OK\n' manifest
  fi
else
  printf '  %-24s -> refused for the wrong reason FAIL\n' manifest; rc=1
  sed 's/^/      /' "$WORK/missing.err" >&2
fi

# An UNDECLARED key on a branch that never executes must NOT fail. `carrier-control` references
# several keys and takes only some branches; with no manifest naming them, none is checked eagerly.
if "$VM" --self-test carrier-control --ffi "$WORK/provider.so" >/dev/null 2>"$WORK/lazy.err"; then
  printf '  %-24s -> undeclared keys stay lazy OK\n' manifest
else
  printf '  %-24s -> a lazy key was checked eagerly FAIL\n' manifest; rc=1
  sed 's/^/      /' "$WORK/lazy.err" >&2
fi

# The writer and the reader hold the format independently (the build's `foreignManifest` and the VM's
# `manifestBanner`), so they can drift. This takes the banner the BUILD just wrote — the VM's own link
# emits one, since `Purvasm.VM.Loader`/`Foreign` are workspace modules with C siblings — and feeds it
# back as a manifest declaring nothing. It must be accepted, which is only true if the two agree.
#
# The build's manifest is not fed in whole on purpose: it names the VM's OWN trusted leaves, which are
# deliberately not exported (§6), so a host checking it against itself would fail by design. A
# manifest belongs to the image a VM runs, not to the VM.
head -1 "$VMDIR/foreign-manifest" >"$WORK/manifest-from-build"
if "$VM" --self-test loaded-provider --ffi "$WORK/provider.so" --manifest "$WORK/manifest-from-build" \
     >/dev/null 2>"$WORK/banner.err"; then
  printf '  %-24s -> the VM accepts the format the build writes OK\n' manifest
else
  printf '  %-24s -> writer and reader disagree about the format FAIL\n' manifest; rc=1
  printf '      build wrote: %s\n' "$(head -1 "$VMDIR/foreign-manifest")" >&2
  sed 's/^/      /' "$WORK/banner.err" >&2
fi

# Shapes the writer never produces must be refused, not repaired. A parse that skipped blank lines
# would accept a leading one (finding the banner anyway) and would drop an empty key silently — and a
# key silently dropped is a key not checked, which is the failure mode this whole gate exists for.
printf '\npurvasm-foreign-manifest:v1\nTest.Loader.describeBoolImpl\n' >"$WORK/manifest-lead-blank"
printf 'purvasm-foreign-manifest:v1\n\nTest.Loader.describeBoolImpl\n' >"$WORK/manifest-blank-key"
malformed_ok=yes
for bad in manifest-lead-blank manifest-blank-key; do
  if "$VM" --self-test loaded-provider --ffi "$WORK/provider.so" --manifest "$WORK/$bad" \
       >/dev/null 2>"$WORK/$bad.err"; then
    printf '  %-24s -> a malformed manifest was accepted (%s) FAIL\n' manifest "$bad"; rc=1; malformed_ok=no
  fi
done
[ "$malformed_ok" = yes ] && printf '  %-24s -> a blank line is refused, not repaired OK\n' manifest

# A manifest the VM does not understand is refused, not silently treated as empty: a gate that
# quietly becomes a no-op is worse than no gate, because the build still reports emitting one.
if "$VM" --self-test loaded-provider --ffi "$WORK/provider.so" --manifest "$WORK/manifest-bogus" \
     >/dev/null 2>"$WORK/bogus.err"; then
  printf '  %-24s -> an unrecognised manifest was ignored FAIL\n' manifest; rc=1
elif grep -qF "unrecognised foreign manifest" "$WORK/bogus.err"; then
  printf '  %-24s -> an unrecognised manifest is refused OK\n' manifest
else
  printf '  %-24s -> refused for the wrong reason FAIL\n' manifest; rc=1
  sed 's/^/      /' "$WORK/bogus.err" >&2
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

# The positive control, and the stale leg below is only interpretable while it holds: if NOTHING can
# load — a host that exported nothing, a broken fixture — the stale module is refused too, and the
# negative leg reports OK for a reason that has nothing to do with the version. (Measured: an ELF
# host whose allowlist never reached `.dynsym` produced exactly that false pass.) So the control's
# verdict is carried, and the stale leg refuses to conclude without it.
positive_control=no
if build_provider "$WORK/marker.so" "$FIX/Marker.c" Test_2eLoader; then
  if "$VM" --ffi "$WORK/marker.so" >"$WORK/marker.out" 2>"$WORK/marker.err"; then
    if grep -qF "MARKER: provider initialiser ran" "$WORK/marker.err"; then
      printf '  %-24s -> loaded, initialiser ran OK\n' current-version
      positive_control=yes
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
  if [ "$positive_control" != yes ]; then
    # Not "FAIL" for a defect of its own, and emphatically not OK: with nothing loadable, a refusal
    # here carries no information about the version at all.
    printf '  %-24s -> INCONCLUSIVE: the positive control above did not load FAIL\n' stale-version; rc=1
  elif "$VM" --ffi "$WORK/stale.so" >"$WORK/stale.out" 2>"$WORK/stale.err"; then
    printf '  %-24s -> loaded (expected a refusal) FAIL\n' stale-version; rc=1
  elif ! grep -qF "built against foreign ABI v99" "$WORK/stale.err"; then
    # The version in the message, not just the words: the loader distinguishes "built against
    # another ABI" from "this host exports no version symbol", and only the former is this gate.
    printf '  %-24s -> refused, but not by the version it was built against FAIL\n' stale-version; rc=1
    sed 's/^/      /' "$WORK/stale.err" >&2
  elif grep -qF "MARKER: provider initialiser ran" "$WORK/stale.err"; then
    printf '  %-24s -> refused AFTER its initialiser ran FAIL\n' stale-version; rc=1
  else
    printf '  %-24s -> refused before any module code ran OK\n' stale-version
  fi
else
  rc=1
fi

# A refusal must name the RIGHT cause. `dlerror()` reports the provider path beside the unresolved
# symbol, so a loader that searched the whole message for the version stamp could be steered by a
# filename: this provider is missing an ordinary host symbol, and is deliberately built as
# `pv_foreign_abi_v99-bad.so`. Both version verdicts are therefore wrong answers here, and the plain
# one — naming the symbol that is actually missing — is the only right one.
if build_provider "$WORK/pv_foreign_abi_v99-bad.so" "$FIX/Unresolved.c" Test_2eLoader; then
  if "$VM" --ffi "$WORK/pv_foreign_abi_v99-bad.so" >"$WORK/unresolved.out" 2>"$WORK/unresolved.err"; then
    printf '  %-24s -> loaded (expected a refusal) FAIL\n' spoofed-path; rc=1
  elif grep -qE "built against foreign ABI|does not export pv_foreign_abi_v" "$WORK/unresolved.err"; then
    printf '  %-24s -> the path forged an ABI verdict FAIL\n' spoofed-path; rc=1
    sed 's/^/      /' "$WORK/unresolved.err" >&2
  elif ! grep -qF "pvm_no_such_host_symbol" "$WORK/unresolved.err"; then
    printf '  %-24s -> refused without naming the missing symbol FAIL\n' spoofed-path; rc=1
    sed 's/^/      /' "$WORK/unresolved.err" >&2
  else
    printf '  %-24s -> refused, naming the real missing symbol OK\n' spoofed-path
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
