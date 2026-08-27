#!/usr/bin/env bash
# The ADR-0110 §6 step-E **packaging gate**: what `purvasm run` emits *beside* the image so a hosted
# guest can reach a workspace-provided leaf.
#
# Its own gate, not a leg of the other two, because it guards a third contract with a third lifetime.
# `vm-image-e2e.sh` is about the image format and its reader (ADR-0110 §4); `vm-loader-e2e.sh` is
# about the foreign frontier the VM enforces at run (ADR-0111 §1.1–§6). This one is about the
# **build's provider map**: which keys the workspace provides, what gets compiled for them, and what
# is refused. It changes when packaging changes — when the launcher lands, when dist layout is
# settled — and neither of the others should move then.
#
# The build is shared rather than duplicated: `tools/vm-e2e.sh` builds the VM once and passes
# `$PURVASM_VM_DIR` to every gate. Run alone, this script builds its own.
#
# Usage (from the repo root, inside `nix develop`):
#   tools/vm-packaging-e2e.sh
#
# Prerequisites (located, not built): the runtime staticlib (`$PURVASM_RT_A`), a purvasm library
# (`$PURVASM_LIB`), the workspace CoreFn (`spago build`), `clang`, `node`.
set -u

cd "$(dirname "${BASH_SOURCE[0]}")/.."
ROOT="$(pwd)"

unset PURVASM_BYNEED_OFF PURVASM_EMIT_DEBUG_ABI PURVASM_PROFILE_APPLY

: "${PURVASM_RT_A:=$ROOT/runtime/target/release/libpurvasm_rt.a}"
: "${PURVASM_LIB:=$ROOT/purvasm_lib}"
: "${PURVASM_INCLUDE:=$ROOT/runtime/include}"
: "${COREFN_DIR:=$ROOT/output}"
# boot's frozen VM, for the agreement leg. Located, never built: it is the reference, and a gate that
# rebuilt it would be comparing against whatever it just produced.
: "${BOOT_VM:=$ROOT/boot/_build/default/bin/main.exe}"
export PURVASM_RT_A PURVASM_LIB PURVASM_INCLUDE

for f in "$PURVASM_RT_A" "$PURVASM_LIB" "$COREFN_DIR/Purvasm.VM.Main/corefn.json"; do
  [ -e "$f" ] || { echo "missing prerequisite: $f" >&2; exit 2; }
done

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

if [ -n "${PURVASM_VM_DIR:-}" ]; then
  VMDIR="$PURVASM_VM_DIR"
  echo "== using the VM build at $VMDIR =="
  [ -x "$VMDIR/app" ] || { echo "  no executable at $VMDIR/app" >&2; exit 2; }
else
  VMDIR="$WORK/vm"
  echo "== building the VM =="
  if ! node "$ROOT/cli/index.node.js" build --entry Purvasm.VM.Main --corefn-dir "$COREFN_DIR" \
         --outdir "$VMDIR" --host-foreign-api >"$WORK/build.log" 2>&1; then
    echo "  the VM build failed — see $WORK/build.log" >&2
    tail -20 "$WORK/build.log" >&2
    exit 2
  fi
fi
VM="$VMDIR/app"

rc=0
ok ()   { printf '  %-24s -> %s OK\n' "$1" "$2"; }
fail () { printf '  %-24s -> %s FAIL\n' "$1" "$2"; rc=1; }

# Compile <entry> into <outdir>, with the library at $2 (so a leg can hand it a broken one).
emit () { # <outdir> <lib> <entry>
  PURVASM_LIB="$2" node "$ROOT/cli/index.node.js" run --corefn-dir "$COREFN_DIR" \
    --outdir "$1" --build-only -m "$3" >"$1.log" 2>&1
}

# The manifest's keys (its first line is the format banner).
keys_of () { tail -n +2 "$1" | tr '\n' ' ' | sed 's/ *$//'; }

echo "== a program that needs a ulib leaf (ADR-0073 §2: shipped as .c, built for the target) =="
# `bench-json-parse` reaches `Data.Number.isFinite`, which ulib ships as `.c` and a natively compiled
# program LINKS. A hosted guest has no such link, so the build must package it as something loadable.
if emit "$WORK/needs" "$PURVASM_LIB" Bench.JsonParse.Main; then
  if [ -f "$WORK/needs/app.provider.so" ]; then ok provider-emitted "a loadable provider was built"
  else fail provider-emitted "no provider beside the image"; fi
  if [ "$(keys_of "$WORK/needs/app.manifest")" = "Data.Number.isFinite" ]; then
    ok manifest-keys "declares exactly the workspace-provided key"
  else
    fail manifest-keys "declares '$(keys_of "$WORK/needs/app.manifest")'"
  fi
  # One `.c` provides seven `Data.Number` keys; compiling it once per key would define every symbol
  # in it repeatedly and the link would fail. That it links at all is the dedup check, and the symbol
  # count is what makes the check legible rather than incidental.
  defined="$({ llvm-nm --defined-only "$WORK/needs/app.provider.so" 2>/dev/null \
    || nm -g "$WORK/needs/app.provider.so" 2>/dev/null; } | grep -c 'pvf_Data_2eNumber' || true)"
  if [ "$defined" -ge 1 ]; then ok source-dedup "one source compiled once ($defined Data.Number symbols)"
  else fail source-dedup "the provider defines no Data.Number symbol"; fi

  if out=$("$VM" --ffi "$WORK/needs/app.provider.so" --manifest "$WORK/needs/app.manifest" \
             --image "$WORK/needs/app.pvm" -- 500 2>"$WORK/needs.err"); then
    if [ -x "$BOOT_VM" ]; then
      boot_out=$("$BOOT_VM" run "$WORK/needs/app.boot.pvm" 500 2>/dev/null)
      if [ "$out" = "$boot_out" ]; then ok hosted-run "the guest ran, and agrees with boot"
      else fail hosted-run "owned '$out' vs boot '$boot_out'"; fi
    else
      # Never silently skipped: a gate that drops a leg reads as coverage it does not have.
      printf '  %-24s -> SKIPPED (no boot VM at %s)\n' hosted-run "$BOOT_VM"
    fi
  else
    fail hosted-run "the guest could not run"; sed 's/^/      /' "$WORK/needs.err" >&2
  fi

  # The manifest is a CONTRACT, not a hint: a declared key with no module loaded must stop the run
  # before the program's own output appears (ADR-0111 §4).
  if "$VM" --manifest "$WORK/needs/app.manifest" --image "$WORK/needs/app.pvm" -- 500 \
       >"$WORK/nodep.out" 2>"$WORK/nodep.err"; then
    fail declared-unprovided "ran without the provider it declared"
  elif grep -q "no native provider for Data.Number.isFinite" "$WORK/nodep.err"; then
    ok declared-unprovided "refused before the program ran"
  else
    fail declared-unprovided "refused for the wrong reason"; sed 's/^/      /' "$WORK/nodep.err" >&2
  fi
else
  fail provider-emitted "the build failed"; tail -5 "$WORK/needs.log" >&2
fi

echo "== programs that need no packaging =="
# Runtime leaves resolve through `host-runtime` and are deliberately NOT in the manifest — the VM
# leaves them lazy. An empty-but-well-formed manifest is the positive claim "nothing is provided",
# which is not the same as no manifest at all.
for probe in "runtime-leaves Gate.GcChurn" "foreign-free VMGate.Quiet"; do
  set -- $probe
  if emit "$WORK/$1" "$PURVASM_LIB" "$2"; then
    if [ -f "$WORK/$1/app.provider.so" ]; then
      fail "$1" "a provider was built for a program that needs none"
    elif [ -f "$WORK/$1/app.manifest" ] && [ -z "$(keys_of "$WORK/$1/app.manifest")" ]; then
      ok "$1" "no provider, and an empty well-formed manifest"
    else
      fail "$1" "the manifest is missing or not empty"
    fi
  else
    fail "$1" "the build failed"; tail -5 "$WORK/$1.log" >&2
  fi
done

echo "== a reused output directory =="
# The failure this catches is silent: a build that provides nothing, run into an outdir that once did,
# leaves the old module for a runner to find and hand the VM.
if emit "$WORK/reused" "$PURVASM_LIB" Bench.JsonParse.Main \
   && [ -f "$WORK/reused/app.provider.so" ] \
   && emit "$WORK/reused" "$PURVASM_LIB" Gate.GcChurn; then
  if [ -f "$WORK/reused/app.provider.so" ]; then
    fail stale-provider "the previous build's provider is still there"
  else
    ok stale-provider "the previous build's provider was removed"
  fi
else
  fail stale-provider "the two builds did not run"; tail -5 "$WORK/reused.log" >&2
fi

echo "== a library that cannot account for a referenced leaf =="
# The diagnostic hole this closes: a library whose `foreign` map cannot be read must NOT be taken for
# "the workspace provides nothing". A leaf that is neither runtime-defined nor library-mapped is
# refused by name at build time, rather than becoming a run that dies partway through.
cp -r "$PURVASM_LIB" "$WORK/lib-no-map"
node -e 'const fs=require("fs");const p=process.argv[1];const j=JSON.parse(fs.readFileSync(p,"utf8"));delete j.foreign;fs.writeFileSync(p,JSON.stringify(j))' \
  "$WORK/lib-no-map/ulib.json"
if emit "$WORK/unaccounted" "$WORK/lib-no-map" Bench.JsonParse.Main; then
  fail fail-closed "the build accepted a leaf nothing can provide"
elif grep -q "Data.Number.isFinite" "$WORK/unaccounted.log" \
     && grep -q "nothing can provide" "$WORK/unaccounted.log"; then
  ok fail-closed "refused at build time, naming the key"
else
  fail fail-closed "refused for the wrong reason"; tail -5 "$WORK/unaccounted.log" >&2
fi

echo "== the launcher: purvasm run, end to end =="
# Everything above drives the VM directly, which tests the artifacts but not the command a person
# actually types. These drive `purvasm run` itself, because what it does with those artifacts — pass
# the provider it just built, hand the program its own argv, and refuse before execution something
# that cannot run — is a contract of the launcher, not of the packaging.
launch () { # <outdir> <module> [guest args...]
  local out="$1" mod="$2"
  shift 2
  if [ "$#" -eq 0 ]; then
    PURVASM_VM="$VM" node "$ROOT/cli/index.node.js" run \
      --corefn-dir "$COREFN_DIR" --outdir "$out" -m "$mod" 2>"$out.err"
  else
    PURVASM_VM="$VM" node "$ROOT/cli/index.node.js" run \
      --corefn-dir "$COREFN_DIR" --outdir "$out" -m "$mod" -- "$@" 2>"$out.err"
  fi
}

# A program that needs a provider: the launcher built one a moment ago and must hand it over. Its
# argv comes from after `--`, and 500 is echoed back by the program, so a guest that never received
# it would print its default instead.
if out=$(launch "$WORK/launch-needs" Bench.JsonParse.Main 500); then
  if [ "$(printf '%s' "$out" | tail -1)" = "500" ]; then
    ok launch-provider "ran with the provider it built, and the guest read its argv"
  else
    fail launch-provider "the guest printed '$(printf '%s' "$out" | tail -1)'"
  fi
else
  fail launch-provider "the launcher could not run it"; sed 's/^/      /' "$WORK/launch-needs.err" >&2
fi

# A program that needs none must run just the same: the launcher passes no `--ffi`, and the VM's
# exactly-one rule never comes into it.
if out=$(launch "$WORK/launch-plain" Gate.GcChurn); then
  if printf '%s' "$out" | grep -q "sum=50005000"; then
    ok launch-no-provider "ran with no provider to pass"
  else
    fail launch-no-provider "unexpected output"
  fi
else
  fail launch-no-provider "the launcher could not run it"; sed 's/^/      /' "$WORK/launch-plain.err" >&2
fi

# `-m` names the module whose `main` runs, so a module without one is the caller's mistake and must
# be named as such before execution, not as a stuck run with a mangled key in it.
if launch "$WORK/launch-nomain" Purvasm.VM.Image >/dev/null 2>&1; then
  fail launch-no-main "ran a module with no main"
elif grep -q "defines no" "$WORK/launch-nomain.err"; then
  ok launch-no-main "refused, naming the module"
else
  fail launch-no-main "refused for the wrong reason"; sed 's/^/      /' "$WORK/launch-nomain.err" >&2
fi

# With no VM configured there is nothing to run the program WITH, and $PURVASM_VM is the only rule for
# finding one while the install layout is unsettled. Saying so beats a missing-file error.
if env -u PURVASM_VM node "$ROOT/cli/index.node.js" run --corefn-dir "$COREFN_DIR" \
     --outdir "$WORK/launch-novm" -m VMGate.Quiet >/dev/null 2>"$WORK/launch-novm.err"; then
  fail launch-no-vm "ran without a VM configured"
elif grep -q "PURVASM_VM" "$WORK/launch-novm.err"; then
  ok launch-no-vm "refused, naming the variable that would fix it"
else
  fail launch-no-vm "refused for the wrong reason"; sed 's/^/      /' "$WORK/launch-novm.err" >&2
fi

# A program's own failure is the program's, and the launcher must not report success for it.
if launch "$WORK/launch-fail" VMGate.Fails >/dev/null 2>&1; then
  fail launch-guest-failure "reported success for a program that failed"
else
  ok launch-guest-failure "a failing program fails the command"
fi

if [ "$rc" -eq 0 ]; then
  echo "★ the build packages what a hosted guest needs, and refuses what it cannot"
else
  echo "✗ a packaging leg diverged — see the table above (logs under $WORK until exit)" >&2
fi
exit "$rc"
