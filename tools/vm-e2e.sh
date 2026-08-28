#!/usr/bin/env bash
# Build the owned VM once, then run all of its gates against that one build.
#
# The gates stay separate because they are about different things, and they change for different
# reasons: `vm-image-e2e.sh` guards ADR-0110's image format and reader, `vm-packaging-e2e.sh` what the
# build emits beside an image for a hosted guest (§6 step E), and `vm-loader-e2e.sh` ADR-0111's foreign
# frontier as the VM enforces it at run. A format change rewrites the first and leaves the others
# alone; a packaging change moves only the second. What all three share is a `purvasm build`, which
# takes minutes, so this script exists to pay for it once.
#
# It shares a build DIRECTORY rather than an executable: the loader gate inspects the link's own
# artifacts (the export allowlist, the emitted manifest), not just the binary.
#
# Usage (from the repo root, inside `nix develop`):
#   tools/vm-e2e.sh
#
# Each gate also runs alone, building its own VM — that is what `$PURVASM_VM_DIR` being optional is
# for, and it is why a failing gate can be re-run in isolation without this wrapper.
set -u

cd "$(dirname "${BASH_SOURCE[0]}")/.."
ROOT="$(pwd)"

: "${PURVASM_RT_A:=$ROOT/runtime/target/release/libpurvasm_rt.a}"
: "${PURVASM_LIB:=$ROOT/purvasm_lib}"
: "${PURVASM_INCLUDE:=$ROOT/runtime/include}"
: "${COREFN_DIR:=$ROOT/output}"
export PURVASM_RT_A PURVASM_LIB PURVASM_INCLUDE COREFN_DIR

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

echo "== building the owned VM once, for all three gates =="
if ! node "$ROOT/cli/index.node.js" build --entry Purvasm.VM.Main --corefn-dir "$COREFN_DIR" \
       --outdir "$WORK/vm" --host-foreign-api >"$WORK/build.log" 2>&1; then
  echo "  the VM build failed — see $WORK/build.log" >&2
  tail -20 "$WORK/build.log" >&2
  exit 2
fi
grep -F "host-foreign-api: retaining" "$WORK/build.log" | sed 's/^ */  /'

export PURVASM_VM_DIR="$WORK/vm"
rc=0
./tools/vm-image-e2e.sh || rc=1
./tools/vm-packaging-e2e.sh || rc=1
./tools/vm-loader-e2e.sh || rc=1

if [ "$rc" -eq 0 ]; then
  echo "★ every owned-VM gate is green"
else
  echo "✗ an owned-VM gate diverged — re-run the failing one alone for its full output" >&2
fi
exit "$rc"
