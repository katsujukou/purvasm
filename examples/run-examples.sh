#!/usr/bin/env bash
# Run every example on the bytecode VM, the OCaml native backend, and the LLVM native backend,
# and diff the captured stdout across the three (optionally four, with --with-js) columns.
#
# Usage (from the repo root, inside `nix develop`):
#
#   ./examples/run-examples.sh [--with-js] [--keep DIR]
#
# Prerequisites (the script only *locates* these):
#   - example CoreFn in `output/` (the workspace `spago build`),
#   - the staged ulib in `dist/ulib` (`node ulib-tools/index.js build --prepare-release`),
#   - boot built (`dune build` in `boot/`),
#   - the runtime staticlib (`cargo build` in `runtime/`) and `clang` for the LLVM column,
#   - `spago`/`node` for the optional JS column.
#
# Exit status: non-zero iff any example not on the XFAIL list fails to build/run or diverges
# across columns. An XFAIL entry that *passes* is reported as XPASS so the entry gets removed.
set -u

cd "$(dirname "$0")/.."

PURVM=./boot/_build/default/bin/main.exe
WITH_JS=0
KEEP_DIR=""
while [ $# -gt 0 ]; do
  case "$1" in
    --with-js) WITH_JS=1 ;;
    --keep)
      shift
      KEEP_DIR="${1:?--keep needs a directory}"
      ;;
    *)
      echo "unknown argument: $1" >&2
      exit 2
      ;;
  esac
  shift
done

# One line per example: <directory> <entry module> [XFAIL:<reason>]. The completeness check
# below errors on an `examples/*/spago.yaml` directory missing from this table, so an example
# cannot be silently left out of the sweep.
EXAMPLES="
fib              Example.Fib.Main
helloworld       Example.HelloWorld.Main
effect-ref       Example.EffectRef.Main
record-meta      Example.RecordMeta.Main
recursion-scheme Example.RecursionScheme.Main
recursive-value  Example.RecursiveValue.Main
transformer      Example.Transformer.Main
"

if [ ! -x "$PURVM" ]; then
  echo "error: $PURVM not found — run \`dune build\` in boot/ first" >&2
  exit 2
fi

# Completeness: every example directory must appear in the table.
missing=0
for d in examples/*/; do
  name=$(basename "$d")
  [ -f "$d/spago.yaml" ] || continue
  if ! echo "$EXAMPLES" | awk '{print $1}' | grep -qx "$name"; then
    echo "error: examples/$name has no entry in run-examples.sh — add it (or an XFAIL)" >&2
    missing=1
  fi
done
[ $missing -eq 0 ] || exit 2

if [ -n "$KEEP_DIR" ]; then
  WORK=$KEEP_DIR
  mkdir -p "$WORK"
else
  WORK=$(mktemp -d)
  trap 'rm -rf "$WORK"' EXIT
fi
rm -f "$WORK/.failed"
echo "$EXAMPLES" | while read -r name module xfail; do
  [ -n "$name" ] || continue
  base="$WORK/$name"
  mkdir -p "$base"

  corefn="output/$module"
  if [ ! -f "$corefn/corefn.json" ]; then
    echo "error: $corefn/corefn.json missing — run \`spago build\` first" >&2
    printf '%-18s %s\n' "$name" "FAIL(no corefn)"
    echo 1 >"$WORK/.failed"
    continue
  fi

  "$PURVM" build --corefn-dir output --ulib dist/ulib -m "$module" -o "$base/vm" \
    >"$base/vm.build.log" 2>&1 \
    && "$PURVM" run "$base/vm/app.pvm" >"$base/vm.out" 2>"$base/vm.err"
  vm_rc=$?
  "$PURVM" native --backend ocaml --corefn-dir output --ulib dist/ulib -m "$module" -o "$base/ml" \
    >"$base/ml.build.log" 2>&1 \
    && "$base/ml/app" >"$base/ml.out" 2>"$base/ml.err"
  ml_rc=$?
  "$PURVM" native --backend llvm --corefn-dir output --ulib dist/ulib -m "$module" -o "$base/llvm" \
    >"$base/llvm.build.log" 2>&1 \
    && "$base/llvm/app" >"$base/llvm.out" 2>"$base/llvm.err"
  llvm_rc=$?

  status="OK"
  if [ $vm_rc -ne 0 ] || [ $ml_rc -ne 0 ] || [ $llvm_rc -ne 0 ]; then
    status="FAIL(rc vm=$vm_rc ml=$ml_rc llvm=$llvm_rc)"
  elif ! diff -q "$base/vm.out" "$base/ml.out" >/dev/null \
    || ! diff -q "$base/vm.out" "$base/llvm.out" >/dev/null; then
    status="DIVERGE(vm/ml/llvm)"
  elif [ $WITH_JS -eq 1 ]; then
    pkg=$(awk '/^  name:/ {print $2; exit}' "examples/$name/spago.yaml")
    if spago run -q -p "$pkg" -m "$module" >"$base/js.out" 2>"$base/js.err" \
      && diff -q "$base/vm.out" "$base/js.out" >/dev/null; then
      status="OK(+js)"
    else
      status="DIVERGE(js)"
    fi
  fi

  case "$status" in
    OK*)
      if [ -n "$xfail" ]; then
        printf '%-18s XPASS (%s — remove the XFAIL entry)\n' "$name" "${xfail#XFAIL:}"
      else
        printf '%-18s %s\n' "$name" "$status"
      fi
      ;;
    *)
      if [ -n "$xfail" ]; then
        printf '%-18s XFAIL (%s)\n' "$name" "${xfail#XFAIL:}"
      else
        printf '%-18s %s   (logs: %s)\n' "$name" "$status" "$base"
        # The work dir is a temp dir (gone after the run, e.g. on CI), so a failure dumps its
        # evidence inline: the tail of every non-empty build log / runtime stderr / column output.
        for f in "$base"/*.build.log "$base"/*.err "$base"/*.out; do
          [ -s "$f" ] || continue
          echo "----- $(basename "$f") (last 15 lines) -----" >&2
          tail -15 "$f" >&2
        done
        echo 1 >"$WORK/.failed"
      fi
      ;;
  esac
done

[ ! -f "$WORK/.failed" ]
exit $?
