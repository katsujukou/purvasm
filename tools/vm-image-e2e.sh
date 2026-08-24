#!/usr/bin/env bash
# The ADR-0110 slice-2 image gate: the owned VM reading a linked `.pvm` and running it.
#
# Separate from `vm-loader-e2e.sh` deliberately. That gate is about ADR-0111's foreign frontier — the
# link's retained set, `dlopen`, the ABI version — and this one is about ADR-0110's *format*: what the
# reader accepts, what it refuses, and whether a decoded program computes the same thing the bytecode
# says — plus the execution context the VM owes a guest, which is neither (the guest's argv). The two
# have different lifetimes as well as different failure modes: §4(b) will rewrite the `case` encoding
# and every assertion below with it, while the loader gate will not notice.
#
# The build is shared rather than duplicated: `tools/vm-e2e.sh` builds the VM once and passes
# `$PURVASM_VM_DIR` to both gates. Run alone, this script builds its own.
#
# Usage (from the repo root, inside `nix develop`):
#   tools/vm-image-e2e.sh
#
# Prerequisites (located, not built): the runtime staticlib (`$PURVASM_RT_A`), the staged ulib
# (`$PURVASM_LIB`), the workspace CoreFn (`spago build` — the VM's and the fixture package's),
# `clang`, `node`.
set -u

cd "$(dirname "${BASH_SOURCE[0]}")/.."
ROOT="$(pwd)"

unset PURVASM_BYNEED_OFF PURVASM_EMIT_DEBUG_ABI PURVASM_PROFILE_APPLY

FIX="$ROOT/vm/test/fixtures/image"
: "${PURVASM_RT_A:=$ROOT/runtime/target/release/libpurvasm_rt.a}"
: "${PURVASM_LIB:=$ROOT/purvasm_lib}"
: "${PURVASM_INCLUDE:=$ROOT/runtime/include}"
: "${COREFN_DIR:=$ROOT/output}"
# boot's frozen VM, for the cross-runner agreement leg. Located, never built: it is the reference, and
# a gate that rebuilt it would be comparing against whatever it just produced.
: "${BOOT_VM:=$ROOT/boot/_build/default/bin/main.exe}"
export PURVASM_RT_A PURVASM_LIB PURVASM_INCLUDE

for f in "$PURVASM_RT_A" "$PURVASM_LIB" "$COREFN_DIR/Main/corefn.json"; do
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
  if ! node "$ROOT/cli/index.node.js" build --entry Main --corefn-dir "$COREFN_DIR" \
         --outdir "$VMDIR" --host-foreign-api >"$WORK/build.log" 2>&1; then
    echo "  the VM build failed — see $WORK/build.log" >&2
    tail -20 "$WORK/build.log" >&2
    exit 2
  fi
fi
VM="$VMDIR/app"

rc=0

expect_value () { # <label> <image> <expected stdout>
  local label="$1" img="$2" want="$3" got
  if got="$("$VM" --image "$img" 2>"$WORK/$label.err")"; then
    if [ "$got" = "$want" ]; then
      printf '  %-24s -> %s OK\n' "$label" "$got"
    else
      printf '  %-24s -> %s FAIL (expected %s)\n' "$label" "$got" "$want"; rc=1
    fi
  else
    printf '  %-24s -> the VM refused the image FAIL\n' "$label"; rc=1
    sed 's/^/      /' "$WORK/$label.err" >&2
  fi
}

expect_refusal () { # <label> <image> <needle>
  local label="$1" img="$2" needle="$3"
  if "$VM" --image "$img" >"$WORK/$label.out" 2>"$WORK/$label.err"; then
    printf '  %-24s -> ran (expected a refusal) FAIL\n' "$label"; rc=1
  elif grep -qF "$needle" "$WORK/$label.err"; then
    printf '  %-24s -> refused: %s OK\n' "$label" "$needle"
  else
    printf '  %-24s -> refused for the wrong reason FAIL\n' "$label"; rc=1
    sed 's/^/      /' "$WORK/$label.err" >&2
  fi
}

echo "== decoding and running a linked image (§6 slice 2) =="
# The three global shapes are three evaluation strategies, and this image needs all of them to agree:
# `twice` is a closure, `base` a strict CAF built at load, `lazy` a by-need one forced on use.
expect_value globals "$FIX/globals.pvm" "42"

# The `case` encoding today's format uses: switches over relative offsets into a flat block. Both a
# matching arm and the default edge are driven, because an off-by-one in the offset base would land
# inside the wrong arm rather than failing — the first version of this fixture did exactly that.
expect_value switch "$FIX/switch.pvm" "50"
expect_refusal default-arm "$FIX/default-arm.pvm" "no arm matched"

# A stale image must fail loudly rather than being misparsed — the whole point of the version stamp
# (ADR-0110 §Consequences).
expect_refusal stale-version "$FIX/stale-version.pvm" "unsupported image format version 2"

echo "== a real image, produced by \`purvasm run\` (§6 slice 2, step A) =="
# Not a fixture hand-written by the person who wrote the reader: `VMGate.Quiet` is an ordinary
# workspace module, so the ordinary `spago build` produces its CoreFn and the Level-2 bytecode path
# compiles, links and runs it exactly as it would any program. Step A's scope is a program with no
# native leaf, and this is what that looks like end to end.
if node "$ROOT/cli/index.node.js" run --corefn-dir "$COREFN_DIR" --outdir "$WORK/img" \
     --entry VMGate.Quiet >"$WORK/run.log" 2>&1; then
  if "$VM" --image "$WORK/img/app.v4.pvm" >"$WORK/real.out" 2>"$WORK/real.err"; then
    printf '  %-24s -> the owned VM ran a linked image OK\n' real-image
  else
    printf '  %-24s -> the owned VM could not run it FAIL\n' real-image; rc=1
    sed 's/^/      /' "$WORK/real.err" >&2
  fi
  # The legacy form of the SAME program still decodes here: the reader accepts both versions while
  # the two runners coexist (§4(a)), and a change that quietly dropped version 3 would strand boot.
  if "$VM" --image "$WORK/img/app.pvm" >/dev/null 2>"$WORK/legacy.err"; then
    printf '  %-24s -> the legacy version-3 image still decodes OK\n' legacy-image
  else
    printf '  %-24s -> the reader stopped accepting version 3 FAIL\n' legacy-image; rc=1
    sed 's/^/      /' "$WORK/legacy.err" >&2
  fi
else
  printf '  %-24s -> \`purvasm run\` failed FAIL\n' real-image; rc=1
  tail -5 "$WORK/run.log" >&2
fi

echo "== the guest's own argv (ADR-0075 §4, step C) =="
# A hosted program's command line is the HOST's to define. Without this the guest reads the VM's own
# argv and sees `--image` where its first argument belongs — not a crash but a different input, so
# nothing else here would notice: the corpus would simply run at its default size.
if node "$ROOT/cli/index.node.js" run --corefn-dir "$COREFN_DIR" --outdir "$WORK/img2" \
     --entry VMGate.Argv >"$WORK/run2.log" 2>&1; then
  got="$("$VM" --image "$WORK/img2/app.v4.pvm" -- alpha beta 2>"$WORK/argv.err")"
  want="$WORK/img2/app.v4.pvm|alpha|beta"
  if [ "$got" = "$want" ]; then
    printf '  %-24s -> the guest sees [image] ++ its own arguments OK\n' guest-argv
  else
    printf '  %-24s -> saw %s FAIL (expected %s)\n' guest-argv "$got" "$want"; rc=1
    sed 's/^/      /' "$WORK/argv.err" >&2
  fi
  # The VM's own flags are the VM's. A guest that could see them would change behaviour when the VM
  # grew a flag, and the `--` separator is what makes that impossible rather than unlikely.
  got_bare="$("$VM" --image "$WORK/img2/app.v4.pvm" --count 2>/dev/null)"
  if [ "$got_bare" = "$WORK/img2/app.v4.pvm" ]; then
    printf '  %-24s -> the VM'"'"'s own flags never reach the guest OK\n' guest-argv-flags
  else
    printf '  %-24s -> saw %s FAIL\n' guest-argv-flags "$got_bare"; rc=1
  fi
else
  printf '  %-24s -> could not build the argv probe FAIL\n' guest-argv; rc=1
  tail -5 "$WORK/run2.log" >&2
fi

# The other half of the argv decision, asserted behaviourally rather than by inspection: a guest that
# NAMES the trusted setter must not reach it. The loader gate checks it is absent from the dynamic
# exports; this checks what that absence is for — a hand-written image whose `ForeignRef` is exactly
# the key `Host.c` defines, refused as unbound. If the setter were ever exported, a guest could
# rewrite the argv of the runner hosting it, and no positive leg anywhere would notice.
expect_refusal host-control-reach "$FIX/host-control-reach.pvm" \
  "unbound native foreign: Purvasm.VM.Host.setGuestArgvImpl"

echo "== a program with native leaves (§4(a), step B) =="
# The step-B claim, on a real Effect program rather than a fixture: `Gate.GcChurn` allocates, mutates
# and prints, so its reachable set includes runtime leaves (`Purvasm.String.*`, `Purvasm.Stdio.*`).
#
# Three separate things are checked, because passing one while failing another is exactly how this
# would go wrong quietly. (1) The leaf is EMITTED as `ForeignRef key arity` — before step B the
# bytecode backend emitted no `ForeignRef` at all, and a native key stayed an unresolved `Load` that
# boot's VM rescued from its compiled-in registry; a resolver change could make the program run
# without the emission ever being right, so the image is inspected directly. (2) The owned VM runs it.
# (3) Its output equals boot's on the legacy image built by the SAME compilation — the two forms carry
# the same instruction sequence, so a disagreement is a leaf being called wrongly, not a format bug.
if node "$ROOT/cli/index.node.js" run --corefn-dir "$COREFN_DIR" --outdir "$WORK/leafimg" \
     --entry Gate.GcChurn >"$WORK/leafrun.log" 2>&1; then
  if node -e '
      const fs = require("fs");
      const img = JSON.parse(fs.readFileSync(process.argv[1], "utf8"));
      if (img.version !== 4) { console.error("version " + img.version + ", expected 4"); process.exit(1); }
      const keys = new Set();
      const walk = (x) => {
        if (!Array.isArray(x)) return;
        if (x[0] === "fr") {
          if (x.length !== 3 || typeof x[2] !== "number") { console.error("fr without an arity: " + JSON.stringify(x)); process.exit(1); }
          keys.add(x[1] + "/" + x[2]);
          return;
        }
        for (const y of x) walk(y);
      };
      walk(img.gdefs); walk(img.main);
      const want = ["Purvasm.Stdio.writeLineImpl/1", "Purvasm.String.unsafeSetByte/3"];
      const missing = want.filter((k) => !keys.has(k));
      if (missing.length) { console.error("expected leaves absent: " + missing.join(", ") + "; found " + [...keys].join(", ")); process.exit(1); }
      if (/"ld","Purvasm\./.test(fs.readFileSync(process.argv[1], "utf8"))) { console.error("a native key is still loaded as a global"); process.exit(1); }
    ' "$WORK/leafimg/app.v4.pvm" 2>"$WORK/emit.err"; then
    printf '  %-24s -> leaves emitted as ForeignRef key arity OK\n' leaf-emission
  else
    printf '  %-24s -> FAIL\n' leaf-emission; rc=1
    sed 's/^/      /' "$WORK/emit.err" >&2
  fi

  if "$VM" --image "$WORK/leafimg/app.v4.pvm" >"$WORK/leaf.out" 2>"$WORK/leaf.err"; then
    printf '  %-24s -> the owned VM ran a program with native leaves OK\n' leaf-run
    if [ -x "$BOOT_VM" ]; then
      if "$BOOT_VM" run "$WORK/leafimg/app.pvm" >"$WORK/leaf.boot" 2>/dev/null \
         && diff -q "$WORK/leaf.out" "$WORK/leaf.boot" >/dev/null; then
        printf '  %-24s -> same output as boot on the legacy image OK\n' leaf-agreement
      else
        printf '  %-24s -> the two runners disagree FAIL\n' leaf-agreement; rc=1
        diff "$WORK/leaf.boot" "$WORK/leaf.out" | sed 's/^/      /' >&2
      fi
    else
      # Never silently skipped: a gate that quietly drops a leg reads as coverage it does not have.
      printf '  %-24s -> SKIPPED (no boot VM at %s)\n' leaf-agreement "$BOOT_VM"
    fi
  else
    printf '  %-24s -> the owned VM could not run it FAIL\n' leaf-run; rc=1
    sed 's/^/      /' "$WORK/leaf.err" >&2
  fi

  # The legacy form is still produced, and still boot's: step C compares the two runners on ONE
  # compilation, so losing either form silently would take the calibration with it.
  if [ -s "$WORK/leafimg/app.pvm" ] && ! grep -q '"version":3' "$WORK/leafimg/app.pvm"; then
    printf '  %-24s -> the legacy image is no longer version 3 FAIL\n' dual-emission; rc=1
  elif grep -q '\["fr","Purvasm.Stdio.writeLineImpl"\]' "$WORK/leafimg/app.pvm"; then
    printf '  %-24s -> both forms written from one compilation OK\n' dual-emission
  else
    printf '  %-24s -> the legacy image lost its arity-free ForeignRef FAIL\n' dual-emission; rc=1
  fi
else
  printf '  %-24s -> could not build the probe image FAIL\n' leaf-emission; rc=1
  tail -5 "$WORK/leafrun.log" >&2
fi

if [ "$rc" -eq 0 ]; then
  echo "★ the owned VM reads and runs linked images"
else
  echo "✗ an image leg diverged — see the table above (logs under $WORK until exit)" >&2
fi
exit "$rc"
