#!/usr/bin/env sh
# Compile the `ulib` registry-package patches (ADR-0038) to corefn, for boot's `--ulib`
# overlay. Overlays each `ulib/<package>/<Module>.purs` over the resolved registry sources
# (+ the `purvasm-base` primitives), compiles the lot to corefn with the pinned `purs`, and
# extracts only the patched modules' corefn into `<out>/<Module>/corefn.json`.
#
#   sh ulib-tools/install.sh <out-dir> [<ulib-src>] [<purvasm-base-src>] [<purs>] [<spago-pkgs>]
#
# Defaults assume invocation from the repo root.
set -eu

OUT="$1"
ULIB="${2:-ulib}"
BASE="${3:-packages/purvasm-base/src}"
PURS="${4:-purs}"
SPAGO="${5:-.spago/p}"

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
mkdir -p "$TMP/src"

# 1. all resolved package-set sources (registry deps `<pkg>/src`, git deps `<pkg>/<ref>/src`)
#    plus the `purvasm-base` primitives the patches are PureScript over.
for d in "$SPAGO"/*/src "$SPAGO"/*/*/src; do
  [ -d "$d" ] && cp -R "$d/." "$TMP/src/"
done
cp -R "$BASE/." "$TMP/src/"

# 2. overlay the patches over the registry modules (drop the registry `.js`: this is a
#    corefn-only build; a kept `foreign import` resolves to a boot intrinsic at link time).
patches="$(cd "$ULIB" && find . -mindepth 2 -name '*.purs' | sed 's#^\./##')"
for rel in $patches; do
  mod="$(basename "$rel" .purs)"                  # e.g. Data.Semigroup
  modrel="$(printf '%s' "$mod" | tr . /).purs"    # e.g. Data/Semigroup.purs
  mkdir -p "$(dirname "$TMP/src/$modrel")"
  cp "$ULIB/$rel" "$TMP/src/$modrel"
  rm -f "$TMP/src/${modrel%.purs}.js"
done

# 3. compile the whole overlaid set to corefn.
"$PURS" compile --codegen corefn --output "$TMP/output" "$TMP/src/**/*.purs" >/dev/null

# 4. extract the patched modules' corefn into the flat lib layout `<out>/<Module>/corefn.json`.
mkdir -p "$OUT"
for rel in $patches; do
  mod="$(basename "$rel" .purs)"
  mkdir -p "$OUT/$mod"
  cp "$TMP/output/$mod/corefn.json" "$OUT/$mod/corefn.json"
  echo "ulib: $mod"
done
echo "ulib: wrote patched corefn to $OUT"
