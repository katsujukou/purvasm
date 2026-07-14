#!/usr/bin/env bash
# E2E regression check for `Data.String.Internal.CaseMap` / `Data.String.Common`'s case mapping
# (toLower/toUpper/localeCompare): stages the real registry + ulib + in-repo-package source tree
# the way `ulib-tools build` does (Stage.stageRegistry / Stage.overlayPatches), compiles it with
# `purs`, and runs verify-case-mapping.mjs against the ACTUAL compiled output -- not just
# `ulib-tools unicode-gen`'s own self-check (Purvasm.UlibTools.UnicodeData.selfCheck, ADR-0101),
# which never touches the generated PureScript itself.
#
# Usage (from the repo root, inside `nix develop`): ./ulib/strings/test/E2E/verify-case-mapping.sh
set -euo pipefail
cd "$(dirname "$0")/../../../.."   # repo root

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
mkdir -p "$WORK/src" "$WORK/out"

# stageRegistry: every resolved package's src, plus purvasm-base.
for pkgdir in .spago/p/*/; do
  [ -d "$pkgdir/src" ] && cp -R "$pkgdir/src/." "$WORK/src/"
  for refdir in "$pkgdir"*/; do
    [ -d "$refdir/src" ] && cp -R "$refdir/src/." "$WORK/src/"
  done
done
cp -R packages/purvasm-base/src/. "$WORK/src/"

# stageDeclaredDeps: in-repo packages a ulib manifest declares (strings -> purvasm-regex). Staging
# every in-repo package's src is a superset of exactly-what's-declared, which is fine here (an
# unused extra module costs nothing purs doesn't already tolerate elsewhere in this check).
for d in packages/*/; do
  [ -d "${d}src" ] && cp -R "${d}src/." "$WORK/src/"
done

# overlayPatches: every ulib/<package>/<Module>.purs replaces the staged registry module. The real
# Stage.overlayPatches also drops the sibling registry .js (so a kept `foreign import` resolves to
# a boot intrinsic at link time, not the registry's JS) -- correct for a `--codegen corefn`-only
# build, but this check additionally needs runnable JS, and a patch that still keeps a genuine
# foreign import (e.g. Data.Show's `showNumberImpl`) needs SOME `.js` implementation to satisfy
# `purs --codegen js`. The registry's own sibling is a compatible stand-in for that (same signature,
# ADR-0038), so this check leaves it in place rather than deleting it; the only patch this check
# actually exercises, Data.String.Common, has zero foreign imports left (this file's whole point),
# so its own stale .js sibling is merely an "unnecessary FFI module" warning, not an error.
for f in ulib/*/*.purs; do
  base=$(basename "$f" .purs)
  modpath=$(echo "$base" | tr '.' '/')
  mkdir -p "$WORK/src/$(dirname "$modpath")"
  cp "$f" "$WORK/src/$modpath.purs"
done

purs compile --codegen corefn,js --output "$WORK/out" "$WORK/src/**/*.purs" >"$WORK/purs.log" 2>&1 \
  || { echo "error: purs compile failed:" >&2; cat "$WORK/purs.log" >&2; exit 1; }

node "$(dirname "$0")/verify-case-mapping.mjs" "$WORK/out"
