#!/usr/bin/env sh
# One-shot release preparation for the `ulib` patches (ADR-0043): run the full gate, fail-fast.
# Steps are ordered cheapest-first so a problem surfaces before the slow compiles:
#   1. unicode-gen --check     — Data.String.Internal.CaseMap.purs matches the pinned UCD (ADR-0101)
#   2. verify-deps             — declared deps resolve (in-repo/registry) and the graph is acyclic
#   3. verify                  — each patch is interface-faithful to its registry module
#   4. build --prepare-release — compile to the distribution location ($PURVASM_LIB, else dist/ulib)
#   5. test                    — behaviour tests by representation-seam fidelity
#
# The ulib-tools commands resolve `ulib` / `packages` / `.spago/p` relative to the working
# directory, so run this from the repo root:
#
#   sh ulib-tools/prepare-release.sh
set -eu

UT="node ulib-tools/index.js"

echo "==> building ulib-tools"
spago build -p ulib-tools

echo "==> [1/5] unicode-gen --check"
$UT unicode-gen --check

echo "==> [2/5] verify-deps"
$UT verify-deps

echo "==> [3/5] verify"
$UT verify

echo "==> [4/5] build --prepare-release"
$UT build --prepare-release

echo "==> [5/5] test"
$UT test

echo "==> prepare-release: done"
