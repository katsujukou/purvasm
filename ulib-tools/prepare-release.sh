#!/usr/bin/env sh
# One-shot release preparation for the `ulib` patches (ADR-0043): run the full gate, fail-fast.
# Steps are ordered cheapest-first so a problem surfaces before the slow compiles:
#   1. verify-deps             — declared deps resolve (in-repo/registry) and the graph is acyclic
#   2. verify                  — each patch is interface-faithful to its registry module
#   3. build --prepare-release — compile to the distribution location ($PURVASM_LIB, else dist/ulib)
#   4. test                    — behaviour tests by representation-seam fidelity
#
# The ulib-tools commands resolve `ulib` / `packages` / `.spago/p` relative to the working
# directory, so run this from the repo root:
#
#   sh ulib-tools/prepare-release.sh
set -eu

UT="node ulib-tools/index.js"

echo "==> building ulib-tools"
spago build -p ulib-tools

echo "==> [1/4] verify-deps"
$UT verify-deps

echo "==> [2/4] verify"
$UT verify

echo "==> [3/4] build --prepare-release"
$UT build --prepare-release

echo "==> [4/4] test"
$UT test

echo "==> prepare-release: done"
