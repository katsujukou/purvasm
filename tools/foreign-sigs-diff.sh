#!/usr/bin/env bash
# The ADR-0080 §2 consistency differential: for every foreign key BOTH boot's hand registry
# and the Level-2 source reconstruction know, (arity, effectful) must agree — the standing
# check that the two sources cannot drift while boot remains in the build path.
#
# Usage (repo root, inside `nix develop`):  tools/foreign-sigs-diff.sh [ENTRY_MODULE]
set -euo pipefail
ENTRY="${1:-Purvasm.CLI.Native}"
PURVM="${PURVM:-boot/_build/default/bin/main.exe}"
OUT=$(mktemp -d)
trap 'rm -rf "$OUT"' EXIT

PURVASM_LIB="${PURVASM_LIB:-dist/ulib}" node cli/index.node.js foreign-sigs \
  --corefn-dir output --entry "$ENTRY" > "$OUT/lv2.json"
node -e 'console.log(Object.keys(JSON.parse(require("fs").readFileSync(process.argv[1]))).join("\n"))' \
  "$OUT/lv2.json" > "$OUT/keys"
"$PURVM" foreign-sig-dump < "$OUT/keys" > "$OUT/boot.json"

node - "$OUT/lv2.json" "$OUT/boot.json" <<'JS'
const fs = require("fs");
const [lv2, boot] = process.argv.slice(2).map((p) => JSON.parse(fs.readFileSync(p)));
let shared = 0, bad = 0;
for (const [k, bs] of Object.entries(boot)) {
  shared++;
  const ls = lv2[k];
  // boot's registry encoding for a ZERO-arrow perform leaf is "the entry IS the thunk"
  // (arity 1, the unit application — e.g. argvImpl); an n-arrow leaf is registered at its
  // arrow count and RETURNS the thunk. The reconstruction's canonical shape is
  // (arrows, retVsat), so normalize the arity before comparing. (This differential's first
  // run surfaced exactly this undocumented convention.)
  const expectedBootArity = ls && ls.arity === 0 && ls.retVsat ? 1 : ls && ls.arity;
  // The ADR-0034 dual summary. Boot has no vsat leaves (no uncurried EffectFn), so its
  // dump always carries vsat=false; the reconstruction's vsat is compared verbatim.
  if (
    !ls ||
    expectedBootArity !== bs.arity ||
    ls.vsat !== bs.vsat ||
    ls.retVsat !== bs.retVsat
  ) {
    bad++;
    console.error(`DISAGREE ${k}: lv2=${JSON.stringify(ls)} boot=${JSON.stringify(bs)}`);
  }
}
console.log(`foreign-sigs-diff: ${shared} shared keys, ${bad} disagreements`);
process.exit(bad ? 1 : 0);
JS
