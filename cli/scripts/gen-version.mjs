// Generate `Purvasm.CLI.Version.Generated` from `package.json` (the single source of truth for
// the CLI version). Run by the `version` npm script (the `npm version` lifecycle hook).
//
// The output is a plain PureScript constant — NOT an FFI — so the version resolves on every
// backend (purvasm-native has no JS at runtime), while `package.json` stays the source of truth.
import { readFileSync, writeFileSync } from "node:fs";

const { version } = JSON.parse(
  readFileSync(new URL("../package.json", import.meta.url)),
);

const out = `-- Generated from package.json by \`npm version\`; do not edit by hand.
module Purvasm.CLI.Version.Generated (version) where

version :: String
version = "v${version}"
`;

writeFileSync(
  new URL("../src/Purvasm/CLI/Version/Generated.purs", import.meta.url),
  out,
);
