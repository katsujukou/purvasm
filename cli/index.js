#!/usr/bin/env node
// The purvasm CLI entry point (Level-3): a thin launcher that runs the native `purvasm` binary as a
// subprocess (ADR-0055). The native compiler is fast but cannot portably locate its own install
// directory (the OCaml backend is a bootstrap that will be replaced by a self-hosted native backend),
// so *this* launcher — which can locate itself via `import.meta.url` regardless of runtime — resolves
// the bundled `ulib` and hands it to the binary through the `PURVASM_LIB` environment variable. The
// binary reads `PURVASM_LIB` to overlay the patched standard library; users never see `ulib`.
//
// (The pure-JS, Node-interpreted compiler still lives in `index.node.js` for dev/fallback.)
import { spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));

// The native bundle, resolved relative to this launcher (so it travels with the package, not the cwd).
const binary = join(here, "bin", process.platform === "win32" ? "purvasm.exe" : "purvasm");
const ulibDir = join(here, "bin", "ulib");

if (!existsSync(binary)) {
  console.error(
    `purvasm: native binary not found at ${binary}\n` +
      `Build it with: sh cli/scripts/build-native.sh`,
  );
  process.exit(127);
}

// ADR-0055: provide the ulib location through the environment. Honour an explicit PURVASM_LIB
// (dev/packaging override); otherwise point at the bundled ulib if present. If neither exists the
// binary degrades to an un-overlaid build (it logs that itself), so we do not force the variable.
const env = { ...process.env };
if (!env.PURVASM_LIB && existsSync(ulibDir)) {
  env.PURVASM_LIB = ulibDir;
}

const result = spawnSync(binary, process.argv.slice(2), { stdio: "inherit", env });

if (result.error) {
  console.error(`purvasm: failed to launch ${binary}: ${result.error.message}`);
  process.exit(1);
}
// Propagate the child's exit; a signal-terminated child has a null status.
process.exit(result.status === null ? 1 : result.status);
