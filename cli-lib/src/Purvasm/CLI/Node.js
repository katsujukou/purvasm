import { execFileSync, spawnSync } from "node:child_process";
import { readFileSync, writeFileSync, statSync } from "node:fs";

export const execFileImpl = (cmd) => (args) => () => {
  execFileSync(cmd, args, { stdio: "inherit" });
};

// Pipe `input` to the child's stdin; DISCARD its stdout but inherit its stderr. Used to feed a
// long-lived `purwc` batch worker its whole module work-list in one spawn (ADR 0038 C2). The batch
// worker is silent on stdout by design (the orchestrator owns the build's progress display), so
// discarding it just drops the dev-build banner; a worker error still surfaces (it logs to stderr).
export const execFileInputImpl = (cmd) => (args) => (input) => () => {
  execFileSync(cmd, args, { input, stdio: ["pipe", "ignore", "inherit"], maxBuffer: 1e9 });
};

// Read all of this process's stdin synchronously (fd 0). The batch worker's work-list.
export const readStdinImpl = () => readFileSync(0, "utf8");

// Capture stdout as text (the registry query in `ulib compat`). A large maxBuffer matches the
// prototype — `spago registry info --json` payloads can be sizeable.
export const execFileCaptureImpl = (cmd) => (args) => () =>
  execFileSync(cmd, args, { encoding: "utf8", maxBuffer: 1e8 });

// Capture stdout, DISCARD stderr (stdio[2] = "ignore"): a tool whose stderr is benign noise (e.g.
// `llvm-nm`'s "no symbols" per empty archive member) then does not leak to the terminal.
export const execFileCaptureQuietImpl = (cmd) => (args) => () =>
  execFileSync(cmd, args, {
    encoding: "utf8",
    maxBuffer: 1e8,
    stdio: ["ignore", "pipe", "ignore"],
  });

// `readFileSync` returns a Buffer, which is a Uint8Array; `writeFileSync` accepts a Uint8Array.
// So the CLI's binary currency stays `Uint8Array` with no Buffer conversion.
export const readFileBytesImpl = (path) => () => readFileSync(path);

export const writeFileBytesImpl = (path) => (bytes) => () => writeFileSync(path, bytes);

export const fileSizeImpl = (path) => () => statSync(path).size;

// Run a tool with stdio inherited and report its exit STATUS rather than collapsing it to a throw.
// `spawnSync` is used instead of `execFileSync` because that is the difference: `execFileSync` throws
// on a non-zero exit and the code survives only inside an error object, while a launcher needs the
// number itself. `status` is null when the child was killed, in which case `signal` names it.
export const execFileStatusImpl = (cmd) => (args) => () => {
  const r = spawnSync(cmd, args, { stdio: "inherit" });
  if (r.error) return { spawned: false, status: 1, signal: "", message: r.error.message };
  return { spawned: true, status: r.status === null ? 1 : r.status, signal: r.signal ?? "", message: "" };
};
