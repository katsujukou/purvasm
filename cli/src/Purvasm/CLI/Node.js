import { execFileSync } from "node:child_process";
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

// `readFileSync` returns a Buffer, which is a Uint8Array; `writeFileSync` accepts a Uint8Array.
// So the CLI's binary currency stays `Uint8Array` with no Buffer conversion.
export const readFileBytesImpl = (path) => () => readFileSync(path);

export const writeFileBytesImpl = (path) => (bytes) => () => writeFileSync(path, bytes);

export const fileSizeImpl = (path) => () => statSync(path).size;
