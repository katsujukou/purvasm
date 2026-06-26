// Node implementation of the native IO leaves (ADR-0045), for the dual-target stock-`purs`/Node
// build only. On purvasm the backend ignores this file and binds each name to a host leaf.
import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import process from "node:process";

export const readTextImpl = (path) => () => readFileSync(path, "utf8");
export const existsImpl = (path) => () => existsSync(path);
export const writeTextImpl = (path) => (contents) => () => writeFileSync(path, contents, "utf8");
export const mkdirRecImpl = (path) => () => {
  mkdirSync(path, { recursive: true });
};
// Drop the runtime ("node"); element 0 is then the script, mirroring `Sys.argv` on purvasm
// (element 0 = the executable). `main` drops that one element to get the user arguments.
export const argvImpl = () => process.argv.slice(1);
