// Node implementation (dual-target, ADR-0038); on purvasm boot ignores this and binds each leaf to
// a host leaf (`In_channel`/`Out_channel`/`mkdir_p`/`Sys.file_exists`).
import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";

export const readTextImpl = (path) => () => readFileSync(path, "utf8");
export const existsImpl = (path) => () => existsSync(path);
export const writeTextImpl = (path) => (contents) => () => writeFileSync(path, contents, "utf8");
export const mkdirRecImpl = (path) => () => {
  mkdirSync(path, { recursive: true });
};
