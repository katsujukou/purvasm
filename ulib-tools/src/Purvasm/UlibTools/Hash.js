import { readFileSync } from "node:fs";
import { createHash } from "node:crypto";

// Hash the file's raw bytes (not a decoded/re-encoded string), so the digest pins exactly the
// bytes that were downloaded, independent of any text encoding concerns.
export const sha256FileImpl = (path) => () =>
  createHash("sha256").update(readFileSync(path)).digest("hex");
