// Node implementation (dual-target, ADR-0038); on purvasm boot ignores this and binds each leaf to a
// host leaf (`print_string`/`print_newline` on stdout, `prerr_string`/`prerr_newline` on stderr).
import process from "node:process";

export const writeLineImpl = (s) => () => {
  process.stdout.write(s + "\n");
};
export const writeErrLineImpl = (s) => () => {
  process.stderr.write(s + "\n");
};
