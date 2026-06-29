// Node implementation (dual-target, ADR-0038); on purvasm boot ignores this and binds the leaf to
// a host leaf (`Sys.argv`). Drop the runtime ("node") so element 0 is the script, mirroring
// `Sys.argv` where element 0 is the executable.
import process from "node:process";

export const argvImpl = () => process.argv.slice(1);

export const exitImpl = (code) => () => process.exit(code);
