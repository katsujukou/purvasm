// Node implementation (dual-target, ADR-0038); on purvasm boot ignores this and binds the leaf to
// a host leaf. "" when unset, matching the native `Sys.getenv_opt` leaf; the wrapper folds "" to
// `Nothing`.
import process from "node:process";

export const getenvImpl = (name) => () => process.env[name] ?? "";
