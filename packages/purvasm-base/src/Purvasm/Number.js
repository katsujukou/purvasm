// JS provider for `Purvasm.Number` — used by stock `purs` / purs-backend-es builds only. The
// purvasm backend resolves these to intrinsics (the machine primops) and ignores this file.
export const add = (a) => (b) => a + b;
export const sub = (a) => (b) => a - b;
export const mul = (a) => (b) => a * b;
export const div = (a) => (b) => a / b;
export const eq = (a) => (b) => a === b;
export const lt = (a) => (b) => a < b;
