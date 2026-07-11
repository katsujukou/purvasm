// JS provider for `Purvasm.Number` — used by stock `purs` / purs-backend-es builds only. The
// purvasm backend resolves these to intrinsics (the machine primops) and ignores this file.
export const add = (a) => (b) => a + b;
export const sub = (a) => (b) => a - b;
export const mul = (a) => (b) => a * b;
export const div = (a) => (b) => a / b;
export const eq = (a) => (b) => a === b;
export const lt = (a) => (b) => a < b;

// float-bits reads: DataView defaults to big-endian, so offset 0 is the high word.
const floatBitsView = new DataView(new ArrayBuffer(8));
export const floatBitsHi = (f) => {
  floatBitsView.setFloat64(0, f);
  return floatBitsView.getInt32(0);
};
export const floatBitsLo = (f) => {
  floatBitsView.setFloat64(0, f);
  return floatBitsView.getInt32(4);
};

// Full-consumption decimal parse matching the native runtime leaf (Rust `str::parse::<f64>`): reject
// surrounding whitespace, the empty string, and the JS-only numeric forms `Number` accepts but the
// native parse rejects — hex/binary/octal literals (`0x…`/`0b…`/`0o…`). A non-number then yields NaN,
// which the guest `fromString` folds to `Nothing`. Only stock `purs`/purs-backend-es builds reach this
// stub; the purvasm backend resolves `parseFloat` to the runtime leaf and ignores it.
export const parseFloat = (s) => {
  if (s.length === 0 || s.trim() !== s || /^[+-]?0[xXbBoO]/.test(s)) return NaN;
  return Number(s);
};
