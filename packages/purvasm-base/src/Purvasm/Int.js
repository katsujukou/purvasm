// JS provider for `Purvasm.Int` — used by stock `purs` / purs-backend-es builds only. The
// purvasm backend resolves these to intrinsics (the machine primops) and ignores this file.
export const add = (a) => (b) => (a + b) | 0;
export const sub = (a) => (b) => (a - b) | 0;
export const mul = (a) => (b) => Math.imul(a, b);
export const eq = (a) => (b) => a === b;
export const lt = (a) => (b) => a < b;
// Euclidean div / remainder (PureScript's EuclideanRing Int, 4.x+): non-negative
// remainder; 0 on a zero divisor. (Truncating division is `quot`/`rem`, not these.)
//
// The trailing `| 0` is `ToInt32`, and it matters at exactly one input pair: `bottom / -1` is 2^31,
// one past `Int`'s maximum. The registry's `intDiv` (and hence stock `purs`) returns that unwrapped
// value, which is not an `Int` and which the native representation cannot hold (`pv_int` takes an
// `int32_t`); purvasm wraps it to `bottom` on every target instead — a deliberate, recorded
// divergence from stock `purs` at that pair (ADR-0112). Every other operation in this file already
// normalises; this one was the omission.
export const div = (a) => (b) => (b === 0 ? 0 : b > 0 ? Math.floor(a / b) : Math.ceil(a / b)) | 0;
export const mod = (a) => (b) => {
  if (b === 0) return 0;
  const m = Math.abs(b);
  return ((a % m) + m) % m;
};
// Bitwise ops. JS `<<`/`>>`/`>>>`/`&`/`|`/`^`/`~` already operate on signed 32-bit ints
// (the count is masked to 5 bits), matching the purvasm intrinsics; `>>>` yields an unsigned
// result, so re-wrap to signed 32 with `| 0` to keep `Int`'s invariant.
export const and = (a) => (b) => a & b;
export const or = (a) => (b) => a | b;
export const xor = (a) => (b) => a ^ b;
export const shl = (a) => (b) => a << b;
export const shr = (a) => (b) => a >> b;
export const zshr = (a) => (b) => (a >>> b) | 0;
export const complement = (a) => ~a;
// Int<->Number casts (ADR-0041). On JS they are one value: `toNumber` is the identity and
// `fromNumber` is the `n | 0` (ToInt32) coercion — matching the registry `Data.Int`'s own JS.
export const toNumber = (n) => n;
export const fromNumber = (n) => n | 0;

