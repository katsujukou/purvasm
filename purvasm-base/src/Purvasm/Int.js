// JS provider for `Purvasm.Int` — used by stock `purs` / purs-backend-es builds only. The
// purvasm backend resolves these to intrinsics (the machine primops) and ignores this file.
export const add = (a) => (b) => (a + b) | 0;
export const sub = (a) => (b) => (a - b) | 0;
export const mul = (a) => (b) => Math.imul(a, b);
export const eq = (a) => (b) => a === b;
export const lt = (a) => (b) => a < b;
// Euclidean div / remainder (PureScript's EuclideanRing Int, 4.x+): non-negative
// remainder; 0 on a zero divisor. (Truncating division is `quot`/`rem`, not these.)
export const div = (a) => (b) => (b === 0 ? 0 : b > 0 ? Math.floor(a / b) : Math.ceil(a / b));
export const mod = (a) => (b) => {
  if (b === 0) return 0;
  const m = Math.abs(b);
  return ((a % m) + m) % m;
};
