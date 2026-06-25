// JS provider for `Purvasm.Array`'s first-order primitives — used by stock `purs` /
// purs-backend-es builds only. The purvasm backend resolves these to intrinsics.
export const length = (a) => a.length;
export const unsafeIndex = (a) => (i) => a[i];
export const unsafeNew = (n) => new Array(n);
export const unsafeSet = (a) => (i) => (v) => {
  a[i] = v;
  return a;
};
