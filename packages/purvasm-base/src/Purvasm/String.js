// JS provider for `Purvasm.String`'s byte primitives — used by the `purs` / purs-backend-es builds
// only. The purvasm backend resolves these to its byte-level primitives and ignores this file. On
// purvasm a "byte" is a UTF-8 byte of the `String`; here it is the host string's native code unit (a
// UTF-16 unit), which is the right notion for JS-hosted code. The `Data.String.*` UTF-8 code-point
// layer (ADR-0006) is built for purvasm; on the JS backends the registry `Data.String` is used, not
// the shadow, so these primitives are only exercised by code that imports `Purvasm.String` directly.
// Strings are immutable, so `unsafeSetByte` rebuilds — callers thread the returned value, so this is
// observably the same.
export const byteLength = (s) => s.length;
export const byteAt = (s) => (i) => s.charCodeAt(i);
export const unsafeNew = (n) => "\0".repeat(n);
export const unsafeSetByte = (s) => (i) => (b) => s.slice(0, i) + String.fromCharCode(b) + s.slice(i + 1);

// --- the ADR-0103 bulk surface (provider-relative semantics: here a "byte" is a UTF-16 code
// unit, the code-point ops count Unicode code points on both providers; plain substring/iterator
// code — the view machinery is the purvasm runtime's, not JS's, where V8 already owns slicing).
const cpOffset = (s, k) => {
  let i = 0;
  let c = 0;
  while (c < k && i < s.length) {
    i += s.codePointAt(i) > 0xffff ? 2 : 1;
    c++;
  }
  return i;
};
export const byteSlice = (from) => (to) => (s) => s.slice(from, to);
export const dropCodePoints = (k) => (s) => s.slice(cpOffset(s, k < 0 ? 0 : k));
export const takeCodePoints = (k) => (s) => s.slice(0, cpOffset(s, k < 0 ? 0 : k));
export const codePointLength = (s) => {
  let i = 0;
  let c = 0;
  while (i < s.length) {
    i += s.codePointAt(i) > 0xffff ? 2 : 1;
    c++;
  }
  return c;
};
export const codePointAt = (i) => (s) => {
  if (i < 0) return -1;
  const o = cpOffset(s, i);
  return o < s.length ? s.codePointAt(o) : -1;
};
export const byteIndexOf = (hay) => (needle) => (from) => {
  const f = from < 0 ? 0 : from;
  if (f + needle.length > hay.length) return -1;
  return hay.indexOf(needle, f);
};
export const byteLastIndexOf = (hay) => (needle) => (from) => {
  if (needle.length > hay.length) return -1;
  const f = Math.min(from < 0 ? 0 : from, hay.length - needle.length);
  return hay.lastIndexOf(needle, f);
};
export const compareBytes = (a) => (b) => (a < b ? -1 : a > b ? 1 : 0);
export const appendBulk = (a) => (b) => a + b;
export const materialize = (s) => s;
