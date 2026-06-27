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
