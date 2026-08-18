// JS provider for `Purvasm.VM.Foreign` — used by stock `purs` / purs-backend-es builds only; the
// purvasm backend resolves this to the sibling `Foreign.c` and ignores this file (ADR-0038).
//
// There is no runtime here to apply anything with. A guest program that never touches a foreign leaf
// runs identically on both targets; one that does is a native-only program, and this says so at the
// point of the call rather than misbehaving later.
export const applyImpl = (_f) => (_args) => () => {
  throw new Error("purvasm vm: native application is unavailable on the JS target (the VM must be compiled natively)");
};
