// JS provider for `Purvasm.VM.Foreign` — used by stock `purs` / purs-backend-es builds only; the
// purvasm backend resolves this to the sibling `Foreign.c` and ignores this file (ADR-0038).
//
// There is no runtime here to apply anything with. A guest program that never touches a foreign leaf
// runs identically on both targets; one that does is a native-only program, and this says so at the
// point of the call rather than misbehaving later.
export const applyImpl = (_f) => (_args) => () => {
  throw new Error("purvasm vm: native application is unavailable on the JS target (the VM must be compiled natively)");
};

// The array-promotion half (ADR-0111 §3). Unreachable on this target for the same reason as
// `applyImpl`: promotion exists to hand a runtime object to a leaf, and there are no leaves here.
const unavailable = (what) => () => {
  throw new Error(`purvasm vm: ${what} is unavailable on the JS target (the VM must be compiled natively)`);
};

export const blankArrayImpl = (_n) => unavailable("array promotion");

export const arrayLengthImpl = (_a) => 0;

export const readFieldImpl = (_a) => (_i) => unavailable("a promoted array read");

export const writeFieldImpl = (_a) => (_i) => (_v) => unavailable("a promoted array write");

// The use-site decoders (ADR-0111 §3). A carrier cannot exist on this target — nothing produces one —
// so reaching these means a VM defect rather than a guest one, and they say so.
const noCarrier = (what) => {
  throw new Error(`purvasm vm: ${what} on the JS target, where no carrier can exist (a VM defect)`);
};

export const intOfImpl = (_v) => noCarrier("an Int decode");

export const numberOfImpl = (_v) => noCarrier("a Number decode");

export const booleanOfImpl = (_v) => noCarrier("a Boolean decode");

export const stringOfImpl = (_v) => noCarrier("a String decode");

export const forceCarrierImpl = (_v) => () => noCarrier("a carrier force");

export const newAdtImpl = (_tag) => (_fields) => unavailable("an ADT construction");

export const adtTagImpl = (_v) => noCarrier("a constructor-tag read");

export const adtFieldImpl = (_v) => (_i) => unavailable("an ADT field read");

export const newNullaryAdtImpl = (_tag) => noCarrier("a nullary ADT construction");
