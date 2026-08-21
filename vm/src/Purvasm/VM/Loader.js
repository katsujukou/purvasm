// JS provider for `Purvasm.VM.Loader` — used by stock `purs` / purs-backend-es builds only; the
// purvasm backend resolves these to the sibling `Loader.c` and ignores this file (ADR-0038).
//
// There is nothing to implement here, and that is the honest answer rather than a gap: the foreign
// frontier resolves `pvf_*` symbols out of a native image, and a Node process hosting a JS-compiled
// VM has no such symbols — not the runtime's, not a provider's. So the two effectful constructors
// fail by name and `resolve` declines, which is exactly what the VM would conclude anyway. A guest
// program that uses no foreign leaf runs identically on both targets; one that does is a native-only
// program, and says so at the first attempt rather than misbehaving later.
const unavailable = "dynamic native providers are unavailable on the JS target (the VM must be compiled natively)";

export const hostRuntimeImpl = () => -1;

export const loadImpl = (_path) => () => -1;

export const loadErrorImpl = () => unavailable;

export const describeImpl = (_index) => "<unloaded>";

export const resolveImpl = (_just) => (nothing) => (_index) => (_symbol) => (_arity) => nothing;

export const declaresImpl = (_index) => (_symbol) => false;
