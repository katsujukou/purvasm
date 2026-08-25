// JS provider for `Purvasm.VM.Host` — stock `purs` / purs-backend-es builds only; the purvasm backend
// resolves this to the sibling `Host.c` and ignores this file (ADR-0038).
//
// A no-op, and honestly so: there is no embedded runtime to configure on the JS target, and the argv
// a JS-hosted guest would read is the Node process's either way. Failing here would stop a VM that
// runs perfectly well as long as its guest ignores `argv`.
export const setGuestArgvImpl = (_argv) => () => {};
