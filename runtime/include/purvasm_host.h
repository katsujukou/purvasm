/* purvasm_host.h — the **host-control** C surface, for a program that EMBEDS the purvasm runtime and
 * runs another purvasm program inside it (today: the owned VM, ADR-0110).
 *
 * This header is deliberately not `purvasm.h`. That one is the contract a *foreign leaf author*
 * writes against, and everything it declares is retained and dynamically exported by a
 * `--host-foreign-api` executable so that `dlopen`ed providers can bind it (ADR-0111 §1.1). Nothing
 * here is: these entries configure the runtime *for* a guest, so a guest reaching one would be
 * rewriting the very context it runs in. The separation is what keeps that unrepresentable — the
 * symbols are absent from the allowlist, therefore absent from the executable's dynamic exports,
 * therefore unbindable by a provider and unreachable from a guest `ForeignRef`.
 *
 * The audience is a runner's own trusted C, compiled into the runner (the VM's `Purvasm/VM/Host.c`).
 * It is NOT mirrored in `purvasm-sys`, which is the Rust DX layer over the *author* API (ADR-0078).
 *
 * These do not extend `PV_FOREIGN_ABI_VERSION`: no provider can reference them, so no provider's
 * compatibility depends on them.
 */
#ifndef PURVASM_HOST_H
#define PURVASM_HOST_H

#include "purvasm.h"

/* Replace the argv the guest of this context observes — what `Purvasm.System.Process.argvImpl`
 * reports from here on (ADR-0075 §4's convention: element 0 the image, then the guest's own
 * arguments). `argv` is an `Array String` value on this context.
 *
 * Per CONTEXT, not per process: two contexts hosting two guests keep their argvs apart. Until a host
 * calls this, a context reports the process argv, so a compiled program with no host above it is
 * unaffected.
 *
 * The strings are copied out; the array is not retained, and holding it after this call is the
 * caller's business as usual (it is an ordinary value that the next allocation may move).
 */
void pv_runtime_set_guest_argv(PVContext *ctx, PVWord argv);

#endif /* PURVASM_HOST_H */
