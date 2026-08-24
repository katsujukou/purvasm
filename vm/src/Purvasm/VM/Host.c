/* The owned VM's host-control provider (ADR-0110 §4(a) Correction), built as the app-C sibling of
 * `Host.purs` with -DPVF_MODULE=Purvasm_2eVM_2eHost.
 *
 * Trusted C, like `Loader.c`, and for a sharper reason: `purvasm_host.h` declares entries that
 * configure the runtime *for* a guest, so this file is the only place in the executable that calls
 * them. They are not in `purvasm.h`, hence not in the export allowlist, hence not in the dynamic
 * exports a `--host-foreign-api` executable offers — a loaded provider has no symbol to bind and a
 * guest `ForeignRef` has no name to resolve.
 */
#include "purvasm_host.h"

#include <stddef.h>
#include <stdint.h>

static PVWord pvm_set_guest_argv_thunk(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs);

/* `setGuestArgvImpl :: Array String -> Effect Unit` — the outer `\argv -> thunk`: saturating an
 * effectful leaf builds the action, it does not perform it (ADR-0067 §3/§5). */
PVWord PVF_EXPORT(setGuestArgvImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  PVWord env = pv_new_array(ctx, args, 1);
  return pv_make_closure(ctx, (uint64_t)(uintptr_t)&pvm_set_guest_argv_thunk, 1, env);
}

/* The action: hand the captured array to the runtime, which copies the strings out. */
static PVWord pvm_set_guest_argv_thunk(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)args;
  (void)nargs;
  PVWord env = pv_closure_env(ctx, clo);
  pv_runtime_set_guest_argv(ctx, pv_read_field(ctx, env, 0));
  return pv_unit();
}
