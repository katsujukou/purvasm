/* app-C native provider (ADR-0091 §2): built with -DPVF_MODULE=AppC, so PVF_EXPORT(addSeven) exports
 * pvf_AppC_2eaddSeven. */
#include "purvasm.h"

PVWord PVF_EXPORT(addSeven)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  return pv_int(pv_int_payload(ctx, args[0]) + 7);
}
