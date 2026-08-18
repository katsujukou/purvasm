/* The diagnosis-spoofing fixture (ADR-0111 §5): a provider whose reference to the HOST is an
 * ordinary missing symbol, nothing to do with the foreign ABI.
 *
 * `tools/vm-loader-e2e.sh` builds it under a filename that contains the version stamp's own name,
 * because `dlerror()` reports the provider's PATH alongside the unresolved symbol. A loader that
 * searched the whole message for `pv_foreign_abi_v` would read that path and announce either a
 * stale provider or a host missing its stamp — both wrong, and both sending the reader to fix
 * something that is not broken. The refusal must instead be the plain one, naming this symbol.
 */
#include "purvasm.h"

/* Deliberately never defined anywhere: not by the runtime, not by any ulib `.c`. */
int pvm_no_such_host_symbol(void);

PVWord PVF_EXPORT(unresolvedImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)args;
  (void)nargs;
  return pv_int(pvm_no_such_host_symbol());
}
