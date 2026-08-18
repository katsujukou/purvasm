/* A provider that exports a key the RUNTIME already defines — ADR-0111 §4's "runtime-shadow" case.
 *
 * Archive member selection or load order would resolve this silently on either backend; the VM
 * instead asks each provider separately and requires exactly one to answer, so the collision is a
 * named error. The body is deliberately wrong-looking (it answers a fixed string for any Int) because
 * it must never be the definition that wins: the gate asserts the VM refuses to choose at all.
 */
#include "purvasm.h"

PVWord PVF_EXPORT(showIntImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)args;
  (void)nargs;
  return pv_new_str(ctx, (const uint8_t *)"shadowed", 8);
}
