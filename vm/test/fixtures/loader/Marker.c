/* The ADR-0111 §5 stale-module fixture: a provider that announces itself from a **module
 * initialiser**, built twice by `tools/vm-loader-e2e.sh` — once against the real `purvasm.h`, once
 * against a copy whose `PV_FOREIGN_ABI_VERSION` has been bumped.
 *
 * The marker is the whole point. `dlopen` runs initialisers before it returns, so a version check
 * performed *after* loading would run only once the module had already called into a `pv_*` surface
 * it disagrees with. The two builds separate the two claims a passing exit code cannot:
 *
 *   - the current-version build loads, and the marker appears — so the marker works, and its absence
 *     below means something;
 *   - the bumped build fails to load, and the marker does NOT appear — so the refusal happened at
 *     symbol resolution, before any of this module's code ran.
 */
#include "purvasm.h"

#include <stdio.h>

__attribute__((constructor)) static void pvm_marker(void) {
  fputs("MARKER: provider initialiser ran\n", stderr);
  fflush(stderr);
}

PVWord PVF_EXPORT(markerImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)args;
  (void)nargs;
  /* One ordinary API call, so this is a real provider rather than an empty object: the module must
   * resolve `pv_*` against the host exactly as any leaf does. */
  return pv_new_str(ctx, (const uint8_t *)"marker", 6);
}
