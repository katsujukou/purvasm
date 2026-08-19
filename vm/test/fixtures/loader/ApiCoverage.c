/* The ADR-0111 §1.1 API-coverage provider: a module that references EVERY function of the
 * foreign-author API, built as a shared object and loaded by the VM (`tools/vm-loader-e2e.sh`).
 *
 * Loading is the assertion, and nothing here is ever called. `dlopen` with `RTLD_NOW` (ADR-0111 §6)
 * binds every reference the module makes before it returns, so a `pv_*` the VM's link failed to
 * retain — or retained but did not export — makes this module fail to load, by name. That is the
 * only way to see the gap: the VM itself calls almost none of this API, so a build that dropped
 * `pv_new_record` links, runs, and passes every other test.
 *
 * Coverage is therefore the point of the file, and it is exhaustive rather than one-per-group: the
 * retained set is derived from this header (`Purvasm.CLI.NativeLink.foreignAuthorApi`), so an entry
 * that exists in the header and not here would be untested retention.
 */
#include "purvasm.h"

/* Signature contract, for the slice that eventually *calls* this: one argument, and it must be a
 * one-argument function — it is what `pv_apply` is exercised on. Applying the leaf's own closure
 * would have covered the same symbol while making the first real call recurse forever.
 *
 * The body is also written to the rooting contract it is sampling, not merely to touch symbols: the
 * argument is rooted and reloaded, and every heap value is read down to a C scalar *before* the
 * allocating apply at the end, so nothing here holds a word across a safepoint. A fixture that
 * demonstrates the API by violating its contract would be the wrong thing to have in the tree, even
 * while it is only ever loaded. */
PVWord PVF_EXPORT(apiCoverageImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;

  PVWord mark = pv_frame(ctx);
  PVWord held = pv_root(ctx, args[0]);

  /* Constructors, each read back immediately by its matching accessor — so a mismatched pairing
     faults when this leaf is called rather than passing silently. Every read happens before the
     next allocating call, which is why none of these words is rooted. */
  PVWord str = pv_new_str(ctx, (const uint8_t *)"api", 3);
  uint8_t buf[8];
  size_t len = pv_str_len(ctx, str);
  size_t copied = pv_str_copy(ctx, str, buf, sizeof buf);

  PVWord num = pv_new_number(ctx, 0x4000000000000000ull); /* 2.0 */
  uint64_t bits = pv_number_bits(ctx, num);

  PVWord elems[2] = {pv_int(1), pv_bool(1)};
  PVWord arr = pv_new_array(ctx, elems, 2);
  int32_t i = pv_int_payload(ctx, pv_read_field(ctx, arr, 0));
  int b = pv_bool_payload(ctx, pv_read_field(ctx, arr, 1));
  size_t n = pv_array_len(ctx, arr) + pv_array_len(ctx, pv_empty_array());

  PVWord adt = pv_new_adt(ctx, 7, elems, 2);
  /* An Adt's payload is [tag] ++ fields, so field 0 is payload word 1 — reading word 0 would hand
     back the raw tag, which is not a value word. */
  int32_t field = pv_int_payload(ctx, pv_read_field(ctx, adt, 1));
  uint32_t tag = pv_adt_tag(ctx, adt) + pv_adt_tag(ctx, pv_new_nullary_adt(9));

  PVWord ids[1] = {pv_int(0)};
  PVWord vals[1] = {pv_int(1)};
  PVWord rec = pv_new_record(ctx, ids, vals, 1);
  PVWord got = pv_record_get(ctx, rec, ids[0]);
  int32_t updated = pv_int_payload(ctx, pv_record_get(ctx, pv_record_set(ctx, rec, ids[0], pv_int(2)), ids[0]));

  PVWord ref = pv_new_ref(ctx, pv_unit());
  pv_write_field(ctx, ref, 0, got);
  int32_t stored = pv_int_payload(ctx, pv_read_field(ctx, ref, 0));

  PVWord fn = pv_make_closure(ctx, (uint64_t)(uintptr_t)&PVF_EXPORT(apiCoverageImpl), 1, pv_empty_array());
  size_t captures = pv_array_len(ctx, pv_closure_env(ctx, fn));

  /* Application / forcing, last: the argument is reloaded through its root (everything above was a
     safepoint), and it is applied to an immediate, which cannot move. */
  PVWord forced = pv_force_if_byneed(ctx, pv_get(ctx, held));
  PVWord arg = pv_int(1);
  PVWord applied = pv_apply(ctx, forced, &arg, 1);
  int32_t answer = pv_int_payload(ctx, applied);

  pv_pop_frame(ctx, mark);
  return pv_int((int32_t)(len + copied + n + captures) + i + b + (int32_t)(bits >> 62) + field +
                updated + stored + answer + (int32_t)tag);
}
