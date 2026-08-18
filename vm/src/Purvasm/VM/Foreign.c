/* The FFI boundary's one native operation (ADR-0111 §2), built as the app-C sibling of
 * `Foreign.purs` with -DPVF_MODULE=Purvasm_2eVM_2eForeign.
 *
 * Only `pv_apply` needs C. Going in, a VM scalar or string IS already a runtime value — the VM is a
 * purvasm program on the same heap — so conversion is a matter of type, not of code, and it lives in
 * `Foreign.purs`. Coming out, there is nothing to convert: the result stays opaque.
 *
 * This file is trusted surface in the same sense `Loader.c` is, and for the same reason: it hands
 * guest-reachable code a way to call an arbitrary runtime closure. Neither is exported from the VM
 * executable — the link's allowlist names only the foreign-author API and the runtime's own leaves
 * (ADR-0111 §1.1), and `tools/vm-loader-e2e.sh` asserts that set exactly.
 */
#include "purvasm.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

static PVWord pvm_apply_thunk(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs);

/* `applyImpl :: ForeignValue -> Array ForeignValue -> Effect ForeignValue` — the outer leaf, whose
 * saturation BUILDS the action rather than performing it (ADR-0067 §3/§5). Applying a leaf runs
 * guest-visible effects, so the `Effect` is not decoration: a pure declaration here would let the
 * optimiser duplicate or drop a write. */
PVWord PVF_EXPORT(applyImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  PVWord env = pv_new_array(ctx, args, 2);
  return pv_make_closure(ctx, (uint64_t)(uintptr_t)&pvm_apply_thunk, 1, env);
}

/* The action: read back the captured function and argument vector, then `pv_apply`.
 *
 * The rooting is the whole difficulty, and the argument is precisely this. ADR-0066 §3 says a
 * `PVWord` a leaf holds stays valid until the *next* allocating `pv_*` call — no more than that.
 * So:
 *
 *   - the function and the array are held through ROOTS, because they must survive `pv_apply`, which
 *     moves the heap by design, and a root is the only thing that survives it;
 *   - the raw words in `buf` are NOT rooted, and do not need to be: they are read out with
 *     `pv_read_field` (which does not allocate) with no allocating call between that read-out and
 *     the `pv_apply` that consumes them, so every one of them is still valid at the instant it is
 *     handed over. `malloc` is not a `pv_*` call and cannot move the guest heap.
 *
 * What happens to those words *inside* `pv_apply` is the runtime's business, not this file's — the
 * contract owed here is that they are valid when passed, and the ordering above is what discharges
 * it. (Note that the header's self-rooting promise is made for the `pv_*` **constructors**; leaning
 * on it for `pv_apply` would be assuming more than the ABI states.) */
static PVWord pvm_apply_thunk(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)args;
  (void)nargs;

  PVWord mark = pv_frame(ctx);
  PVWord env = pv_closure_env(ctx, clo);
  PVWord fn = pv_root(ctx, pv_read_field(ctx, env, 0));
  PVWord vec = pv_root(ctx, pv_read_field(ctx, env, 1));

  size_t n = pv_array_len(ctx, pv_get(ctx, vec));
  PVWord *buf = NULL;
  if (n > 0) {
    buf = (PVWord *)malloc(n * sizeof(PVWord));
    if (buf == NULL) {
      fputs("purvasm vm foreign: out of memory building an argument vector\n", stderr);
      abort();
    }
    /* No allocating call between this read-out and `pv_apply` — that is what makes the raw words
       below safe to hold at all. */
    PVWord vector = pv_get(ctx, vec);
    for (size_t i = 0; i < n; i++) buf[i] = pv_read_field(ctx, vector, i);
  }

  PVWord result = pv_apply(ctx, pv_get(ctx, fn), buf, n);
  free(buf);
  pv_pop_frame(ctx, mark);
  return result;
}

/* ── Array promotion's native half (ADR-0111 §3) ────────────────────────────────────────────────────
 *
 * The ABI has no blank-array constructor — `pv_new_array` wants `n` already-built words — which is
 * why promotion cannot be "allocate, then fill" without help. `blankArrayImpl` builds the array over
 * `n` copies of `pv_unit()` so the cell can be forwarded BEFORE any element is migrated, which is
 * what makes a cyclic array terminate (§3's step 3, and the same trick a copying collector's
 * forwarding pointer is). It also answers `n = 0` with the canonical empty array, so there is no
 * nullary entry here: a `foreign import` of arity 0 reaches the VM as an arity-0 CLOSURE rather than
 * a value, and a leaf handed that gets a `Closure` where it expected an `Array`.
 */

static PVWord pvm_blank_thunk(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs);
static PVWord pvm_read_thunk(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs);
static PVWord pvm_write_thunk(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs);

/* `blankArrayImpl :: Int -> Effect ForeignValue` */
PVWord PVF_EXPORT(blankArrayImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  PVWord env = pv_new_array(ctx, args, 1);
  return pv_make_closure(ctx, (uint64_t)(uintptr_t)&pvm_blank_thunk, 1, env);
}

static PVWord pvm_blank_thunk(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)args;
  (void)nargs;
  PVWord env = pv_closure_env(ctx, clo);
  int32_t n = pv_int_payload(ctx, pv_read_field(ctx, env, 0));
  /* `n == 0` IS handled here — it is §3's step 1, the canonical empty array — which is why the
     caller has no separate empty case to write. A negative `n` cannot arise (the caller passes an
     array's length) and takes the same answer rather than reaching `pv_new_array`. */
  if (n <= 0) return pv_empty_array();

  PVWord *slots = (PVWord *)malloc((size_t)n * sizeof(PVWord));
  if (slots == NULL) {
    fputs("purvasm vm foreign: out of memory blanking an array\n", stderr);
    abort();
  }
  /* `pv_unit()` is an immediate and allocates nothing, so the whole vector is built with no
     safepoint between filling it and handing it over. */
  for (int32_t i = 0; i < n; i++) slots[i] = pv_unit();
  PVWord array = pv_new_array(ctx, slots, (size_t)n);
  free(slots);
  return array;
}

/* `arrayLengthImpl :: ForeignValue -> Int` — pure: an array's length never changes, only its slots. */
PVWord PVF_EXPORT(arrayLengthImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  return pv_int((int32_t)pv_array_len(ctx, args[0]));
}

/* `readFieldImpl :: ForeignValue -> Int -> Effect ForeignValue` — effectful because a promoted array
 * is mutable: a leaf may have written the slot since the last read. */
PVWord PVF_EXPORT(readFieldImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  PVWord env = pv_new_array(ctx, args, 2);
  return pv_make_closure(ctx, (uint64_t)(uintptr_t)&pvm_read_thunk, 1, env);
}

static PVWord pvm_read_thunk(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)args;
  (void)nargs;
  PVWord env = pv_closure_env(ctx, clo);
  PVWord array = pv_read_field(ctx, env, 0);
  int32_t i = pv_int_payload(ctx, pv_read_field(ctx, env, 1));
  return pv_read_field(ctx, array, (uint64_t)i);
}

/* `writeFieldImpl :: ForeignValue -> Int -> ForeignValue -> Effect Unit` — the VM-side write to a
 * promoted array, which is the same write a leaf makes: one object, every alias. */
PVWord PVF_EXPORT(writeFieldImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  PVWord env = pv_new_array(ctx, args, 3);
  return pv_make_closure(ctx, (uint64_t)(uintptr_t)&pvm_write_thunk, 1, env);
}

static PVWord pvm_write_thunk(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)args;
  (void)nargs;
  PVWord env = pv_closure_env(ctx, clo);
  PVWord array = pv_read_field(ctx, env, 0);
  int32_t i = pv_int_payload(ctx, pv_read_field(ctx, env, 1));
  PVWord value = pv_read_field(ctx, env, 2);
  pv_write_field(ctx, array, (uint64_t)i, value);
  return pv_unit();
}
