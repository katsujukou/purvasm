/* A user-supplied provider: everything `host-runtime` alone cannot exercise (ADR-0111 §4, §7
 * slices 3 and 4).
 *
 * The runtime's own leaves are a provider class, but a fixed one — it defines what it defines, and
 * several claims need a leaf that it does *not* define, or one whose signature no runtime leaf has:
 *
 *   - **resolution across both provider classes.** `describeBoolImpl` is a key the runtime does not
 *     define, so a program that runs it can only have resolved it in this module — which is the
 *     whole claim of "one authoring surface": the same `.c` a native build links statically is the
 *     one the VM loads.
 *   - **the `Boolean` boundary arm, in both directions.** No runtime leaf takes OR returns a
 *     `Boolean` (nothing in `runtime/src/leaf.rs` touches `pv_bool_payload`), so this file owns both
 *     halves: `describeBoolImpl` READS one crossing outward, and `isPositiveImpl` PRODUCES one for
 *     the Boolean-demanding elimination sites — `JumpUnless`, a `Guarded` condition, `SwitchLit`
 *     over `LBool`. Their absence is what let `Guarded` ship undecoded.
 *   - **arrays in both directions.** `writeArrayImpl`/`readArrayImpl` write and read an array the
 *     GUEST owns (the promotion path, §3), and `makeArrayImpl` RETURNS one — a carrier from birth,
 *     which never gets promoted and which `SetArray` must still reach. `lengthOfImpl` is the only
 *     leaf an EMPTY array can be handed, since every other one indexes a slot.
 *
 * Built with -DPVF_MODULE=Test_2eLoader, exactly as the app-C sibling of a module `Test.Loader`
 * would be (ADR-0091 §2).
 */
#include "purvasm.h"

/* `Test.Loader.describeBoolImpl :: Boolean -> String` — reads the boundary's `Boolean` and answers a
 * string that could not be produced by the wrong branch, so the gate distinguishes "a Boolean
 * crossed correctly" from "something crossed". */
PVWord PVF_EXPORT(describeBoolImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  const char *answer = pv_bool_payload(ctx, args[0]) ? "provider read Boolean true" : "provider read Boolean false";
  size_t len = 0;
  while (answer[len] != '\0') len++;
  return pv_new_str(ctx, (const uint8_t *)answer, len);
}

/* `Test.Loader.writeArrayImpl :: Array -> Int -> String -> Effect Unit` — a leaf that WRITES into an
 * array the guest owns (ADR-0111 §3's aliasing gate).
 *
 * This is what makes promotion observable rather than merely implemented: the array reaching here is
 * the runtime object the VM's cell was forwarded to, so a write lands on the object every VM alias
 * shares. An elementwise copy at the boundary would make this leaf write to a corpse — the guest
 * would see nothing, and the two VM bindings holding "the same" array would stop agreeing.
 *
 * Effectful, so it is the ADR-0067 pair: the outer leaf captures its three arguments into a thunk,
 * and the thunk performs the write when the effect runs.
 */
static PVWord pvm_write_array_thunk(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs);

PVWord PVF_EXPORT(writeArrayImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  PVWord env = pv_new_array(ctx, args, 3);
  return pv_make_closure(ctx, (uint64_t)(uintptr_t)&pvm_write_array_thunk, 1, env);
}

static PVWord pvm_write_array_thunk(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)args;
  (void)nargs;
  PVWord env = pv_closure_env(ctx, clo);
  PVWord array = pv_read_field(ctx, env, 0);
  int32_t i = pv_int_payload(ctx, pv_read_field(ctx, env, 1));
  PVWord value = pv_read_field(ctx, env, 2);
  pv_write_field(ctx, array, (uint64_t)i, value);
  return pv_unit();
}

/* `Test.Loader.readArrayImpl :: Array -> Int -> String` — read a slot back out, so the gate can also
 * check the OTHER direction: a write the *guest* made through its own `SetArray` must be visible to
 * a leaf, on the same object. */
PVWord PVF_EXPORT(readArrayImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  int32_t i = pv_int_payload(ctx, args[1]);
  return pv_read_field(ctx, args[0], (uint64_t)i);
}

/* `Test.Loader.lengthOfImpl :: Array -> Int` — the leaf an EMPTY array can be handed. Every other
 * array leaf here indexes a slot, and an empty array has none, so without this §3's step 1 (the
 * `pv_empty_array` case) could not be exercised at all: promotion only happens when something
 * actually crosses. */
PVWord PVF_EXPORT(lengthOfImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  return pv_int((int32_t)pv_array_len(ctx, args[0]));
}

/* `Test.Loader.makeArrayImpl :: String -> Array` — a leaf that RETURNS an array.
 *
 * This is the other entrance to the identity invariant (ADR-0111 §3, and the review that named it):
 * an array the guest built and handed over is a `VArray` whose cell gets promoted, but an array a
 * leaf *returned* is a carrier from birth — it never had a VM cell at all. `IndexArray`,
 * `LengthArray` and `SetArray` must reach it just the same, so the VM gives it a cell that forwards
 * to this very object rather than copying it into one.
 */
PVWord PVF_EXPORT(makeArrayImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  PVWord elems[2];
  elems[0] = args[0];
  elems[1] = args[0];
  return pv_new_array(ctx, elems, 2);
}

/* `Test.Loader.isPositiveImpl :: Int -> Boolean` — a leaf that RETURNS a Boolean.
 *
 * `describeBoolImpl` reads one across the boundary; this produces one, which is what the
 * Boolean-demanding *elimination* sites need (ADR-0111 §3): `JumpUnless`, a `Guarded` clause's
 * condition, and a `SwitchLit` over `LBool`. Without a leaf on this side of the arrow those sites
 * can only be exercised with VM-built Booleans, which is exactly the coverage gap that let the
 * `Guarded` site ship undecoded.
 */
PVWord PVF_EXPORT(isPositiveImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  return pv_bool(pv_int_payload(ctx, args[0]) > 0);
}

/* `Test.Loader.lookupImpl :: Int -> Maybe String` — a leaf that RETURNS a data value, in both of its
 * shapes (ADR-0111 §3, slice 5).
 *
 * This is the case `pv_adt_tag` exists for: before it, the VM could not dispatch on a constructor a
 * leaf produced, so a leaf could not return a `Maybe` — too common a signature to leave out. Both
 * arms matter and they are represented DIFFERENTLY:
 *
 *   - `Just x` is a heap ADT, built with `pv_new_adt`;
 *   - `Nothing` is nullary: `pv_new_nullary_adt`, its OWN ABI entry, because the representation is an
 *     immediate rather than a heap object (ADR-0064 §1). `pv_new_adt` with no fields is the legacy
 *     spelling and builds the wrong thing — a zero-field heap object no native `case` matches — so a
 *     provider uses neither that nor a hand-built immediate.
 *
 * A caller holding the result cannot tell those apart, which is why `pv_adt_tag` answers for both.
 * The tags are `fnv1a64(name).lo & 0x7fffffff` over "Data.Maybe.Just" / "Data.Maybe.Nothing" — the
 * fully qualified constructor names the bytecode carries — computed here the same way the VM and
 * codegen compute them.
 */
static uint32_t pvm_ctor_tag(const char *name) {
  /* FNV-1a-64 over the name's bytes, low 32 bits masked to 31 (ADR-0069 §2). 64-bit arithmetic is
     native here, so this is the whole derivation rather than the limb-wise version PureScript needs. */
  uint64_t h = 0xcbf29ce484222325ULL;
  for (const char *p = name; *p != '\0'; p++) {
    h ^= (uint64_t)(unsigned char)*p;
    h *= 0x100000001b3ULL;
  }
  return (uint32_t)(h & 0xffffffffULL) & 0x7fffffffU;
}

PVWord PVF_EXPORT(lookupImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  if (pv_int_payload(ctx, args[0]) > 0) {
    PVWord field = pv_new_str(ctx, (const uint8_t *)"found by the leaf", 17);
    return pv_new_adt(ctx, pvm_ctor_tag("Data.Maybe.Just"), &field, 1);
  }
  /* Nullary, through its OWN public constructor. `pv_new_adt` with no fields would compile and build
     the wrong thing (a zero-field heap object no native `case` matches), and `pv_int` would work here
     while hiding that the API had no way to express this — which is what an earlier draft of this
     fixture did, and what let the gap survive. */
  return pv_new_nullary_adt(pvm_ctor_tag("Data.Maybe.Nothing"));
}

/* `Test.Loader.describeMaybeImpl :: Maybe String -> String` — a leaf that RECEIVES a data value.
 *
 * The other direction of the same boundary: `toPv` builds this from the VM's `VData`, deriving the
 * tag from the constructor NAME the bytecode carries. Both shapes cross — `Just x` as a heap ADT and
 * `Nothing` as a bare immediate — and this leaf tells them apart the only way anything can, by tag.
 */
PVWord PVF_EXPORT(describeMaybeImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  /* THREE outcomes, not two. An `else` branch meaning "Nothing" would report a tag the VM computed
   * WRONGLY as a correct `Nothing`, so the outbound nullary path could break with the gate still
   * green — the leaf must be able to say "neither of the tags I know". */
  uint32_t tag = pv_adt_tag(ctx, args[0]);
  if (tag == pvm_ctor_tag("Data.Maybe.Just")) {
    return pv_new_str(ctx, (const uint8_t *)"leaf received Just", 18);
  }
  if (tag == pvm_ctor_tag("Data.Maybe.Nothing")) {
    return pv_new_str(ctx, (const uint8_t *)"leaf received Nothing", 21);
  }
  return pv_new_str(ctx, (const uint8_t *)"WRONG tag reached the leaf", 26);
}
