/*
 * purvasm.h — the stable C-ABI a native `foreign` leaf is written against (ADR-0071 / ADR-0073 §2).
 *
 * A ulib-shipped native foreign (ADR-0073) is a `.c` file that `#include`s this header and exports one
 * `AbiCodeFn` per leaf under the mangled symbol `pvf_<mangle(key)>` (ADR-0073 §3). The build compiles it
 * with the same `clang` that lowers the program's `.ll`, then links it against the runtime staticlib that
 * ships beside this header. This header is the ONLY surface a foreign may use.
 *
 * ── Representation opacity (ADR-0069) ──────────────────────────────────────────────────────────────
 * A leaf works with **opaque `PVWord`s**. It must NEVER inspect a tag bit, decide pointer-vs-immediate,
 * or dereference a word: it only calls the `pv_*` functions below. Each `pv_*` constructor validates the
 * shape of the object it builds and self-roots its own arguments across its own allocation (ADR-0066 §3).
 * To read a scalar's payload without knowing the encoding, use the accessors (`pv_number_bits`, …).
 *
 * ── Rooting contract (ADR-0066 §3 / ADR-0073 §2) ───────────────────────────────────────────────────
 * The heap MOVES (a copying collector, ADR-0064). A `PVWord` a leaf holds stays valid until the *next*
 * allocating `pv_*` call; to keep a value live ACROSS such a call, root it:
 *
 *     PVWord mark = pv_frame(ctx);              // open a shadow-stack frame
 *     PVWord h    = pv_root(ctx, v);            // protect v; h is a stable handle
 *     PVWord w    = pv_new_str(ctx, ...);       // a safepoint — v may move
 *     v           = pv_get(ctx, h);             // reload v's current address
 *     pv_pop_frame(ctx, mark);                  // close the frame
 *
 * A single-allocation leaf (e.g. `showNumberImpl` building one `String`) needs no rooting; a leaf that
 * allocates more than once while holding a live value does. Rooting is the FFI author's responsibility;
 * representation, GC, and panic-containment stay the runtime's.
 *
 * NOT exposed here (deliberately): the runtime lifecycle (`pv_runtime_new`/`pv_runtime_free`), the entry
 * plumbing (`pv_run_effect`/`pv_drain_output`/`pv_print_int`/`pv_case_fail`), and the `pv_prim_*` primop
 * helpers — those are codegen's internal lowering ABI, emitted straight into the `.ll`, not a foreign's.
 */
#ifndef PURVASM_H
#define PURVASM_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/** The runtime context, passed to every `pv_*` call. Opaque: never allocate or dereference one. */
typedef struct PVContext PVContext;

/** A purvasm value — an opaque tagged word (immediate scalar OR moving heap pointer). Do not inspect. */
typedef uint64_t PVWord;

/**
 * A native leaf's code, exported as `pvf_<mangle(key)>` (ADR-0073 §3): `(ctx, closure, args, nargs)`.
 * `args` points at `nargs` argument words (or is unused when `nargs == 0`). `closure` is this leaf's own
 * closure value — when the leaf captures, read its env via `pv_closure_env`, then the individual
 * captures out of that env via `pv_read_field`.
 */
typedef PVWord (*PVCodeFn)(PVContext *ctx, PVWord closure, const PVWord *args, size_t nargs);

/**
 * `PVF_EXPORT(ident)` — name a native-leaf provider by its bare PureScript identifier, letting the build
 * supply the module (ADR-0091). For a project module `M`'s sibling `.c`, the build injects
 * `-DPVF_MODULE=<escapeIdent(M)>` (a valid C token, e.g. `Data.Show` → `Data_2eShow`); this macro pastes
 * the exported symbol `pvf_<PVF_MODULE>_2e<ident>` = `pvf_<escapeIdent("M.ident")>` (ADR-0073 §3 ABI).
 * A provider writes `PVWord PVF_EXPORT(foo)(PVContext *ctx, PVWord clo, const PVWord *args, size_t n)`.
 *
 * Correct only when `ident` is purely alphanumeric (the common case). An ident containing `_` or other
 * bytes mangles differently (`_localeCompare` → `_5flocaleCompare`); export such a leaf under its full
 * hand-written `pvf_<escapeIdent("M.ident")>` symbol. Needs `-DPVF_MODULE=…`, which the build injects
 * per file — the symbol audit (ADR-0091 §4) reports a mismatch by key, so a typo fails by name.
 */
#define PVF_CAT_(a, b) a##b
#define PVF_CAT(a, b) PVF_CAT_(a, b)
#define PVF_EXPORT(ident) PVF_CAT(PVF_CAT(pvf_, PVF_MODULE), PVF_CAT(_2e, ident))

/* ── Shadow-stack rooting (see the contract above) ─────────────────────────────────────────────────── */

/** Open a shadow-stack frame; returns a mark to pass to `pv_pop_frame`. */
PVWord pv_frame(PVContext *ctx);
/** Protect `v` for the current frame; returns a stable handle to reload it after a safepoint. */
PVWord pv_root(PVContext *ctx, PVWord v);
/** Reload the current (possibly moved) value behind a root `handle`. */
PVWord pv_get(PVContext *ctx, PVWord handle);
/** Close the frame opened by the matching `pv_frame`, discarding its roots. */
void pv_pop_frame(PVContext *ctx, PVWord mark);

/* ── Scalar accessors — read a payload without knowing the encoding (ADR-0073 §2) ──────────────────── */

/** The IEEE-754 bit pattern of a boxed `Number` (boxed → takes `ctx` to reach & shape-check the object). */
uint64_t pv_number_bits(PVContext *ctx, PVWord number);
/** An immediate `Int`'s payload. `ctx` is unused (immediate) but taken for accessor uniformity. */
int32_t pv_int_payload(PVContext *ctx, PVWord i);
/** An immediate `Boolean`'s payload as 0/1. `ctx` is unused (immediate) but taken for uniformity. */
int pv_bool_payload(PVContext *ctx, PVWord b);
/**
 * A `String`'s UTF-8 byte length. Pairs with `pv_str_copy` — the two-call copy-out shape deliberately
 * never hands the leaf an interior pointer into the moving heap.
 */
size_t pv_str_len(PVContext *ctx, PVWord s);
/**
 * Copy a `String`'s UTF-8 bytes into the caller's buffer of capacity `cap`; returns the count copied
 * (`min(len, cap)` — size the buffer with `pv_str_len`). The copied bytes are caller-owned and stay
 * valid regardless of later `pv_*` calls.
 */
size_t pv_str_copy(PVContext *ctx, PVWord s, uint8_t *dst, size_t cap);

/* ── Immediate constructors (no allocation, hence no ctx; the encoding stays the runtime's) ─────────── */

/** An immediate `Int` from a C `int32_t`. */
PVWord pv_int(int32_t v);
/** An immediate `Boolean` from a C truth value (0 = false, non-0 = true). */
PVWord pv_bool(int v);
/** The immediate `Unit` value. */
PVWord pv_unit(void);

/* ── Value constructors (each self-roots its arguments across its own allocation) ───────────────────── */

/** A `String` from `len` UTF-8 bytes at `bytes` (`len == 0` → the empty string; `bytes` may be NULL). */
PVWord pv_new_str(PVContext *ctx, const uint8_t *bytes, size_t len);
/** A boxed `Number` from an IEEE-754 bit pattern (pair with `pv_number_bits`). */
PVWord pv_new_number(PVContext *ctx, uint64_t bits);
/** An immutable `Array` from `n` value words at `elems` (`n >= 1`; use `pv_empty_array` for empty). */
PVWord pv_new_array(PVContext *ctx, const PVWord *elems, size_t n);
/** The canonical empty `Array` (no allocation). */
PVWord pv_empty_array(void);
/** An `Array`'s element count (the empty array reads as 0) — pairs with `pv_read_field` for the
    FFI's array conversions (ADR-0078). */
size_t pv_array_len(PVContext *ctx, PVWord array);
/** A field-carrying algebraic-data value: constructor `tag`, then `n` field words at `fields`.
    A NULLARY constructor is `pv_new_nullary_adt`, not this: the two representations differ.
    `n == 0` is LEGACY and non-canonical: it allocates a zero-field heap object, which
    carries the right tag and matches no native `case` at all. It is kept because refusing it would
    change what an existing v1 symbol does — a provider built before the nullary entry existed would
    pass the version check and then fault inside a call that used to return. */
PVWord pv_new_adt(PVContext *ctx, uint32_t tag, const PVWord *fields, size_t n);
/** The NULLARY constructor `tag` — an immediate, so no allocation and no ctx.
    This is the only representation a nullary constructor has, and the one a generated `case` matches:
    a `case` splits on representation before comparing tags, so a zero-field heap object would carry
    the right tag and miss every `case Nothing`. Build `Nothing` with this, never by hand. */
PVWord pv_new_nullary_adt(uint32_t tag);
/** An algebraic-data value's constructor tag — the number `pv_new_adt` was given, which is
    `fnv1a64(name).lo & 0x7fffffff` for the constructor NAME. Answers for a NULLARY constructor too:
    that one has no heap object (it is an immediate whose payload is the tag), and a caller holding an
    opaque word cannot tell the two apart, so the accessor does. A typed accessor, not introspection:
    it answers what tag THIS ADT carries, never what kind a word is.
    NOTE the shape check reaches only the pointer case: a heap non-ADT aborts, but a nullary
    constructor is indistinguishable from an `Int`/`Boolean`/`Unit` — all immediates — so the caller
    must be a site whose TYPE already established that this is an ADT. */
uint32_t pv_adt_tag(PVContext *ctx, PVWord adt);
/** A record from parallel `ids` (sorted FNV-1a-64 label ids) and `values`, length `n` (ADR-0069). */
PVWord pv_new_record(PVContext *ctx, const PVWord *ids, const PVWord *values, size_t n);
/** A mutable one-cell `Ref` initialised to `init`. */
PVWord pv_new_ref(PVContext *ctx, PVWord init);
/** A no-/some-capture closure over `code` (an `AbiCodeFn` address) of `arity`, capturing `env`. */
PVWord pv_make_closure(PVContext *ctx, uint64_t code, uint32_t arity, PVWord env);

/* ── Field / record access ─────────────────────────────────────────────────────────────────────────── */

/** Read record field for label id `id` (ADR-0069). */
PVWord pv_record_get(PVContext *ctx, PVWord record, PVWord id);
/** A record with field `id` set to `value` (immutable copy-on-update, ADR-0069). */
PVWord pv_record_set(PVContext *ctx, PVWord record, PVWord id, PVWord value);
/** A `Closure`'s captured `env` value — how an effect-thunk leaf reaches its captures without
    knowing the closure layout (the layout stays the runtime's). */
PVWord pv_closure_env(PVContext *ctx, PVWord closure);
/** Read value-slot `i` of a heap object (e.g. a capture out of a `pv_closure_env` env, ADR-0066). */
PVWord pv_read_field(PVContext *ctx, PVWord obj, uint64_t i);
/** Write value-slot `i` of a heap object. */
void pv_write_field(PVContext *ctx, PVWord obj, uint64_t i, PVWord v);
/* NB: raw (non-value) word reads are intentionally NOT exposed — reaching a scalar's payload word directly
   (e.g. a `Number`'s `f64` bits) would break representation-opacity (ADR-0069 / ADR-0073 §2). Read scalars
   through the typed accessors above (`pv_number_bits` / `pv_int_payload` / `pv_bool_payload`); those grow
   on demand as new scalar shapes need FFI reads. */

/* ── Application / forcing ──────────────────────────────────────────────────────────────────────────── */

/** Apply `f` to `nargs` argument words at `args` (curried; over-/under-application handled, ADR-0066). */
PVWord pv_apply(PVContext *ctx, PVWord f, const PVWord *args, size_t nargs);
/** Force a by-need cell to its value; passes any non-cell through unchanged (ADR-0070). */
PVWord pv_force_if_byneed(PVContext *ctx, PVWord v);

/* ── Foreign-ABI version (ADR-0111 §5) ─────────────────────────────────────────────────────────────── */

/**
 * The version of the foreign-author surface above — the `pv_*` functions, their signatures and their
 * contracts. Bumped when that surface changes incompatibly; an additive entry does not bump it.
 *
 * Deliberately NOT `PV_CTX_HEADER_VERSION`, which versions the generated-code `pv_ctx_header` layout
 * below and is explicitly not the foreign surface: the two change for different reasons and a shared
 * counter would make each one's bump a false alarm for the other.
 */
#define PV_FOREIGN_ABI_VERSION 1

#if defined(__GNUC__) || defined(__clang__)
#define PVF_USED __attribute__((used))
#else
#define PVF_USED
#endif

/*
 * The version travels as an undefined **reference**, not as a stamp to be read back after loading.
 * Every translation unit that includes this header carries a reference to `pv_foreign_abi_v<N>`, and
 * the runtime defines that symbol for its own N only — so a provider built against a different header
 * fails to *resolve*: at link when it is linked statically, and at `dlopen` when the VM loads it as a
 * shared object (ADR-0111 §6 loads with RTLD_NOW, which binds every reference before the module's
 * initialisers run).
 *
 * That ordering is the whole point. A version read *after* `dlopen` is too late: `dlopen` runs
 * `init_array` / `+load` / a Rust `ctor` before it returns, so a stale module would already have
 * called into a `pv_*` surface it disagrees with. Nothing here is ever called or read; the reference
 * alone carries the version, and it costs one word of unreferenced data per object.
 *
 * The author writes nothing for this and no build flag selects it — the reference is unconditional,
 * so a statically linked provider gets the same protection from the runtime staticlib's definition.
 */
#define PV_FOREIGN_ABI_SYM_(n) pv_foreign_abi_v##n
#define PV_FOREIGN_ABI_SYM(n) PV_FOREIGN_ABI_SYM_(n)

void PV_FOREIGN_ABI_SYM(PV_FOREIGN_ABI_VERSION)(void);

static void (*const pv_foreign_abi_stamp)(void) PVF_USED = PV_FOREIGN_ABI_SYM(PV_FOREIGN_ABI_VERSION);

/* ════════════════════════════════════════════════════════════════════════════════════════════════════
 * GENERATED-CODE ABI (ADR-0079) — NOT part of the foreign-author API above.
 *
 * A `PVContext*` points to storage whose FIRST BYTES are a `pv_ctx_header`. Exactly TWO consumers may
 * rely on that prefix: the compiler's generated code (release-mode inline rooting/trampoline fast
 * paths) and the runtime / `purvasm-sys` `#[repr(C)]` mirrors (each side carries compile-time layout
 * assertions). **A foreign provider must NOT read or write `pv_ctx_header`** — the supported foreign
 * surface is, and remains, the `pv_*` functions above (ADR-0073 §2). `PVContext` itself stays opaque:
 * this is a documented prefix guarantee, not a struct member to reach through.
 *
 * The layout is versioned CONTRACT: any change bumps `PV_CTX_HEADER_VERSION`, renames the
 * `pv_ctx_abi_v<N>` link-time stamp the runtime exports for its own version (a stale object then
 * fails to link), and is caught at run time by `pv_abi_check` as the final backstop.
 * ══════════════════════════════════════════════════════════════════════════════════════════════════ */

typedef struct pv_ctx_header {
  uint64_t *roots_base;   /* the shadow stack's storage (moves only on slow-path growth)      */
  uint64_t  roots_len;    /* one past the top root = the next handle = the frame mark         */
  uint64_t  roots_cap;    /* fast-path bound; roots_len == roots_cap -> slow-path pv_root     */
  uint64_t  pending_tail; /* 0 = no stashed generic tail (pv_settle fast path)                */
} pv_ctx_header;

#define PV_CTX_HEADER_VERSION 1

/** Run-time ABI-version backstop (ADR-0079 §1): the generated entry stub calls this once at startup;
    a mismatch aborts loudly. (The per-object net is the `pv_ctx_abi_v<N>` link-time symbol.) */
void pv_abi_check(uint32_t version);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* PURVASM_H */
