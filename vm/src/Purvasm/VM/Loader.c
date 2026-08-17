/* The owned VM's dynamic-loader provider (ADR-0111 §6), built as the app-C sibling of `Loader.purs`
 * with -DPVF_MODULE=Purvasm_2eVM_2eLoader.
 *
 * This is the whole trusted surface of the foreign frontier: everything above it is ordinary purvasm
 * code. Three properties are why it is written here rather than exposed piecemeal:
 *
 *   - a **code address never becomes a purvasm value**. `dlsym`'s result exists only inside
 *     `resolveImpl`, which hands back a closure built by `pv_make_closure` — a real value — so no
 *     guest arithmetic can reach a function pointer.
 *   - a **module handle is a table index**, not a `void *`. The GC traces whatever the VM stores, and
 *     a raw `dlopen` handle in a purvasm field would be a non-value word masquerading as a tagged
 *     one. An index is an ordinary immediate. Nothing is ever unloaded, so an index never goes stale
 *     and needs no generation counter.
 *   - **strings that name a file or a symbol are never truncated**. A boundary that quietly shortens
 *     its input can open a *different* file or resolve a *different* leaf — and a wrong leaf is
 *     called at the wrong arity. Every such string is copied out at its full length, and a path
 *     containing an interior NUL (which `dlopen` would read as a terminator) is refused by name.
 *
 * Index 0 is `host-runtime`: the VM executable itself, which provides every `pvf_*` the runtime
 * staticlib defines (ADR-0111 §1.1/§4). It is opened with `dlopen(NULL)` by an *effectful* entry, so
 * a failure to establish it is reported where it happens rather than reappearing later as "that
 * symbol is absent".
 */
#include "purvasm.h"

#include <dlfcn.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PVM_MAX_MODULES 64
#define PVM_HOST_RUNTIME 0

typedef struct {
  void *handle;
  char *name; /* the path it was loaded from, or "host-runtime" for index 0 (ADR-0111 §6) */
} pvm_module;

static pvm_module pvm_modules[PVM_MAX_MODULES];
static int pvm_count = 0;
static char pvm_error[512] = "";

/* A state this boundary's own contract says cannot arise (a corrupt table, an arity the type system
 * already refused, an allocation failure). Failing loudly beats degrading to "absent", which would
 * be indistinguishable from a provider that simply does not define the key. */
static void pvm_fatal(const char *what) {
  fprintf(stderr, "purvasm vm loader: %s\n", what);
  abort();
}

static void pvm_set_error(const char *what) {
  const char *detail = dlerror();
  snprintf(pvm_error, sizeof pvm_error, "%s%s%s", what, detail ? ": " : "", detail ? detail : "");
}

/* Copy a purvasm `String` out whole, NUL-terminated, on the C heap. Returns NULL and sets
 * `pvm_error` when the string cannot be represented as a C string — never a shortened copy. The
 * two-call read is the ABI's: it hands out no interior pointer into the moving heap. */
static char *pvm_dup_str(PVContext *ctx, PVWord s, const char *what) {
  size_t len = pv_str_len(ctx, s);
  char *buf = (char *)malloc(len + 1);
  if (buf == NULL) pvm_fatal("out of memory copying a boundary string");
  pv_str_copy(ctx, s, (uint8_t *)buf, len);
  buf[len] = '\0';
  if (memchr(buf, '\0', len) != NULL) {
    snprintf(pvm_error, sizeof pvm_error, "%s contains an interior NUL byte", what);
    free(buf);
    return NULL;
  }
  return buf;
}

/* Open the host as provider zero. Returns 0 on success, -1 with `pvm_error` set. */
static int pvm_ensure_host(void) {
  if (pvm_count > 0) return 0;
  (void)dlerror();
  void *self = dlopen(NULL, RTLD_NOW);
  if (self == NULL) {
    pvm_set_error("host-runtime: dlopen(NULL) failed");
    return -1;
  }
  char *name = strdup("host-runtime");
  if (name == NULL) pvm_fatal("out of memory naming host-runtime");
  pvm_modules[PVM_HOST_RUNTIME].handle = self;
  pvm_modules[PVM_HOST_RUNTIME].name = name;
  pvm_count = 1;
  return 0;
}

/* RTLD_NOW is what makes the ABI-version reference a *load* failure, before the module's
 * initialisers run (ADR-0111 §5); RTLD_LOCAL keeps providers from seeing or interposing on each
 * other, so a per-handle `dlsym` is an honest question about one module (§4/§6). */
static int pvm_load(const char *path) {
  if (pvm_ensure_host() != 0) return -1;
  if (pvm_count >= PVM_MAX_MODULES) {
    snprintf(pvm_error, sizeof pvm_error, "too many loaded modules (max %d)", PVM_MAX_MODULES);
    return -1;
  }
  (void)dlerror(); /* clear any stale error before the call whose failure we report */
  void *handle = dlopen(path, RTLD_NOW | RTLD_LOCAL);
  if (handle == NULL) {
    pvm_set_error("load failed");
    return -1;
  }
  char *name = strdup(path);
  if (name == NULL) pvm_fatal("out of memory recording a module path");
  pvm_modules[pvm_count].handle = handle;
  pvm_modules[pvm_count].name = name;
  return pvm_count++;
}

/* `hostRuntimeImpl :: Effect Int` — establish provider zero and answer its index, or `-1`. A nullary
 * `Effect` leaf *is* the action, so this function is the thunk and its closure arity is 1. */
PVWord PVF_EXPORT(hostRuntimeImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)ctx;
  (void)clo;
  (void)args;
  (void)nargs;
  return pv_int(pvm_ensure_host() == 0 ? PVM_HOST_RUNTIME : -1);
}

static PVWord pvm_load_thunk(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs);

/* `loadImpl :: String -> Effect Int` — the outer `\path -> thunk`: an effectful leaf's saturation
 * builds the action, it does not perform it (ADR-0067 §3/§5). */
PVWord PVF_EXPORT(loadImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  PVWord env = pv_new_array(ctx, args, 1);
  return pv_make_closure(ctx, (uint64_t)(uintptr_t)&pvm_load_thunk, 1, env);
}

/* The `load` action: read the captured path, `dlopen` it, and answer the table index (negative on
 * failure — `Loader.purs` turns that into a named error, with `loadErrorImpl` supplying the text). */
static PVWord pvm_load_thunk(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)args;
  (void)nargs;
  PVWord env = pv_closure_env(ctx, clo);
  char *path = pvm_dup_str(ctx, pv_read_field(ctx, env, 0), "the provider path");
  if (path == NULL) return pv_int(-1);
  int index = pvm_load(path);
  free(path);
  return pv_int(index);
}

/* `loadErrorImpl :: Effect String` — the diagnostic for the most recent failure above. */
PVWord PVF_EXPORT(loadErrorImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)args;
  (void)nargs;
  return pv_new_str(ctx, (const uint8_t *)pvm_error, strlen(pvm_error));
}

/* `describeImpl :: Int -> String` — a provider's diagnostic name, which the exactly-one and
 * runtime-shadow errors name (ADR-0111 §4). */
PVWord PVF_EXPORT(describeImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  int index = pv_int_payload(ctx, args[0]);
  if (index < 0 || index >= pvm_count) return pv_new_str(ctx, (const uint8_t *)"<unloaded>", 10);
  const char *name = pvm_modules[index].name;
  return pv_new_str(ctx, (const uint8_t *)name, strlen(name));
}

/* `resolveImpl :: (leaf -> r) -> r -> Int -> String -> Int -> r` — `Just`/`Nothing` are passed in
 * rather than built here, so this file needs no knowledge of any data type's representation.
 *
 * Pure, deliberately: a handle can only be obtained from an effectful constructor (`hostRuntime` or
 * `load`), so by the time this runs the table exists and the only question left — does *this*
 * provider define *this* symbol — has an answer that does not depend on when it is asked.
 * `Nothing` therefore means exactly "this provider does not define it", never "something went wrong".
 */
PVWord PVF_EXPORT(resolveImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  int index = pv_int_payload(ctx, args[2]);
  int arity = pv_int_payload(ctx, args[4]);

  /* Both are guaranteed by `Loader.purs` — a `ModuleHandle` comes only from the loader, and an
   * `Arity` only from its checked constructor — so a violation here is not a bad query. */
  if (index < 0 || index >= pvm_count) pvm_fatal("resolve: module handle outside the loader table");
  if (arity < 0) pvm_fatal("resolve: negative arity");

  char *symbol = pvm_dup_str(ctx, args[3], "the symbol name");
  if (symbol == NULL) pvm_fatal("resolve: symbol name contains an interior NUL");

  /* A symbol's *value* may legitimately be NULL, so the failure test is `dlerror`, not the address
   * (POSIX): clear it first, then read it after the call. */
  (void)dlerror();
  void *address = dlsym(pvm_modules[index].handle, symbol);
  const char *failure = dlerror();
  free(symbol);
  if (failure != NULL) return args[1];

  /* `pv_make_closure` allocates, so `just` must be rooted across it and reloaded (ADR-0066 §3). */
  PVWord mark = pv_frame(ctx);
  PVWord just = pv_root(ctx, args[0]);
  PVWord leaf = pv_make_closure(ctx, (uint64_t)(uintptr_t)address, (uint32_t)arity, pv_unit());
  PVWord leaf_root = pv_root(ctx, leaf);
  PVWord applied = pv_get(ctx, leaf_root);
  PVWord result = pv_apply(ctx, pv_get(ctx, just), &applied, 1);
  pv_pop_frame(ctx, mark);
  return result;
}
