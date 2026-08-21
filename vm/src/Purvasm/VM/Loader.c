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

/* The symbol name of THIS host's foreign-ABI version (ADR-0111 §5), built from the header's own
 * paste so it cannot drift from what a provider references. */
#define PVM_STR_(x) #x
#define PVM_STR(x) PVM_STR_(x)
#define PVM_ABI_SYMBOL PVM_STR(PV_FOREIGN_ABI_SYM(PV_FOREIGN_ABI_VERSION))

/* Does this executable export its own version stamp? Asked of the host handle directly, because it
 * is a FACT about this binary rather than a reading of a message: if the answer is no, no provider
 * can load at all and every other explanation is noise. */
static int pvm_host_exports_abi_stamp(void) {
  if (pvm_ensure_host() != 0) return 0;
  (void)dlerror();
  (void)dlsym(pvm_modules[PVM_HOST_RUNTIME].handle, PVM_ABI_SYMBOL);
  return dlerror() == NULL;
}

/* Return the last occurrence: a provider path precedes the loader's reason and may itself contain
 * either marker. The actual undefined-symbol field is the later occurrence. */
static const char *pvm_last_strstr(const char *haystack, const char *needle) {
  const char *last = NULL;
  const char *at = haystack;
  while ((at = strstr(at, needle)) != NULL) {
    last = at;
    at++;
  }
  return last;
}

/* The symbol the loader named as undefined, taken from the platform's OWN field rather than from
 * anywhere in the message. The message also contains the provider's PATH, and a file named
 * `/tmp/pv_foreign_abi_v99-bad.so` would otherwise make an unrelated missing symbol read as a stale
 * ABI — a diagnosis that sends the reader to rebuild a provider that is not the problem. */
static const char *pvm_undefined_symbol(const char *detail) {
  const char *linux_at = pvm_last_strstr(detail, "undefined symbol: ");
  const char *darwin_at = pvm_last_strstr(detail, "flat namespace '");
  /* glibc/musl: "<path>: undefined symbol: <name>"; dyld: "... flat namespace '_<name>'" */
  if (linux_at != NULL && (darwin_at == NULL || linux_at > darwin_at))
    return linux_at + (sizeof "undefined symbol: " - 1);
  if (darwin_at != NULL) return darwin_at + (sizeof "flat namespace '" - 1);
  return NULL;
}

/* Read `symbol` as `pv_foreign_abi_v<N>` exactly — the canonical spelling and nothing adjacent to
 * it. `pv_foreign_abi_v1-bad` is a DIFFERENT symbol, not version 1, and `pv_foreign_abi_v01` is not
 * a version this project ever emits (the header refuses it). Returns 0 when it is not the stamp. */
static int pvm_abi_symbol_version(const char *symbol, long *version) {
  if (symbol == NULL) return 0;
  if (*symbol == '_') symbol++; /* Mach-O prefixes symbol names */
  if (strncmp(symbol, "pv_foreign_abi_v", sizeof "pv_foreign_abi_v" - 1) != 0) return 0;
  const char *digits = symbol + (sizeof "pv_foreign_abi_v" - 1);
  if (*digits < '1' || *digits > '9') return 0; /* canonical decimal: no sign, no leading zero */
  const char *end = digits;
  while (*end >= '0' && *end <= '9') end++;
  /* The name must END here; the delimiters are the ones the two message forms put after it. */
  if (*end != '\0' && *end != '\'' && *end != ' ' && *end != '\n' && *end != ',' && *end != ')') return 0;
  *version = strtol(digits, NULL, 10);
  return 1;
}

/* A failed provider `dlopen`, after the host's own stamp was verified. The message is consulted only
 * through the platform's undefined-symbol field. Deciding from the whole message was measured to
 * misfire because any provider path containing the stamp's name could forge the verdict.
 *
 * The path is *not* repeated here; `Loader.purs` names the module. */
static void pvm_set_load_error(void) {
  const char *detail = dlerror();
  if (detail == NULL) detail = "(no diagnostic)";

  long referenced = 0;
  if (pvm_abi_symbol_version(pvm_undefined_symbol(detail), &referenced) &&
      referenced != PV_FOREIGN_ABI_VERSION) {
    snprintf(pvm_error, sizeof pvm_error,
             "built against foreign ABI v%ld — this VM provides PV_FOREIGN_ABI_VERSION=%d, so "
             "rebuild the provider against this runtime's purvasm.h (loader: %s)",
             referenced, PV_FOREIGN_ABI_VERSION, detail);
    return;
  }

  snprintf(pvm_error, sizeof pvm_error, "load failed: %s", detail);
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
  /* Check this before `dlopen`: besides naming the primary host defect directly, this avoids keeping
   * a `dlerror()` pointer across another dynamic-loader call (which may overwrite its storage). */
  if (!pvm_host_exports_abi_stamp()) {
    snprintf(pvm_error, sizeof pvm_error,
             "this VM does not export " PVM_ABI_SYMBOL ", so no provider can resolve against it — it "
             "was linked without the host foreign API (ADR-0111 §1.1); relink the VM before provider "
             "compatibility can be assessed");
    return -1;
  }
  (void)dlerror(); /* clear any stale error before the call whose failure we report */
  void *handle = dlopen(path, RTLD_NOW | RTLD_LOCAL);
  if (handle == NULL) {
    pvm_set_load_error();
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

/* `declaresImpl :: Int -> String -> Boolean` — does THIS provider define this symbol?
 *
 * Separate from `resolveImpl` because the two questions differ in what they cost and in what they
 * mean. Resolution BUILDS a closure (`pv_make_closure` allocates), so asking every provider that way
 * to find out which one answers would allocate a closure per candidate and throw all but one away.
 * More importantly, the exactly-one check (ADR-0111 §4) and the manifest's eager check are asking
 * about *existence*, not about a value — and existence needs no arity, which is why a manifest can
 * name keys without carrying the arities only the image knows (ADR-0110 §4(a)).
 *
 * As in `resolveImpl`, failure is detected through `dlerror` rather than a NULL address, since a
 * symbol's value may legitimately be NULL. */
PVWord PVF_EXPORT(declaresImpl)(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  int index = pv_int_payload(ctx, args[0]);
  if (index < 0 || index >= pvm_count) pvm_fatal("declares: module handle outside the loader table");

  char *symbol = pvm_dup_str(ctx, args[1], "the symbol name");
  if (symbol == NULL) pvm_fatal("declares: symbol name contains an interior NUL");

  (void)dlerror();
  (void)dlsym(pvm_modules[index].handle, symbol);
  const char *failure = dlerror();
  free(symbol);
  return pv_bool(failure == NULL);
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
