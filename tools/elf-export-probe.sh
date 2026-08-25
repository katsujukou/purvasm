#!/usr/bin/env bash
# The ELF half of ADR-0111 §1.1, measured in isolation — the claim its Correction note left owed:
# "`-Wl,-u,<sym>` alone pulls the archive member in *and* roots it against dead-strip ... Measured on
# Mach-O only; the same claim for ELF + `--gc-sections` is **owed a measurement**."
#
# Why a minimal probe rather than only the full `vm-loader-e2e.sh`: that harness answers "does the VM
# host a provider", which on a failure could be retention, export, the link order, or the loader. This
# one varies ONE thing — how the host's symbols are made visible — over a two-file program, so each
# candidate gets a verdict of its own. It runs in seconds and needs no purvasm build at all.
#
# The four link modes, and what each is:
#
#   none                          the platform default — the control. If this passes, the whole
#                                 retention/export apparatus is unnecessary on ELF and the ADR should
#                                 say so.
#   version-script                what `Purvasm.CLI.NativeLink` emits today for ELF.
#   export-dynamic+version-script what ADR-0111 §1.1's prose describes.
#   dynamic-list                  the candidate: one flag that both adds to `.dynsym` and confines it
#                                 to a list.
#
# Three questions per mode, because a mode that answers only the first two is not usable:
#
#   RETAINED  is `pv_probe_api` — defined in a static archive NOTHING references, pulled only by
#             `-Wl,-u`, under `--gc-sections` — present in the executable's ORDINARY symbol table?
#             This is the ADR's owed claim on its own terms, and it is a different question from the
#             next one: an executable can perfectly well contain a symbol while correctly keeping it
#             out of `.dynsym`. Reading only `.dynsym` would report retention as failing whenever
#             export does, and the two need separate answers to know which flag to reach for.
#   EXPORTED  is it in `.dynsym` — the table `dlopen` actually resolves against?
#   LEAKED    is `probe_private` — an ordinary host function no provider may reach — in `.dynsym`
#             too? `--export-dynamic` on its own exports everything, which is what the Mach-O
#             measurement rejected (635 symbols, including the VM's own loader leaves).
#   DLOPEN    does a shared object whose reference to `pv_probe_api` is left undefined load with
#             RTLD_NOW? This is the question the other three only predict.
#
# Usage: tools/elf-export-probe.sh     (from the repo root; ELF hosts only — exits 0 as a SKIP elsewhere)
set -uo pipefail

case "$(uname -s)" in
  Linux) ;;
  *)
    echo "elf-export-probe: SKIP — this measures ELF linker behaviour (host is $(uname -s));"
    echo "                  the Mach-O side is covered by tools/vm-loader-e2e.sh."
    exit 0
    ;;
esac

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
CC="${CC:-clang}"

cat >"$WORK/api.c" <<'EOF'
/* Stands in for the runtime staticlib's foreign-author API: defined in an archive the host never
   references, so only `-Wl,-u` can pull it in. */
int pv_probe_api(void) { return 42; }
EOF

cat >"$WORK/host.c" <<'EOF'
#include <dlfcn.h>
#include <stdio.h>

/* Stands in for everything a host must NOT expose — the VM's own loader leaves, generated globals,
   the Rust runtime's internals. */
int probe_private(int x) { return x + 1; }

int main(int argc, char **argv) {
  if (argc < 2) return 2;
  void *h = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
  if (h == NULL) {
    printf("dlopen-failed: %s\n", dlerror());
    return 1;
  }
  return probe_private(-1); /* keep it referenced, and return 0 */
}
EOF

cat >"$WORK/mod.c" <<'EOF'
/* A provider: it calls the host's API and defines nothing else of interest. Its reference to
   `pv_probe_api` is left undefined at link time and must bind at `dlopen`. */
int pv_probe_api(void);
int mod_entry(void) { return pv_probe_api(); }
EOF

cat >"$WORK/export.map" <<'EOF'
{ global:
  pv_probe_api;
local: *;
};
EOF

cat >"$WORK/dynamic.list" <<'EOF'
{ pv_probe_api; };
EOF

"$CC" -c -O2 -ffunction-sections -fdata-sections "$WORK/api.c" -o "$WORK/api.o" || exit 2
ar rcs "$WORK/libprobe.a" "$WORK/api.o" || exit 2
"$CC" -shared -fPIC -O2 "$WORK/mod.c" -o "$WORK/mod.so" || exit 2

rc=0
printf '%-30s %-9s %-9s %-7s %s\n' MODE RETAINED EXPORTED LEAKED DLOPEN

verdict () { # <mode> <want-retained> <want-exported> <want-leaked> <want-dlopen> <link flags…>
  local mode="$1" wantRetained="$2" wantExported="$3" wantLeaked="$4" wantDlopen="$5"; shift 5
  local exe="$WORK/host_$mode"
  if ! "$CC" -O2 -ffunction-sections -fdata-sections "$WORK/host.c" \
      -Wl,--gc-sections -Wl,-u,pv_probe_api "$@" "$WORK/libprobe.a" -ldl -o "$exe" 2>"$exe.log"; then
    printf '%-30s %s\n' "$mode" "LINK-FAILED (see log)"; sed 's/^/    /' "$exe.log"; return 1
  fi
  local symtab dynsym retained exported leaked dlopen
  # Two tables, two questions (see the header): the ordinary one answers "did `-u` pull the archive
  # member in and did `--gc-sections` keep it", the dynamic one answers "can `dlopen` see it".
  symtab="$(nm --defined-only "$exe" 2>/dev/null || llvm-nm --defined-only "$exe" 2>/dev/null)"
  dynsym="$(nm -D --defined-only "$exe" 2>/dev/null || llvm-nm --dynamic --defined-only "$exe" 2>/dev/null)"
  retained=no; exported=no; leaked=no; dlopen=fail
  printf '%s\n' "$symtab" | grep -qw pv_probe_api && retained=yes
  printf '%s\n' "$dynsym" | grep -qw pv_probe_api && exported=yes
  printf '%s\n' "$dynsym" | grep -qw probe_private && leaked=yes
  "$exe" "$WORK/mod.so" >"$exe.out" 2>&1 && dlopen=ok
  printf '%-30s %-9s %-9s %-7s %s\n' "$mode" "$retained" "$exported" "$leaked" "$dlopen"
  [ "$dlopen" = ok ] || sed 's/^/    /' "$exe.out"
  # The expectations are pre-committed so this reads as a measurement with a verdict, not a dump: a
  # surprise (a mode that starts or stops working) fails the harness instead of scrolling past.
  if [ "$retained" != "$wantRetained" ] || [ "$exported" != "$wantExported" ] \
    || [ "$leaked" != "$wantLeaked" ] || [ "$dlopen" != "$wantDlopen" ]; then
    printf '    UNEXPECTED: wanted retained=%s exported=%s leaked=%s dlopen=%s\n' \
      "$wantRetained" "$wantExported" "$wantLeaked" "$wantDlopen"
    return 1
  fi
}

# RETAINED is expected to be `yes` in EVERY row, `none` included — that row is the ADR's owed claim
# stated in isolation: `-Wl,-u` alone, with no export flag anywhere, pulling a member out of an
# archive nothing references and surviving `--gc-sections`.
#
# EXPORTED is expected to be `no` for `none` and `version-script`: an ELF executable does not
# populate `.dynsym` by default, and a version script filters what is exported rather than deciding
# that anything is. That gap is the difference from Mach-O (where `-exported_symbols_list` does both
# jobs) and the reason this measurement was owed.
verdict none                          yes no  no fail || rc=1
verdict version-script                yes no  no fail -Wl,--version-script,"$WORK/export.map" || rc=1
verdict export-dynamic+version-script yes yes no ok   -Wl,--export-dynamic -Wl,--version-script,"$WORK/export.map" || rc=1
verdict dynamic-list                  yes yes no ok   -Wl,--dynamic-list,"$WORK/dynamic.list" || rc=1

echo
if [ "$rc" -eq 0 ]; then
  echo "★ ELF: -Wl,-u retains the archive member under --gc-sections (RETAINED holds even with no"
  echo "  export flag at all), but a version script alone does NOT put it in .dynsym — the host needs"
  echo "  --export-dynamic beside it, or --dynamic-list instead."
else
  echo "✗ an ELF link mode behaved differently from the recorded expectation — see the table above" >&2
fi
exit "$rc"
