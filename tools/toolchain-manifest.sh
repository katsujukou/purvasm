#!/usr/bin/env bash
# Toolchain provenance for the measurement harnesses: WHAT was measured, and — where it can actually
# be proven — that it was built from the sources it claims to describe.
#
# The harnesses already snapshot their inputs rigorously: one copy, taken once, used by every leg.
# That is what makes their comparisons valid, and it is not enough on its own:
#
#   > snapshotting guarantees both legs measured the SAME artifact.
#   > It says nothing about whether that artifact was built from the CURRENT sources.
#
# Found 2026-08-26, after the owned-VM merge: `dist/ulib` was a staging six weeks older than
# `ulib/`, and the release runtime staticlib predated `runtime/src` likewise. Every measurement in
# that window was perfectly reproducible and described a different library. It did not move the
# headline figure; it moved `wrapper-entry` 15,430 -> 15,428. A hole that only sometimes changes the
# answer is still a hole, and its failure mode is the worst kind: green, repeatable, and wrong about
# what it is describing.
#
# TWO MECHANISMS, and the difference between them is the point of this file.
#
#   toolchain_prepare  — BUILDS runtime, ulib and the compiler/CLI/census output in ONE leg and
#                        records what it built. This is a PROOF: the artifacts exist because this
#                        leg made them, from the tree as it stands.
#   toolchain_check    — compares artifact mtimes against the last commit touching their sources.
#                        This is a DIAGNOSTIC, not a proof, and it is labelled as one everywhere it
#                        prints. It cannot see an uncommitted edit made after the build (the source
#                        has no commit newer than the artifact), and it cannot tell an artifact
#                        built on another branch from one built here. It catches the case that
#                        actually happened — an artifact left behind for weeks — and nothing more.
#
# A headline measurement uses `toolchain_prepare`. `toolchain_check` is for the cheap runs, and for
# telling someone early that the thing they are about to measure is old.
#
# Usage (sourced, not executed):
#   . tools/toolchain-manifest.sh
#   toolchain_prepare <workdir>          # build everything, then record the manifest (PROOF)
#   toolchain_add <label> <artifact> <source...>   # declare a derivation for the diagnostic
#   toolchain_check                      # advisory staleness report; non-zero when stale
#   toolchain_write <manifest-file>      # record commit, dirty state, and per-artifact digests

# --- platform helpers ---------------------------------------------------------------------------
# BSD and GNU disagree on every one of these, and the §5.2 performance box is Linux while
# development is macOS — so a gate that works on only one of them fails exactly where it is needed.
_tc_mtime() { # $1 = path -> epoch seconds
  stat -f %m "$1" 2>/dev/null || stat -c %Y "$1" 2>/dev/null
}

_tc_date() { # $1 = epoch -> YYYY-MM-DD
  date -r "$1" +%Y-%m-%d 2>/dev/null || date -d "@$1" +%Y-%m-%d 2>/dev/null
}

_tc_sha() { # stdin -> sha256 hex
  if command -v shasum >/dev/null 2>&1; then shasum -a 256 | cut -d' ' -f1
  else sha256sum | cut -d' ' -f1; fi
}

# label \t artifact \t snapshot_rel \t sources(space-separated)
_TOOLCHAIN_ROWS=""

# | Declare that `$2` is built from the sources at `$4..`, and that a harness copies it to `$3`
# | INSIDE its snapshot.
# |
# | `snapshot_rel` is explicit and not derived: the first version of this file used
# | `basename "$artifact"`, which verified the runtime staticlib at `<snap>/libpurvasm_rt.a` while
# | every harness copies it to `<snap>/rt/libpurvasm_rt.a` — so an UNCHANGED snapshot verified as
# | MISSING. A path convention that the verifier guesses is a path convention that will be wrong.
toolchain_add() { # $1=label  $2=artifact path  $3=path inside a snapshot  $4...=source paths
  local label="$1" artifact="$2" rel="$3"
  shift 3
  _TOOLCHAIN_ROWS="${_TOOLCHAIN_ROWS}${label}	${artifact}	${rel}	$*
"
}

_toolchain_src_epoch() { # $1... = source paths -> epoch of the last commit touching any of them
  git log -1 --format=%ct -- "$@" 2>/dev/null
}

# | An artifact's mtime. For a directory this is the NEWEST file inside it: a staged library keeps
# | its own directory mtime when a file within it is rewritten.
_toolchain_art_epoch() { # $1 = artifact path
  if [ -d "$1" ]; then
    find "$1" -type f -print0 2>/dev/null | xargs -0 -n 200 stat -f %m 2>/dev/null \
      || find "$1" -type f -print0 2>/dev/null | xargs -0 -n 200 stat -c %Y 2>/dev/null
  elif [ -e "$1" ]; then
    _tc_mtime "$1"
  fi | sort -rn | head -1
}

# | A content hash, so the manifest IDENTIFIES an artifact rather than merely dating it. This is the
# | part that carries across machines and across time; the dates do not.
_toolchain_hash() { # $1 = artifact path
  if [ -d "$1" ]; then
    ( cd "$1" 2>/dev/null && find . -type f | LC_ALL=C sort | while IFS= read -r f; do
        printf '%s ' "$f"; _tc_sha <"$f"
      done ) | _tc_sha
  elif [ -e "$1" ]; then
    _tc_sha <"$1"
  fi
}

# | A digest over the FULL source closure, tracked files and working-tree contents alike. This is
# | what makes "built from these sources" checkable: HEAD alone misses uncommitted edits, and
# | `git status` alone misses which content those edits carry.
_toolchain_source_digest() {
  {
    git rev-parse HEAD 2>/dev/null || echo NO-HEAD
    # every tracked file's path and content hash, in a stable order…
    git ls-files -z 2>/dev/null | LC_ALL=C sort -z | while IFS= read -r -d '' f; do
      [ -f "$f" ] || continue
      printf '%s ' "$f"; _tc_sha <"$f"
    done
    # …plus anything untracked that is not ignored, since a new file is a source change too
    git ls-files -z --others --exclude-standard 2>/dev/null | LC_ALL=C sort -z       | while IFS= read -r -d '' f; do
          [ -f "$f" ] || continue
          printf '%s ' "$f"; _tc_sha <"$f"
        done
  } | _tc_sha
}

# --- the PROOF: one preparation leg --------------------------------------------------------------
# Builds the four things every harness consumes, in dependency order, from the tree as it stands, and
# records the result.
#
# Sequential builds take minutes, and a tree can be edited while they run — in which case the runtime,
# the ulib and the CoreFn come from three different source states and the manifest would describe
# none of them. So the FULL source closure is digested before and after, and a mismatch FAILS: the
# guarantee is "these artifacts were built from one tree state", and it is checked rather than hoped.
toolchain_prepare() { # $1 = work dir for logs and the manifest
  local work="${1:?toolchain_prepare needs a work dir}" d0 d1
  mkdir -p "$work"
  echo "== toolchain: preparing (runtime, ulib, corefn) — this is the PROOF leg ============"
  d0=$(_toolchain_source_digest)

  echo "   [1/3] cargo build --release (runtime staticlib)"
  ( cd runtime && cargo build --release ) >"$work/prepare-runtime.log" 2>&1 \
    || { echo "toolchain: runtime build FAILED; see $work/prepare-runtime.log" >&2; return 1; }

  echo "   [2/3] ulib staging (ulib-tools prepare-release)"
  sh ulib-tools/prepare-release.sh >"$work/prepare-ulib.log" 2>&1 \
    || { echo "toolchain: ulib staging FAILED; see $work/prepare-ulib.log" >&2; return 1; }

  echo "   [3/3] spago build (compiler, CLI, census -> output/)"
  npx spago build >"$work/prepare-corefn.log" 2>&1 \
    || { echo "toolchain: spago build FAILED; see $work/prepare-corefn.log" >&2; return 1; }

  d1=$(_toolchain_source_digest)
  if [ "$d0" != "$d1" ]; then
    echo "toolchain: the SOURCE TREE CHANGED during preparation." >&2
    echo "  The artifacts were built from more than one tree state, so the manifest would describe" >&2
    echo "  none of them. Re-run on a quiescent tree." >&2
    return 1
  fi

  _TOOLCHAIN_ROWS=""
  toolchain_declare_defaults
  TOOLCHAIN_PREPARED=1 TOOLCHAIN_SOURCE_DIGEST="$d1" toolchain_write "$work/toolchain-manifest.tsv"
  echo "   prepared; manifest at $work/toolchain-manifest.tsv"
}

# --- the DIAGNOSTIC ------------------------------------------------------------------------------
# Non-zero when an artifact is older than the last commit touching its sources. Advisory: see the
# header for what it cannot see. Callers decide whether that is fatal.
toolchain_check() {
  local bad=0 label artifact sources art_e src_e
  echo "toolchain: staleness DIAGNOSTIC (mtime vs last source commit — not a derivation proof)"
  printf '%-16s %-10s %-12s %-12s %s\n' "artifact" "state" "built" "sources" "path"
  while IFS=$'\t' read -r label artifact rel sources; do
    [ -n "$label" ] || continue
    art_e=$(_toolchain_art_epoch "$artifact")
    # shellcheck disable=SC2086
    src_e=$(_toolchain_src_epoch $sources)
    if [ -z "$art_e" ]; then
      printf '%-16s %-10s %-12s %-12s %s\n' "$label" "MISSING" "-" "-" "$artifact"; bad=1; continue
    fi
    if [ -z "$src_e" ]; then
      # a check that cannot be made must not report success
      printf '%-16s %-10s %-12s %-12s %s\n' "$label" "UNKNOWN" "$(_tc_date "$art_e")" "-" "$artifact"
      bad=1; continue
    fi
    if [ "$art_e" -lt "$src_e" ]; then
      printf '%-16s %-10s %-12s %-12s %s\n' "$label" "STALE" \
        "$(_tc_date "$art_e")" "$(_tc_date "$src_e")" "$artifact"
      bad=1
    else
      printf '%-16s %-10s %-12s %-12s %s\n' "$label" "ok" \
        "$(_tc_date "$art_e")" "$(_tc_date "$src_e")" "$artifact"
    fi
  done <<EOF
$_TOOLCHAIN_ROWS
EOF
  if [ -n "$(git status --porcelain 2>/dev/null)" ]; then
    echo "toolchain: the tree is DIRTY — an uncommitted edit is invisible to this diagnostic," >&2
    echo "  which compares against COMMIT times. Use toolchain_prepare for a headline run." >&2
  fi
  return "$bad"
}

# | Record the manifest. Digests are the part that identifies an artifact across machines and time.
toolchain_write() { # $1 = manifest file
  local label artifact sources art_e src_e
  {
    echo "# toolchain manifest"
    echo "commit	$(git rev-parse HEAD 2>/dev/null || echo UNKNOWN)"
    echo "dirty	$(test -n "$(git status --porcelain 2>/dev/null)" && echo yes || echo no)"
    echo "prepared	${TOOLCHAIN_PREPARED:-0}"
    # The tree state the artifacts were built from, recorded even when dirty. This IDENTIFIES that
    # state; it does not REPRODUCE it — the digest says two runs did or did not see the same
    # sources, and nothing more. Reproducing a dirty tree needs the patch or a source snapshot,
    # which this file does not keep.
    echo "source_digest	${TOOLCHAIN_SOURCE_DIGEST:-}"
    echo "allow_stale	${TOOLCHAIN_ALLOW_STALE:-0}"
    echo "taken	$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "#label	artifact	snapshot_rel	sha256	built	sources_last_commit"
    while IFS=$'\t' read -r label artifact rel sources; do
      [ -n "$label" ] || continue
      art_e=$(_toolchain_art_epoch "$artifact")
      # shellcheck disable=SC2086
      src_e=$(_toolchain_src_epoch $sources)
      printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$label" "$artifact" "$rel" \
        "$(_toolchain_hash "$artifact")" \
        "${art_e:+$(_tc_date "$art_e")}" "${src_e:+$(_tc_date "$src_e")}"
    done <<EOF
$_TOOLCHAIN_ROWS
EOF
  } >"$1"
}

# | Verify a snapshot against the manifest taken when it was built. This is what a `--toolchain`
# | run checks: the snapshot is HISTORICAL, so comparing it against today's source dates is
# | meaningless — what must hold is that the artifacts still hash to what the manifest recorded.
# | The labels a PREPARED manifest must carry, stated HERE — in the verifier — and not read out of
# | the manifest being checked. A verifier that derives its expectations from its input cannot catch
# | a truncated input: deleting four of five rows left the fifth matching, and the check passed.
TOOLCHAIN_REQUIRED_LABELS="ulib runtime corefn cli-wrapper census-wrapper"

# | …and WHERE each one must live inside a snapshot. The label set alone is not enough: with the
# | five labels intact, pointing `cli-wrapper` at any other file and recording THAT file's correct
# | hash verified green. The pairing is the invariant, so the pairing is what is pinned.
_toolchain_expected_rel() { # $1 = label -> the only snapshot_rel that label may carry
  case "$1" in
    ulib) echo ulib ;;
    runtime) echo rt/libpurvasm_rt.a ;;
    corefn) echo output ;;
    cli-wrapper) echo cli/index.node.js ;;
    census-wrapper) echo census/index.js ;;
    *) echo "" ;;
  esac
}

# | `$3` = `require-prepared` to demand the manifest came from a `toolchain_prepare` leg AND that it
# | is complete. A headline or pinned run passes it; anything else is explicitly choosing a weaker
# | guarantee.
toolchain_verify_snapshot() { # $1 = manifest file  $2 = snapshot root  [$3 = require-prepared]
  local m="$1" root="$2" mode="${3:-}" bad=0 label artifact rel sha rest actual n=0
  [ -f "$m" ] || { echo "toolchain: no manifest at $m — a pinned snapshot must carry one" >&2; return 1; }
  if [ "$mode" = require-prepared ] && ! grep -qx 'prepared	1' "$m"; then
    echo "toolchain: $m is a DIAGNOSTIC-only manifest (prepared != 1)." >&2
    echo "  A headline or pinned run needs one produced by toolchain_prepare." >&2
    return 1
  fi
  if [ "$mode" = require-prepared ]; then
    # Each metadata key EXACTLY once. Without this, appending `prepared\t1` to a diagnostic
    # manifest produces a file with two `prepared` rows, one of which says what the checker wants
    # to hear — and `grep -qx` is happy with either.
    local key n_meta
    for key in commit dirty prepared source_digest; do
      n_meta=$(grep -c "^$key	" "$m")
      if [ "$n_meta" != 1 ]; then
        echo "toolchain: $m has $n_meta '$key' rows; exactly one is required" >&2
        return 1
      fi
    done
    # a dirty PREPARED run is identified by its source digest; an absent or malformed one means the
    # manifest cannot say which tree state it describes, which is the thing being required. The
    # match is EXACT — a prefix test accepted `aaNOT-HEX`.
    local sd
    sd=$(grep '^source_digest	' "$m" | cut -f2)
    if ! [[ "$sd" =~ ^[0-9a-f]{64}$ ]]; then
      echo "toolchain: $m has no usable source_digest ('$sd') — it cannot say which tree it describes" >&2
      return 1
    fi
  fi
  echo "toolchain: verifying snapshot against its build-time manifest ($m)"
  local seen="" dup_rel="" unknown="" want_rel=""
  while IFS=$'\t' read -r label artifact rel sha rest; do
    case "$label" in '#'*|commit|dirty|prepared|allow_stale|taken|source_digest|'') continue ;; esac
    n=$((n + 1))
    # unknown labels are refused rather than ignored: a row this verifier does not understand is a
    # row it cannot check, and silently skipping it is how a manifest grows an unverified artifact.
    case " $TOOLCHAIN_REQUIRED_LABELS " in
      *" $label "*)
        # the label must live where that label lives, not merely somewhere that hashes correctly
        want_rel=$(_toolchain_expected_rel "$label")
        if [ "$rel" != "$want_rel" ]; then
          echo "toolchain: $label is recorded at '$rel'; it must be '$want_rel'" >&2
          bad=1
        fi
        ;;
      *) unknown="$unknown $label" ;;
    esac
    case " $seen " in
      *" $label "*) echo "toolchain: duplicate row for $label" >&2; bad=1 ;;
      *) seen="$seen $label" ;;
    esac
    case " $dup_rel " in
      *" $rel "*) echo "toolchain: two artifacts share snapshot_rel $rel" >&2; bad=1 ;;
      *) dup_rel="$dup_rel $rel" ;;
    esac
    actual=$(_toolchain_hash "$root/$rel")
    if [ -z "$actual" ]; then
      printf '  %-16s %s\n' "$label" "MISSING in snapshot at $rel"; bad=1
    elif [ "$actual" != "$sha" ]; then
      printf '  %-16s %s\n' "$label" "HASH CHANGED since it was recorded"; bad=1
    else
      printf '  %-16s %s\n' "$label" "ok ($rel)"
    fi
  done <"$m"
  # A manifest with no artifact rows verifies vacuously, which is the shape a truncated or
  # half-written file takes. Requiring a positive count makes "nothing to check" a failure.
  if [ "$n" -eq 0 ]; then
    echo "toolchain: $m records no artifacts — refusing to verify vacuously" >&2
    bad=1
  fi
  if [ -n "$unknown" ]; then
    echo "toolchain: unrecognised artifact row(s):$unknown" >&2; bad=1
  fi
  if [ "$mode" = require-prepared ]; then
    local want
    for want in $TOOLCHAIN_REQUIRED_LABELS; do
      case " $seen " in
        *" $want "*) : ;;
        *) echo "toolchain: $m is INCOMPLETE — no row for $want" >&2; bad=1 ;;
      esac
    done
  fi
  return "$bad"
}

# | The derivations every measurement harness depends on. ONE place, so a harness cannot check a
# | subset by accident — and each closure is deliberately wide: a narrow one reports `ok` for a
# | stale artifact whose real input changed elsewhere, which is the failure this file exists for.
toolchain_declare_defaults() {
  toolchain_add ulib "${PURVASM_LIB:-dist/ulib}" ulib \
    ulib ulib-tools packages base spago.yaml spago.lock
  toolchain_add runtime "${PURVASM_RT_A:-runtime/target/release/libpurvasm_rt.a}" rt/libpurvasm_rt.a \
    runtime/src runtime/include runtime/Cargo.toml runtime/Cargo.lock
  toolchain_add corefn output output \
    compiler cli census abi vm base spago.yaml spago.lock
  # The WRAPPERS are inputs too: both resolve their compiled modules relative to themselves, so a
  # snapshot whose wrapper changed is measuring a different classifier with the same CoreFn.
  toolchain_add cli-wrapper cli/index.node.js cli/index.node.js cli
  toolchain_add census-wrapper census/index.js census/index.js census
}

# | Record what a PINNED run consumed. A `--toolchain` run builds nothing and so has no manifest of
# | its own; this is how its report stays traceable to the snapshot that produced it.
# |
# | It exists as a function because the first version of this record was inlined in the harness at a
# | point where the path it read was still unset, so the file was silently never written. A shared
# | function can be exercised directly, which is what the self-test below does.
toolchain_record_input() { # $1 = toolchain root  $2 = destination file
  local root="$1" out="$2" m="$1/toolchain-manifest.tsv"
  {
    echo "# pinned toolchain consumed by this run"
    echo "toolchain_root	$root"
    echo "input_manifest	$m"
    if [ -f "$m" ]; then
      echo "input_manifest_sha256	$(_tc_sha <"$m")"
    else
      # reached only under the override; recorded as MISSING rather than omitted, so the absence is
      # a fact in the file instead of a blank the reader has to interpret.
      echo "input_manifest_sha256	MISSING"
    fi
    echo "allow_unprepared	${TOOLCHAIN_ALLOW_UNPREPARED:-0}"
    echo "taken	$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  } >"$out"
}

# --- self-test ----------------------------------------------------------------------------------
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  st=0
  t=$(mktemp -d); trap 'rm -rf "$t"' EXIT
  here="$PWD"
  mkdir -p "$t/src" "$t/art"
  ( cd "$t" && git init -q && git config user.email a@b && git config user.name a \
    && echo one >src/f.txt && git add -A && git commit -qm one )

  check() { # $1=label $2=expected pass|fail
    if ( cd "$t" && toolchain_check ) >/dev/null 2>&1; then got=pass; else got=fail; fi
    if [ "$got" = "$2" ]; then printf '  ok    %-52s (%s)\n' "$1" "$got"
    else printf '  FAIL  %-52s (expected %s, got %s)\n' "$1" "$2" "$got"; st=1; fi
  }

  cd "$t" || exit 1
  sleep 1; echo built >art/lib.a
  _TOOLCHAIN_ROWS=""; toolchain_add lib art/lib.a lib.a src
  check "an artifact newer than its sources" pass

  sleep 1; echo two >src/f.txt; git add -A; git commit -qm two
  _TOOLCHAIN_ROWS=""; toolchain_add lib art/lib.a lib.a src
  check "an artifact older than its sources" fail

  _TOOLCHAIN_ROWS=""; toolchain_add lib art/absent.a lib.a src
  check "a missing artifact" fail

  mkdir -p untracked; echo x >untracked/f
  _TOOLCHAIN_ROWS=""; toolchain_add lib art/lib.a lib.a untracked
  check "an unknown source date is not a pass" fail

  # THE LIMIT, asserted rather than hoped for: an uncommitted edit made AFTER the build is invisible
  # to this diagnostic. It is recorded here so nobody reads a green `toolchain_check` as a proof —
  # `toolchain_prepare` is the mechanism that answers this case.
  sleep 1; echo built-again >art/lib.a; sleep 1; echo edited-after >src/f.txt
  _TOOLCHAIN_ROWS=""; toolchain_add lib art/lib.a lib.a src
  check "KNOWN LIMIT: an uncommitted post-build edit passes" pass
  if [ -n "$(git status --porcelain)" ]; then
    printf '  ok    %-52s (%s)\n' "…and the dirty tree is reported" "warned"
  else printf '  FAIL  %-52s\n' "…and the dirty tree is reported"; st=1; fi

  # the manifest identifies by CONTENT
  git add -A; git commit -qm three
  _TOOLCHAIN_ROWS=""; toolchain_add lib art/lib.a lib.a src
  toolchain_write "$t/m1.tsv"; sleep 1; echo changed >art/lib.a; toolchain_write "$t/m2.tsv"
  if [ "$(grep '^lib	' "$t/m1.tsv" | cut -f4)" != "$(grep '^lib	' "$t/m2.tsv" | cut -f4)" ]; then
    printf '  ok    %-52s (%s)\n' "the manifest hash follows content" "differs"
  else printf '  FAIL  %-52s\n' "the manifest hash follows content"; st=1; fi

  # a prepared run says so, so a diagnostic-only run cannot be misread as a proven one
  TOOLCHAIN_PREPARED=1 toolchain_write "$t/m3.tsv"
  grep -q '^prepared	1$' "$t/m3.tsv" \
    && printf '  ok    %-52s (%s)\n' "a prepared run is recorded as prepared" "present" \
    || { printf '  FAIL  %-52s\n' "a prepared run is recorded as prepared"; st=1; }

  # snapshot verification compares against the BUILD-TIME manifest, not today's source dates
  # A NESTED snapshot path — the shape every harness actually uses for the runtime staticlib
  # (`<snap>/rt/libpurvasm_rt.a`). The first version of this file derived the location with
  # `basename`, so an UNCHANGED snapshot verified as MISSING; this row is that bug.
  mkdir -p "$t/snap/rt"; cp "$t/art/lib.a" "$t/snap/rt/libpurvasm_rt.a"
  _TOOLCHAIN_ROWS=""; toolchain_add runtime "$t/art/lib.a" rt/libpurvasm_rt.a src; toolchain_write "$t/m4.tsv"
  toolchain_verify_snapshot "$t/m4.tsv" "$t/snap" >/dev/null 2>&1 \
    && printf '  ok    %-52s (%s)\n' "an unchanged NESTED snapshot verifies" "pass" \
    || { printf '  FAIL  %-52s\n' "an unchanged NESTED snapshot verifies"; st=1; }
  echo tampered >"$t/snap/rt/libpurvasm_rt.a"
  toolchain_verify_snapshot "$t/m4.tsv" "$t/snap" >/dev/null 2>&1 \
    && { printf '  FAIL  %-52s\n' "a tampered snapshot is refused"; st=1; } \
    || printf '  ok    %-52s (%s)\n' "a tampered snapshot is refused" "fail"
  cp "$t/art/lib.a" "$t/snap/rt/libpurvasm_rt.a"

  # a DIAGNOSTIC-only manifest must not be accepted where a prepared one is required
  toolchain_verify_snapshot "$t/m4.tsv" "$t/snap" require-prepared >/dev/null 2>&1 \
    && { printf '  FAIL  %-52s\n' "an unprepared manifest is refused when required"; st=1; } \
    || printf '  ok    %-52s (%s)\n' "an unprepared manifest is refused when required" "fail"

  # --- a COMPLETE prepared manifest, built the way toolchain_prepare builds one -------------------
  # The verifier's schema is its own, so these rows exercise the real label set rather than a
  # stand-in. The earlier version of this suite pinned a ONE-ROW prepared manifest as a success,
  # which is exactly the truncation the schema now refuses.
  mkdir -p "$t/full/rt" "$t/full/output" "$t/full/ulib" "$t/full/cli" "$t/full/census"
  echo r >"$t/full/rt/libpurvasm_rt.a"; echo o >"$t/full/output/x"; echo u >"$t/full/ulib/x"
  echo c >"$t/full/cli/index.node.js"; echo s >"$t/full/census/index.js"
  _TOOLCHAIN_ROWS=""
  toolchain_add ulib "$t/full/ulib" ulib src
  toolchain_add runtime "$t/full/rt/libpurvasm_rt.a" rt/libpurvasm_rt.a src
  toolchain_add corefn "$t/full/output" output src
  toolchain_add cli-wrapper "$t/full/cli/index.node.js" cli/index.node.js src
  toolchain_add census-wrapper "$t/full/census/index.js" census/index.js src
  TOOLCHAIN_PREPARED=1 TOOLCHAIN_SOURCE_DIGEST=$(_toolchain_source_digest) toolchain_write "$t/m5.tsv"
  toolchain_verify_snapshot "$t/m5.tsv" "$t/full" require-prepared >/dev/null 2>&1 \
    && printf '  ok    %-52s (%s)\n' "a COMPLETE prepared manifest satisfies it" "pass" \
    || { printf '  FAIL  %-52s\n' "a COMPLETE prepared manifest satisfies it"; st=1; }

  # THE TRUNCATION: drop four of the five rows. The survivor still hashes correctly, and that used
  # to be enough.
  grep -v '^ulib	\|^corefn	\|^cli-wrapper	\|^census-wrapper	' "$t/m5.tsv" >"$t/m7.tsv"
  toolchain_verify_snapshot "$t/m7.tsv" "$t/full" require-prepared >/dev/null 2>&1 \
    && { printf '  FAIL  %-52s\n' "a truncated prepared manifest is refused"; st=1; } \
    || printf '  ok    %-52s (%s)\n' "a truncated prepared manifest is refused" "fail"

  # an empty source_digest cannot say which tree it describes
  sed 's/^source_digest	.*/source_digest	/' "$t/m5.tsv" >"$t/m8.tsv"
  toolchain_verify_snapshot "$t/m8.tsv" "$t/full" require-prepared >/dev/null 2>&1 \
    && { printf '  FAIL  %-52s\n' "an empty source_digest is refused"; st=1; } \
    || printf '  ok    %-52s (%s)\n' "an empty source_digest is refused" "fail"

  # an unknown artifact row is refused, not skipped
  cp "$t/m5.tsv" "$t/m9.tsv"; printf 'mystery\tx\tulib\tdeadbeef\t-\t-\n' >>"$t/m9.tsv"
  toolchain_verify_snapshot "$t/m9.tsv" "$t/full" require-prepared >/dev/null 2>&1 \
    && { printf '  FAIL  %-52s\n' "an unknown artifact row is refused"; st=1; } \
    || printf '  ok    %-52s (%s)\n' "an unknown artifact row is refused" "fail"

  # THE REPOINT: keep all five labels, aim one at a DIFFERENT file, and record that file's correct
  # hash. Every hash matches and the label set is complete — which is why the label→path pairing has
  # to be pinned independently rather than read out of the manifest.
  mkdir -p "$t/full/decoy"; echo decoy >"$t/full/decoy/other.js"
  _TOOLCHAIN_ROWS=""
  toolchain_add ulib "$t/full/ulib" ulib src
  toolchain_add runtime "$t/full/rt/libpurvasm_rt.a" rt/libpurvasm_rt.a src
  toolchain_add corefn "$t/full/output" output src
  toolchain_add cli-wrapper "$t/full/cli/index.node.js" cli/index.node.js src
  toolchain_add census-wrapper "$t/full/decoy/other.js" decoy/other.js src
  TOOLCHAIN_PREPARED=1 TOOLCHAIN_SOURCE_DIGEST=$(_toolchain_source_digest) toolchain_write "$t/m11.tsv"
  toolchain_verify_snapshot "$t/m11.tsv" "$t/full" require-prepared >/dev/null 2>&1 \
    && { printf '  FAIL  %-52s\n' "a label repointed to another file is refused"; st=1; } \
    || printf '  ok    %-52s (%s)\n' "a label repointed to another file is refused" "fail"

  # a malformed digest that merely STARTS hex
  sed 's/^source_digest	.*/source_digest	aaNOT-HEX/' "$t/m5.tsv" >"$t/m12.tsv"
  toolchain_verify_snapshot "$t/m12.tsv" "$t/full" require-prepared >/dev/null 2>&1 \
    && { printf '  FAIL  %-52s\n' "a part-hex source_digest is refused"; st=1; } \
    || printf '  ok    %-52s (%s)\n' "a part-hex source_digest is refused" "fail"

  # …and one of the right shape but the wrong length
  sed 's/^source_digest	.*/source_digest	abc123/' "$t/m5.tsv" >"$t/m13.tsv"
  toolchain_verify_snapshot "$t/m13.tsv" "$t/full" require-prepared >/dev/null 2>&1 \
    && { printf '  FAIL  %-52s\n' "a short source_digest is refused"; st=1; } \
    || printf '  ok    %-52s (%s)\n' "a short source_digest is refused" "fail"

  # appending `prepared 1` to a DIAGNOSTIC manifest must not launder it
  cp "$t/m4.tsv" "$t/m14.tsv"; printf 'prepared\t1\n' >>"$t/m14.tsv"
  toolchain_verify_snapshot "$t/m14.tsv" "$t/full" require-prepared >/dev/null 2>&1 \
    && { printf '  FAIL  %-52s\n' "a second prepared row does not launder it"; st=1; } \
    || printf '  ok    %-52s (%s)\n' "a second prepared row does not launder it" "fail"

  # two rows sharing a snapshot_rel means one artifact was never really checked
  sed 's|^corefn	\(.*\)	output	|corefn	\1	ulib	|' "$t/m5.tsv" >"$t/m10.tsv"
  toolchain_verify_snapshot "$t/m10.tsv" "$t/full" require-prepared >/dev/null 2>&1 \
    && { printf '  FAIL  %-52s\n' "a duplicated snapshot_rel is refused"; st=1; } \
    || printf '  ok    %-52s (%s)\n' "a duplicated snapshot_rel is refused" "fail"

  # a manifest with no artifact rows must not verify vacuously
  grep -v '	rt/libpurvasm_rt.a	\|	ulib	\|	output	\|	cli/index.node.js	\|	census/index.js	' "$t/m5.tsv" >"$t/m6.tsv"
  toolchain_verify_snapshot "$t/m6.tsv" "$t/full" >/dev/null 2>&1 \
    && { printf '  FAIL  %-52s\n' "an empty manifest is refused"; st=1; } \
    || printf '  ok    %-52s (%s)\n' "an empty manifest is refused" "fail"

  # --- the PINNED-run input record, over the function the harness calls ---------------------------
  # The record is a fact about provenance, so "it was never written" must be a test failure and not
  # something noticed months later. These rows drive the real function.
  # a pinned snapshot carries its manifest at the root, exactly as apply-profile leaves it
  cp "$t/m5.tsv" "$t/full/toolchain-manifest.tsv"
  toolchain_record_input "$t/full" "$t/input.tsv"
  if [ -s "$t/input.tsv" ] && grep -q '^toolchain_root	' "$t/input.tsv"; then
    printf '  ok    %-52s (%s)\n' "a pinned run records what it consumed" "written"
  else printf '  FAIL  %-52s\n' "a pinned run records what it consumed"; st=1; fi
  # the hash is of the manifest actually present, not of a path a stale variable pointed at
  if [ "$(grep '^input_manifest_sha256	' "$t/input.tsv" | cut -f2)" = "$(_tc_sha <"$t/full/toolchain-manifest.tsv" 2>/dev/null)" ]; then
    printf '  ok    %-52s (%s)\n' "…and hashes the manifest it verified" "matches"
  else printf '  FAIL  %-52s\n' "…and hashes the manifest it verified"; st=1; fi
  # under the override there may be no manifest at all: MISSING is recorded, not omitted
  mkdir -p "$t/bare"
  TOOLCHAIN_ALLOW_UNPREPARED=1 toolchain_record_input "$t/bare" "$t/input2.tsv"
  if grep -qx 'input_manifest_sha256	MISSING' "$t/input2.tsv" \
    && grep -qx 'allow_unprepared	1' "$t/input2.tsv"; then
    printf '  ok    %-52s (%s)\n' "a missing manifest is recorded as MISSING" "present"
  else printf '  FAIL  %-52s\n' "a missing manifest is recorded as MISSING"; st=1; fi

  # the source digest notices an uncommitted edit — which is exactly what mtime cannot
  before=$(_toolchain_source_digest); echo drifted >>src/f.txt; after=$(_toolchain_source_digest)
  if [ "$before" != "$after" ]; then
    printf '  ok    %-52s (%s)\n' "the source digest sees an uncommitted edit" "differs"
  else printf '  FAIL  %-52s\n' "the source digest sees an uncommitted edit"; st=1; fi

  cd "$here" || exit 1
  [ "$st" = 0 ] && echo "OK: the toolchain diagnostic behaves as documented, limits included" \
    || echo "toolchain-manifest: self-test FAILED" >&2
  exit "$st"
fi
