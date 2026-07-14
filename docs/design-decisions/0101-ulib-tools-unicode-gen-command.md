# 0101. `ulib-tools unicode-gen`: fetch, pin, and generate the Unicode simple-case-mapping table, replacing `gen/gen_casemap.py`

- Status: ~~Proposed~~ **Accepted** _(2026-07-14: accepted by the maintainer)_
- Date: 2026-07-14

## Context

`ulib/strings/Data.String.Internal.CaseMap` (the table backing `Data.String.Common`'s `toLower`/
`toUpper`, ADR-0038/ADR-0006) is generated today by `ulib/strings/gen/gen_casemap.py`: a Python
script that reads a vendored `gen/UnicodeData.txt` (the Unicode Character Database's per-code-point
field file, ~35k lines), extracts fields 12/13 (`Simple_Uppercase_Mapping`/`Simple_Lowercase_Mapping`),
self-checks a handful of known-tricky code points (U+0130/U+0131/U+00DF/U+1E9E/U+017F — see the
script's own doc comment), and prints the `.purs` module to stdout for the maintainer to redirect and
commit.

This is the only Python dependency in the toolchain, which — per ADR-0043's decision to keep
`ulib-tools` "a typed, testable CLI consistent with the rest of the PureScript-first toolchain" —
is exactly the kind of `sh`/ad-hoc-script glue that ADR grew `ulib-tools` to replace. Two more
specific problems:

- **The vendored `UnicodeData.txt` has no place to live.** It is currently an untracked file sitting
  under a tracked directory (`ulib/strings/gen/`) — an accident, not a decision. At ~35k lines of
  third-party data it should not enter git history at all, but nothing today fetches or verifies it;
  a fresh checkout simply has no input for the generator.
- **Nothing pins *which* upstream bytes produced the committed `.purs`.** The script's docstring names
  a Unicode version (`15.0.0`) in a comment, but there is no machine-checked link between "the table in
  `Data.String.Internal.CaseMap.purs`" and "the exact `UnicodeData.txt` that produced it" — a
  silently-stale regeneration, a hand edit, or a corrupted re-download would all be undetectable.

## Decision

### 1. A new `ulib-tools unicode-gen` subcommand

Joins `build`/`verify`/`verify-deps`/`test` (ADR-0043). It shells out to one more external tool,
`curl` (alongside the toolchain's existing `purs`/`spago`/`git`/`node`/`purvm`, ADR-0043 §1), to fetch
`https://www.unicode.org/Public/<version>/ucd/UnicodeData.txt`.

### 2. A committed lock file is the single fixed reference to the upstream input

`ulib/strings/gen/unicode-data.json` (small, committed) records the pin:

```json
{ "ucdVersion": "15.0.0", "sha256": "<hex>" }
```

`UnicodeData.txt` itself is **never committed**. It is fetched into a gitignored, version-keyed cache
(`.unicode-cache/UnicodeData-<version>.txt`, mirroring `.ulib-test-cache`'s `repo@ref` keying from
ADR-0048) so re-runs with an unchanged pin are offline. `/.unicode-cache` is added to `.gitignore`.

### 3. Three modes, one command

- **Default** (`ulib-tools unicode-gen`): read the pinned `ucdVersion`/`sha256` from the lock file;
  fetch (or reuse the cached copy of) that version's `UnicodeData.txt`; **hard-error if its sha256
  does not match the pinned hash** (a changed, corrupted, or wrong-version file is a build-time
  failure, not a silent drift — same posture as ADR-0055's "fail fast" correction); parse it; run the
  self-check invariants (§5); render the `.purs`; write it to `--out` (default
  `ulib/strings/Data.String.Internal.CaseMap.purs`).
- **`--pin <version>`**: fetch `<version>` fresh, compute its sha256, and **rewrite the lock file** to
  that `{ ucdVersion, sha256 }` — the one explicit, reviewable action that changes what is pinned
  (e.g. adopting a new Unicode release). Then generates/writes as above, so the lock and the generated
  `.purs` never drift apart across a re-pin. Mutually exclusive with `--check`.
- **`--check`**: generate in memory against the current pin and **diff against the existing `--out`**
  instead of writing; non-zero exit on any difference. This is what `prepare-release.sh` and CI run —
  it catches a hand-edited generated file or a lock bump that was never regenerated (no silent
  omission, matching ADR-0048's own "no silent omission" principle for its own gate).

Re-pinning is the only path that changes the recorded hash; every other invocation treats the pin as
fixed and verifies against it, so "the committed table came from exactly this upstream file" is a
machine-checked fact, not a comment.

### 4. Parsing/rendering as a pure, unit-tested module

A new `Purvasm.UlibTools.UnicodeData` module ports the script's logic as pure functions: parse
`UnicodeData.txt` into sorted `(codepoint, mapping)` pairs for fields 12/13, the self-check assertions
(ASCII round-trip; U+0130→U+0069 simple lowercase; U+0131→U+0049 simple uppercase; U+00DF absent from
the uppercase table; U+1E9E→U+00DF; U+017F→U+0053; NUL/`'0'`/CJK `中` absent from both), and the flat
`[cp, mapped, …]` table renderer. Being pure, this is unit-tested directly per module (CLAUDE.md/
ADR-0043's testing philosophy), unlike the script's print-to-stdout original.

### 5. SHA-256 stays local to `ulib-tools`, not `cli-lib`

A small Node `crypto`-backed FFI (`Purvasm.UlibTools.Hash`, hashing the raw downloaded bytes — not a
UTF-8 text round-trip) lives in `ulib-tools` itself. `cli-lib` is the *extracted-because-actually-shared*
surface between `cli` and `ulib-tools` (ADR-0043 §1); no other command needs hashing today, so adding
it there would be speculative. Revisit only if a second consumer appears.

### 6. Retire the Python script and the loose data file

`ulib/strings/gen/gen_casemap.py` and the untracked `ulib/strings/gen/UnicodeData.txt` are deleted.
`ulib/strings/gen/unicode-data.json` is the only file left in `gen/`.

### 7. Wire into the release gate

`prepare-release.sh` gains a leading `unicode-gen --check` step, ahead of `verify-deps` (cheapest
first, per the file's existing ordering rationale).

## Consequences

- No Python anywhere in the toolchain; `unicode-gen` is PureScript, unit-testable, and consistent with
  the rest of `ulib-tools` (ADR-0043).
- `UnicodeData.txt` never enters git history; the repo instead carries a ~2-line JSON pin.
- Bumping the Unicode version is one explicit, reviewable `--pin` action (a small diff of
  `unicode-data.json` plus the regenerated `.purs`), not an unaudited script re-run.
- CI/`prepare-release.sh` can catch a stale or hand-edited generated file via `--check`.
- One more required external tool (`curl`), alongside the toolchain's existing git/purs/spago/node/purvm
  dependencies (ADR-0043 §1).

## Alternatives considered

- **Keep the Python script.** Rejected — it is exactly the ad-hoc-script dependency ADR-0043 moved
  `ulib-tools` away from, and is the problem this record exists to fix.
- **Vendor `UnicodeData.txt` into the repo.** Rejected: ~35k lines of third-party data churning the
  repo on every Unicode bump, when a hash-checked fetch-with-cache (already the precedent for upstream
  test suites, ADR-0043/0048) gives the same reproducibility without the weight.
- **Pin only the version string, not a hash.** Rejected: does not detect a local corruption, a
  mid-download truncation, or unicode.org silently revising a "final" release file at the same URL —
  the hash is the actual reproducibility guarantee; the version alone is not.
- **Every generation run rewrites the lock to whatever was just fetched** (no distinct `--pin` action).
  Rejected: would silently re-pin to a *changed* upstream file with no chance to review the diff first,
  defeating the point of pinning. Re-pinning must be an explicit, auditable action.
- **Put the sha256/fetch capability in `cli-lib`.** Deferred: no other command needs it yet; `cli-lib`
  is for *actually shared* node-effect helpers (ADR-0043 §1), not speculative future reuse.
