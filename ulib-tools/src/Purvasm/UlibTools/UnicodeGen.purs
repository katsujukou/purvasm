-- | `ulib-tools unicode-gen` (ADR-0101): fetch, pin, and generate `Data.String.Internal.CaseMap`
-- | from the Unicode Character Database's `UnicodeData.txt` — the typed replacement for
-- | `gen/gen_casemap.py`. `UnicodeData.txt` itself is never committed: it is fetched (via `curl`)
-- | into a gitignored, version-keyed cache, and `ulib/strings/gen/unicode-data.json` is the single
-- | committed pin (`{ ucdVersion, sha256 }`) that fixes exactly which upstream bytes are trusted.
-- |
-- | Three modes:
-- | - default: verify the cached (or freshly fetched) file's sha256 against the pin, then generate
-- |   and write `--out`.
-- | - `--pin <version>`: unconditionally re-fetch `<version>` (never reuses a cached copy — a
-- |   same-version cache could be an earlier, since-corrected upstream file), then generate and
-- |   commit **both** the pin and `--out` as one unit — parsing/self-check/rendering must all
-- |   succeed first, then `--out` is committed, then the pin; if the pin commit itself fails,
-- |   `--out` is rolled back to its pre-transaction state (`commitPinAndOut`), so the pair can
-- |   never be observed as "new output, old pin" (or vice versa) — only all-old or all-new.
-- | - `--check`: generate in memory and diff against the existing `--out` instead of writing.
module Purvasm.UlibTools.UnicodeGen
  ( Options
  , options
  , cmd
  , commitFailureMessage
  ) where

import Prelude

import ArgParse.Basic (ArgParser, fromRecord)
import ArgParse.Basic as ArgParser
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust, maybe)
import Fmt as Fmt
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Log as Log
import Purvasm.CLI.Effect.Process (PROC)
import Purvasm.CLI.Effect.Process as Proc
import Purvasm.UlibTools.Hash as Hash
import Purvasm.UlibTools.Stage as Stage
import Purvasm.UlibTools.UnicodeData (Pin)
import Purvasm.UlibTools.UnicodeData as UnicodeData
import Run (EFFECT, Run, liftEffect)
import Run.Except (EXCEPT, throw)
import Type.Row (type (+))

type Options =
  { pin :: Maybe String
  , check :: Boolean
  , lockFile :: FilePath
  , out :: FilePath
  , cacheDir :: FilePath
  , curl :: String
  }

options :: ArgParser Options
options = fromRecord
  { pin:
      ArgParser.argument [ "--pin" ]
        "Fetch this Unicode Character Database version fresh and rewrite the pin\n\
        \(ulib/strings/gen/unicode-data.json) to its sha256, then generate --out. The only way\n\
        \to change what is pinned. Mutually exclusive with --check."
        # ArgParser.optional
  , check:
      ArgParser.flag [ "--check" ]
        "Generate in memory and diff against the existing --out instead of writing it;\n\
        \non-zero exit on any difference. Mutually exclusive with --pin."
        # ArgParser.boolean
  , lockFile:
      ArgParser.argument [ "--lock" ]
        "The committed pin file. Defaults to 'ulib/strings/gen/unicode-data.json'."
        # ArgParser.default "ulib/strings/gen/unicode-data.json"
  , out:
      ArgParser.argument [ "--out" ]
        "Destination for the generated module. Defaults to\n\
        \'ulib/strings/Data.String.Internal.CaseMap.purs'."
        # ArgParser.default "ulib/strings/Data.String.Internal.CaseMap.purs"
  , cacheDir:
      ArgParser.argument [ "--cache-dir" ]
        "Cache dir for fetched UnicodeData.txt (keyed by version, so re-runs with an unchanged\n\
        \pin are offline). Defaults to '.unicode-cache'."
        # ArgParser.default ".unicode-cache"
  , curl:
      ArgParser.argument [ "--curl" ]
        "curl executable. Defaults to 'curl'."
        # ArgParser.default "curl"
  }

cmd :: forall r. Options -> Run (PROC + FS + LOG + EXCEPT String + EFFECT + r) Unit
cmd opts = do
  when (isJust opts.pin && opts.check) do
    throw "unicode-gen: --pin and --check are mutually exclusive."

  -- Resolve the target version and, for the non-pinning path, the hash it must match. `--pin`
  -- carries no `expectedHash`: it trusts whatever it is about to fetch fresh (that fetch is what
  -- redefines the pin), so there is nothing yet to compare it against.
  { version, expectedHash } <- case opts.pin of
    Just v -> pure { version: v, expectedHash: Nothing }
    Nothing -> do
      pinned <- readPin opts.lockFile
      pure { version: pinned.ucdVersion, expectedHash: Just pinned.sha256 }

  FS.mkdirP opts.cacheDir
  cachePath <- FS.joinPath [ opts.cacheDir, "UnicodeData-" <> version <> ".txt" ]
  case opts.pin of
    -- Re-pinning must observe upstream's *current* bytes: reusing a same-version cache from an
    -- earlier run could silently re-pin a stale, since-corrected copy.
    Just _ -> fetchFresh opts cachePath version
    Nothing -> ensureCached opts cachePath version
  hash <- liftEffect (Hash.sha256File cachePath)

  for_ expectedHash \expected ->
    when (expected /= hash) do
      throw $ Fmt.fmt
        @"unicode-gen: sha256 mismatch for UnicodeData.txt {version}: pinned {expected}, fetched \
        \{actual} (the upstream file changed, the cache is corrupted, or this is an intentional \
        \version bump -- use --pin {version} to re-pin)."
        { version, expected, actual: hash }

  -- Everything from here on is pure computation over already-fetched (and, off the --pin path,
  -- already-hash-verified) bytes: parse, self-check, and render must all succeed *before* either
  -- the pin or --out is touched, so a failure anywhere above never leaves them drifted apart.
  src <- FS.readText cachePath >>= maybe
    (throw $ Fmt.fmt @"unicode-gen: cannot read {cachePath}" { cachePath })
    pure
  tables <- either throw pure (UnicodeData.parseUnicodeData src)
  either throw pure (UnicodeData.selfCheck tables)
  let generated = UnicodeData.renderCaseMapModule { ucdVersion: version } tables

  if opts.check then
    FS.readText opts.out >>= maybe
      ( throw $ Fmt.fmt @"unicode-gen: {out} does not exist -- run 'unicode-gen' without --check first"
          { out: opts.out }
      )
      \existing ->
        if existing == generated then
          Log.info $ Fmt.fmt @"unicode-gen: {out} is up to date with UCD {version}."
            { out: opts.out, version }
        else
          throw $ Fmt.fmt
            @"unicode-gen: {out} is STALE against the pinned UCD {version} -- regenerate \
            \(ulib-tools unicode-gen) and commit."
            { out: opts.out, version }
  else case opts.pin of
    Nothing -> do
      -- Only one file changes: a plain write-temp-then-rename is a sufficient transaction.
      writeAtomic opts.out generated
      Log.info $ Fmt.fmt @"unicode-gen: wrote {out} (UCD {version})." { out: opts.out, version }
    Just v -> do
      commitPinAndOut opts (UnicodeData.renderPin { ucdVersion: v, sha256: hash }) generated
      Log.info $ Fmt.fmt @"unicode-gen: pinned UCD {v} (sha256 {hash})" { v, hash }
      Log.info $ Fmt.fmt @"unicode-gen: wrote {out} (UCD {version})." { out: opts.out, version }

-- | Write `content` to `path` via a temp file (`path <> ".tmp"`) plus `mv` (a same-filesystem
-- | rename), so a failure never leaves `path` half-written.
writeAtomic :: forall r. FilePath -> String -> Run (FS + PROC + EXCEPT String + r) Unit
writeAtomic path content = do
  let tmp = path <> ".tmp"
  FS.writeText tmp content
  void $ Stage.requireOk =<< Proc.exec "mv" [ "-f", tmp, path ]

-- | Commit the pin and `--out` together, so a failure partway through can never leave them
-- | drifted apart. Stages both as temp files first, then commits `--out`, then the pin; if
-- | *either* commit fails, `--out` is rolled back to whatever it held before this call (its prior
-- | content if it existed, deleted otherwise). The reverse case (the `--out` commit itself
-- | failing) needs the same rollback attempt for symmetry, even though the pin was never touched
-- | in that case (its commit is not reached) — the backup-move already happened either way.
-- |
-- | The rollback is itself a filesystem operation and can itself fail (e.g. the backup was removed
-- | from under us, or `--out`'s directory becomes unwritable between the two commits) — that
-- | result is checked, never discarded, because a silently-ignored rollback failure is exactly how
-- | "new output, old/missing pin" could reappear despite the rollback *attempt*. A rollback
-- | failure is reported as a distinct, more urgent error naming the inconsistency and how to
-- | recover by hand (`commitFailureMessage`).
commitPinAndOut
  :: forall r. Options -> String -> String -> Run (FS + PROC + EXCEPT String + r) Unit
commitPinAndOut opts pinContent outContent = do
  let
    outTmp = opts.out <> ".tmp"
    lockTmp = opts.lockFile <> ".tmp"
    outBak = opts.out <> ".bak"
  FS.writeText outTmp outContent
  FS.writeText lockTmp pinContent

  hadOldOut <- FS.exists opts.out
  when hadOldOut do
    void $ Stage.requireOk =<< Proc.exec "mv" [ "-f", opts.out, outBak ]

  let
    rollbackOut =
      if hadOldOut then Proc.exec "mv" [ "-f", outBak, opts.out ]
      else Proc.exec "rm" [ "-f", opts.out ]

    -- Build the failure message for a rollback that has just been attempted. `hadOldOut` only
    -- records that a backup was *taken* at the start of this transaction — it says nothing about
    -- whether `outBak` is still there by the time a rollback actually fails (it could have been
    -- removed externally, or that removal could be the very reason the rollback failed), so the
    -- message must check `outBak`'s presence fresh, right now, rather than trust that stale flag.
    reportFailure stage cause rollbackResult = do
      backupExists <- FS.exists outBak
      pure $ commitFailureMessage { out: opts.out, lockFile: opts.lockFile, outBak, backupExists }
        stage
        cause
        rollbackResult

  outCommit <- Proc.exec "mv" [ "-f", outTmp, opts.out ]
  case outCommit of
    Left e -> do
      rollbackResult <- rollbackOut
      msg <- reportFailure ("committing " <> opts.out) e rollbackResult
      throw msg
    Right _ -> do
      lockCommit <- Proc.exec "mv" [ "-f", lockTmp, opts.lockFile ]
      case lockCommit of
        Left e -> do
          -- The pin commit failed: undo the --out commit so the pair is never left
          -- "new output, stale pin".
          rollbackResult <- rollbackOut
          msg <- reportFailure ("committing the pin (" <> opts.lockFile <> ")") e rollbackResult
          throw msg
        Right _ ->
          when hadOldOut do
            void $ Proc.exec "rm" [ "-f", outBak ]

-- | The error message for a commit step that failed at `stage` (naming what was being committed
-- | and why), given whether the automatic rollback of `--out` that follows it succeeded, and
-- | whether the pre-transaction backup (`outBak`) is actually present *right now* (checked by the
-- | caller at failure time, not inferred from history — see `commitPinAndOut`'s `reportFailure`).
-- | Pulled out as a pure function (rather than inlined `Fmt.fmt` calls at each throw site) so the
-- | rollback-also-failed branch — the one a live filesystem-failure injection can't reliably
-- | reproduce on demand — is covered by a plain unit test instead.
commitFailureMessage
  :: { out :: FilePath, lockFile :: FilePath, outBak :: FilePath, backupExists :: Boolean }
  -> String
  -> String
  -> Either String Unit
  -> String
commitFailureMessage ctx stage cause rollbackResult = case rollbackResult of
  Right _ ->
    Fmt.fmt
      @"unicode-gen: failed {stage}: {cause} -- {out} was rolled back to its previous state \
      \so the pair never drifts."
      { stage, cause, out: ctx.out }
  Left rollbackErr ->
    Fmt.fmt
      @"unicode-gen: failed {stage}: {cause}. THE ROLLBACK OF {out} ALSO FAILED: {rollbackErr} -- \
      \{out} and {lockFile} may now be INCONSISTENT with each other. {recovery}"
      { stage, cause, out: ctx.out, lockFile: ctx.lockFile, rollbackErr, recovery: recoveryHint ctx }

-- | Where to look to recover by hand after a rollback failure: the pre-transaction backup path,
-- | *only if it is still actually present* -- never claimed unconditionally from "a backup was
-- | taken earlier", since the backup itself may be what's missing (removed externally, or the
-- | very reason the rollback's rename failed).
recoveryHint :: { out :: FilePath, lockFile :: FilePath, outBak :: FilePath, backupExists :: Boolean } -> String
recoveryHint ctx =
  if ctx.backupExists then
    "Its pre-transaction content is preserved at " <> ctx.outBak <> " -- restore it by hand (mv "
      <> ctx.outBak
      <> " "
      <> ctx.out
      <> ") or reconcile the pin to match "
      <> ctx.out
      <> "'s current content."
  else
    ctx.out
      <>
        " may hold newly-generated content with no matching pin, and no backup is available to \
        \recover from (either none was taken, or it is no longer present) -- inspect and \
        \reconcile by hand, or re-pin once the underlying failure is fixed."

-- | The pin (`{ ucdVersion, sha256 }`) from `path`; a hard error naming `--pin` if it is missing or
-- | malformed (no silent "un-pinned" run — ADR-0101 mirrors ADR-0055's fail-fast posture).
readPin :: forall r. FilePath -> Run (FS + EXCEPT String + r) Pin
readPin path =
  FS.readText path >>= case _ of
    Nothing -> throw $ Fmt.fmt
      @"unicode-gen: no pin at {path} -- run with --pin <version> first."
      { path }
    Just src -> either
      (\e -> throw $ Fmt.fmt @"unicode-gen: {path}: {e}" { path, e })
      pure
      (UnicodeData.parsePin src)

-- | Fetch `UnicodeData.txt` for `version` into `cachePath` if not already cached (version-keyed, so
-- | a re-run against an unchanged pin is offline — mirrors `Test.ensureClone`'s `repo@ref` cache).
-- | Used off the `--pin` path, where an existing cached copy is exactly what the pin expects.
ensureCached
  :: forall r. Options -> FilePath -> String -> Run (PROC + FS + LOG + EXCEPT String + r) Unit
ensureCached opts cachePath version =
  unlessM (FS.exists cachePath) (fetchFresh opts cachePath version)

-- | Fetch `UnicodeData.txt` for `version` into `cachePath`, unconditionally overwriting any cached
-- | copy. Used by `--pin`: re-pinning the same version must observe upstream's *current* bytes, not
-- | a possibly-stale copy left over from an earlier run (a same-version cache hit here would defeat
-- | the point of re-pinning — ADR-0101).
fetchFresh :: forall r. Options -> FilePath -> String -> Run (PROC + FS + LOG + EXCEPT String + r) Unit
fetchFresh opts cachePath version = do
  Log.info $ Fmt.fmt @"unicode-gen: fetching UnicodeData.txt {version}…" { version }
  void $ Stage.requireOk =<< Proc.exec opts.curl [ "-fsSL", ucdUrl version, "-o", cachePath ]

-- | The Unicode Character Database's stable per-version file layout.
ucdUrl :: String -> String
ucdUrl version = "https://www.unicode.org/Public/" <> version <> "/ucd/UnicodeData.txt"
