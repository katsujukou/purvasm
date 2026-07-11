-- | `purvasm build`'s native link step (ADR-0082 §1, ADR-0072 §3): compile each emitted `.ll` object
-- | with `clang -c -O2`, compile any **referenced** ulib-shipped native-foreign `.c` (ADR-0073 §3), and
-- | link them all with the runtime staticlib into an executable, tree-shaking dead symbols (`--gc-sections`
-- | / `-dead_strip`). A faithful transcription of boot's `emit_native_llvm` link driver, over the CLI
-- | `PROC`/`FS` effects. Rust-crate foreigns (ADR-0078) are a later slice.
module Purvasm.CLI.NativeLink
  ( LinkOptions
  , link
  ) where

import Prelude

import Data.Argonaut.Core (Json, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array ((..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Fmt as Fmt
import Foreign.Object as Object
import Purvasm.Compiler.Backend.LLVM.Mangle (mangleForeign)
import Purvasm.CLI.Effect.Env (ENV)
import Purvasm.CLI.Effect.Env as Env
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Log as Log
import Purvasm.CLI.Effect.Process (PROC)
import Purvasm.CLI.Effect.Process as Proc
import Run (Run)
import Run.Except (EXCEPT, throw)
import Type.Row (type (+))

type LinkOptions =
  { output :: FilePath -- ^ the output directory (the executable lands at `<output>/app`)
  , buildDir :: FilePath -- ^ where the `.ll`/`.o` objects live (`<output>/_build`)
  , moduleCount :: Int -- ^ number of `mod_<i>.ll` objects (plus the `entry.ll` object)
  , runtimeLib :: Maybe FilePath -- ^ explicit runtime staticlib path (`--runtime-lib`), else resolved
  , ulibDir :: FilePath -- ^ the staged ulib dir — its `ulib.json` `foreign` map is the `.c` link plan
  }

-- | The default conventional runtime staticlib path (release profile — the inline-ABI objects this
-- | backend emits pair with the release runtime, ADR-0079 §2).
defaultRuntimeLib :: FilePath
defaultRuntimeLib = "runtime/target/release/libpurvasm_rt.a"

-- | Resolve the runtime staticlib: `--runtime-lib`, else `$PURVASM_RT_A`, else the conventional repo
-- | path — each checked to exist (ADR-0071 §1). Fails clearly if none is found.
resolveRuntimeLib
  :: forall r
   . Maybe FilePath
  -> Run (ENV + FS + EXCEPT String + r) FilePath
resolveRuntimeLib = case _ of
  Just p -> requireExisting p
  Nothing -> Env.lookupEnv "PURVASM_RT_A" >>= case _ of
    Just p -> requireExisting p
    Nothing -> requireExisting defaultRuntimeLib
  where
  requireExisting p = FS.exists p >>= case _ of
    true -> pure p
    false -> throw
      ( "runtime staticlib (libpurvasm_rt.a) not found at "
          <> p
          <> ". Pass --runtime-lib PATH, set $PURVASM_RT_A, or `cargo build --release` in runtime/."
      )

-- | Whether the host linker is Apple ld64 (macOS) — its tree-shaking / section flags differ from
-- | GNU ld / lld. Detected via `uname -s`, matching boot.
hostIsMacos :: forall r. Run (PROC + r) Boolean
hostIsMacos = Proc.execCapture "uname" [ "-s" ] >>= case _ of
  Right out -> pure (String.trim out == "Darwin")
  Left _ -> pure false

-- | The ulib native-foreign `.c` link plan (ADR-0073 §3): `<ulibDir>/ulib.json`'s `foreign` map (leaf
-- | key → `.c` path), each path resolved absolute against `ulibDir`. Absent file / field ⇒ empty; a
-- | malformed manifest is a link error. (Rust-crate providers, ADR-0078, are a later slice — non-string
-- | values are skipped here.)
loadForeignSources :: forall r. FilePath -> Run (FS + EXCEPT String + r) (Map String FilePath)
loadForeignSources ulibDir = do
  path <- FS.joinPath [ ulibDir, "ulib.json" ]
  FS.readText path >>= case _ of
    Nothing -> pure Map.empty
    Just raw -> case jsonParser raw of
      Left err -> throw (Fmt.fmt @"ulib manifest: {path}: {err}" { path, err })
      Right json -> case toObject json of
        Nothing -> throw (Fmt.fmt @"ulib manifest: {path}: top-level is not a JSON object" { path })
        Just top -> case Object.lookup "foreign" top of
          -- field absent ⇒ no native foreigns; field present but not an object ⇒ malformed.
          Nothing -> pure Map.empty
          Just fjson -> case toObject fjson of
            Nothing -> throw (Fmt.fmt @"ulib manifest: {path}: `foreign` is not a JSON object" { path })
            Just fo -> do
              -- non-string values (Rust-crate providers, ADR-0078/0091) are skipped here — a later slice.
              let rels = Array.mapMaybe (\(k /\ v) -> (\p -> Tuple k p) <$> toString v) (Object.toUnfoldable fo :: Array (String /\ Json))
              Map.fromFoldable <$> traverse (\(Tuple k rel) -> Tuple k <$> FS.joinPath [ ulibDir, rel ]) rels

-- | The co-distributed C header directory (`purvasm.h`) a ulib native foreign `.c` includes (ADR-0073):
-- | `$PURVASM_INCLUDE`, else the conventional `runtime/include`. Only resolved when a `.c` is compiled.
resolveInclude :: forall r. Run (ENV + FS + EXCEPT String + r) FilePath
resolveInclude = Env.lookupEnv "PURVASM_INCLUDE" >>= case _ of
  Just p -> requireInc p
  Nothing -> requireInc "runtime/include"
  where
  requireInc dir = do
    h <- FS.joinPath [ dir, "purvasm.h" ]
    FS.exists h >>= case _ of
      true -> pure dir
      false -> throw
        ("purvasm.h not found at " <> h <> " (needed to compile a ulib native foreign `.c`). Set $PURVASM_INCLUDE or build runtime/.")

-- | Whether an emitted `.ll` references the exact `AbiCodeFn` symbol `@pvf_<mangled>` — an **exact** match,
-- | not a substring, so a prefix-sharing sibling (`@pvf_…sin` vs `@pvf_…sinh`) does not false-positive and
-- | drag in an unrelated provider. Every mangled byte is `[A-Za-z0-9_]` (ADR-0072 §2), so an occurrence is a
-- | genuine reference iff the byte immediately after it is a non-identifier boundary (or end of text).
referencesForeign :: String -> String -> Boolean
referencesForeign mangled ll = case Array.tail (String.split (Pattern ("@" <> mangled)) ll) of
  Nothing -> false
  Just afters -> Array.any boundaryAfter afters
  where
  boundaryAfter seg = case String.uncons seg of
    Nothing -> true -- symbol at end of text
    Just { head } -> not (String.contains (Pattern (String.singleton head)) identChars)
  identChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"

-- | Compile each `.ll` object and any **referenced** ulib native-foreign `.c` (its `@pvf_<key>` symbol
-- | appears in an emitted `.ll`), then link them with the runtime staticlib into `<output>/app`. An
-- | unreferenced leaf's `.c` is not compiled (the linker would dead-strip it, but compiling it wastes work
-- | and can pull unrelated libs).
link :: forall r. LinkOptions -> Run (ENV + PROC + FS + LOG + EXCEPT String + r) Unit
link opts = do
  rt <- resolveRuntimeLib opts.runtimeLib
  macos <- hostIsMacos
  let
    -- GNU ld's `--gc-sections` strips only when each function/datum is in its own section; ld64
    -- dead-strips per symbol and needs nothing (ADR-0072 §3).
    sectionFlags = if macos then [] else [ "-ffunction-sections", "-fdata-sections" ]
    deadStrip = if macos then "-Wl,-dead_strip" else "-Wl,--gc-sections"
    tags = map (\i -> "mod_" <> show i) (0 .. (opts.moduleCount - 1)) <> [ "entry" ]
  llTexts <- traverse readLl tags
  objs <- traverse (compileObj sectionFlags) tags
  -- the ulib native foreigns actually referenced (their `@pvf_<mangle key>` appears in some `.ll`).
  foreignSources <- loadForeignSources opts.ulibDir
  let
    referenced = Array.filter
      (\(Tuple key _) -> any (referencesForeign (mangleForeign key)) llTexts)
      (Map.toUnfoldable foreignSources :: Array (String /\ FilePath))
    -- Dedup to unique source paths: a manifest may map many keys to one `.c`
    -- (e.g. Data.Number.{floor,ceil,…} all → Data.Number.c). Compiling it once per key
    -- would define its `pvf_` symbols in several objects ⇒ duplicate-symbol link error.
    bySource =
      Map.toUnfoldable (Map.fromFoldableWith (<>) (map (\(key /\ src) -> src /\ [ key ]) referenced))
        :: Array (FilePath /\ Array String)
  foreignObjs <-
    if Array.null bySource then pure []
    else do
      inc <- resolveInclude
      traverse (compileForeign sectionFlags inc) (Array.mapWithIndex Tuple bySource)
  exe <- FS.joinPath [ opts.output, "app" ]
  -- `-lm` for libm calls a native foreign `.c` may emit; harmless with none.
  runClang "link" ([ deadStrip ] <> objs <> foreignObjs <> [ rt, "-lm", "-o", exe ])
  Log.info $ Fmt.fmt @"✓ Linked native executable → {exe}" { exe }
  where
  readLl tag = do
    p <- FS.joinPath [ opts.buildDir, tag <> ".ll" ]
    FS.readText p >>= case _ of
      Just t -> pure t
      Nothing -> throw (Fmt.fmt @"missing build artifact {p} (expected an emitted `.ll` object)" { p })

  compileObj sectionFlags tag = do
    ll <- FS.joinPath [ opts.buildDir, tag <> ".ll" ]
    obj <- FS.joinPath [ opts.buildDir, tag <> ".o" ]
    runClang (tag <> " (clang -c)") ([ "-c", "-O2" ] <> sectionFlags <> [ ll, "-o", obj ])
    pure obj

  compileForeign sectionFlags inc (Tuple i (Tuple cPath keys)) = do
    obj <- FS.joinPath [ opts.buildDir, "foreign_" <> show i <> ".o" ]
    let forKeys = String.joinWith ", " keys
    runClang (Fmt.fmt @"foreign {cPath} (for {forKeys}) (clang -c)" { cPath, forKeys })
      ([ "-c", "-O2" ] <> sectionFlags <> [ "-I" <> inc, cPath, "-o", obj ])
    pure obj

  runClang label args = Proc.exec "clang" args >>= case _ of
    Right _ -> pure unit
    Left e -> throw (Fmt.fmt @"{label} failed: {e}" { label, e })
