-- | `purvasm build`'s native link step (ADR-0082 §1, ADR-0072 §3): compile each emitted `.ll` object
-- | with `clang -c -O2`, compile the **referenced** native-foreign providers — ulib `.c` (ADR-0073 §3),
-- | app-C sibling `.c` and the app-Rust `--rust-ffi` crate bundle (ADR-0091) — audit them against a
-- | provider map (exactly one provider per app key), and link with the runtime staticlib (or, under
-- | `--rust-ffi`, the bundle that folds it) into an executable, tree-shaking dead symbols
-- | (`--gc-sections` / `-dead_strip`). Over the CLI `PROC`/`FS` effects.
module Purvasm.CLI.NativeLink
  ( LinkOptions
  , link
  ) where

import Prelude

import Data.Argonaut.Core (Json, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array ((..))
import Data.Array as Array
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Foldable (any, foldMap)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Fmt as Fmt
import Foreign.Object as Object
import Purvasm.Compiler.Backend.LLVM.Mangle (escapeIdent, mangleForeign)
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
  -- | Local workspace source modules (ADR-0091 §2): the app-C sibling `.c` provider search space. Each is
  -- | the CoreFn module `key` and its `.purs` `sourcePath`; the sibling provider is `sourcePath` with the
  -- | extension swapped. Only non-registry workspace source belongs here — `.spago` dependencies never do.
  , appModules :: Array { key :: String, sourcePath :: FilePath }
  -- | `--rust-ffi <dir>` (ADR-0091 §3/§Addendum): the single app-Rust crate. Selecting it suppresses
  -- | app-C provider *compilation* (siblings are still scanned to enforce C-xor-Rust) and links the
  -- | crate as a `purvasm-bundle` staticlib folding the runtime rlib (ADR-0078 §5).
  , rustFfiDir :: Maybe FilePath
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

-- | Every `@pvf_<symbol>` (a native leaf reference, ADR-0073 §3) an emitted `.ll` mentions — the *textual*
-- | referenced-key set. It over-approximates the link (dead etas included), so only its **workspace**
-- | subset gets the pre-link exactly-one-provider check (ADR-0091 §1); non-workspace leaves are left to
-- | the linker + dead-strip. A symbol runs `pvf_` then only `[A-Za-z0-9_]` (the `escapeIdent` alphabet,
-- | ADR-0072 §2), so it ends at the first other byte.
pvfSymbolsIn :: String -> Array String
pvfSymbolsIn ll =
  map (\seg -> "pvf_" <> SCU.takeWhile isSymbolByte seg)
    (fromMaybe [] (Array.tail (String.split (Pattern "@pvf_") ll)))
  where
  isSymbolByte c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'

-- | The `pvf_*` symbols an object/archive **defines** (`nm`, ADR-0091 §4's symbol audit) — the exported
-- | providers, so a `.c` that compiles but exports the wrong symbol is caught by key, not by a raw linker
-- | `undefined symbol`. `nm` prints `<addr> <type> <name>` for a definition and `<U> <name>` for an
-- | undefined reference; a defined symbol's type is not `U`/`u`. Names carry a leading `_` on Mach-O.
-- | The **multiset** of `pvf_*` symbols an object/archive defines — the raw list *with duplicates*, so the
-- | bundle audit (ADR-0078 §5) can count definitions per key (0 = missing, 1 = ok, > 1 = two members define
-- | it). `nmDefinedPvf` dedups this to a `Set` for the provider-membership checks.
nmDefinedPvfList :: forall r. FilePath -> Run (PROC + EXCEPT String + r) (Array String)
nmDefinedPvfList path = do
  -- Prefer `llvm-nm` (ADR-0078 §5): it reads Rust-produced archives cleanly, where Apple `nm` can warn
  -- and exit non-zero. Fall back to `nm`; a failure of *both* is an error, never an empty (masked) result.
  -- `execCaptureQuiet`: discard the lister's stderr — `llvm-nm` prints "no symbols" per empty archive
  -- member (rust-std metadata objects) to stderr; that is benign noise, and its stdout still lists the real
  -- symbols. A genuine failure still yields `Left`, so the `nm` fallback / error path is unaffected.
  out <- Proc.execCaptureQuiet "llvm-nm" [ path ] >>= case _ of
    Right o -> pure o
    Left _ -> Proc.execCaptureQuiet "nm" [ path ] >>= case _ of
      Right o -> pure o
      Left e -> throw (Fmt.fmt @"nm/llvm-nm {path} failed: {e}" { path, e })
  pure (Array.mapMaybe defined (String.split (Pattern "\n") out))
  where
  defined line =
    let
      toks = Array.filter (_ /= "") (String.split (Pattern " ") line)
    in
      case Array.last toks, Array.index toks (Array.length toks - 2) of
        Just name, Just ty
          | ty /= "U" && ty /= "u" ->
              let
                sym = fromMaybe name (String.stripPrefix (Pattern "_") name)
              in
                if isJust (String.stripPrefix (Pattern "pvf_") sym) then Just sym else Nothing
        _, _ -> Nothing

-- | The set of `pvf_*` symbols an object/archive **defines** (`nm`, ADR-0091 §4's symbol audit): the
-- | exported providers, deduped. Used for provider-membership; the count-sensitive bundle audit uses
-- | [nmDefinedPvfList] directly. A `.c`/crate that exports the wrong symbol is caught by key, not by a raw
-- | linker `undefined symbol`.
nmDefinedPvf :: forall r. FilePath -> Run (PROC + EXCEPT String + r) (Set String)
nmDefinedPvf = map Set.fromFoldable <<< nmDefinedPvfList

-- | A best-effort inverse of `escapeIdent` for diagnostics (ADR-0091 §4): recover the qualified key from a
-- | `pvf_<escape key>` symbol by undoing the two escapes that occur in practice (`_2e`→`.`, `_5f`→`_`).
-- | Rarer byte escapes stay as `_<hex>` — this feeds an error message, not the link.
demangleKey :: String -> String
demangleKey sym =
  String.replaceAll (Pattern "_5f") (Replacement "_")
    ( String.replaceAll (Pattern "_2e") (Replacement ".")
        (fromMaybe sym (String.stripPrefix (Pattern "pvf_") sym))
    )

-- | The `purvasm-rt` runtime **crate** dir (its `Cargo.toml`) the app-Rust bundle folds as an rlib
-- | (ADR-0091 §Addendum / ADR-0078 §5): `$PURVASM_RT_CRATE`, else the conventional `runtime`. Distinct
-- | from the runtime *staticlib* (`resolveRuntimeLib`) — only app-Rust, which needs cargo anyway, resolves
-- | it. Returned absolute (the bundle `Cargo.toml`'s path dep must resolve from the generated crate dir).
resolveRuntimeCrate :: forall r. Run (ENV + FS + EXCEPT String + r) FilePath
resolveRuntimeCrate = Env.lookupEnv "PURVASM_RT_CRATE" >>= case _ of
  Just p -> requireCrate p
  Nothing -> requireCrate "runtime"
  where
  requireCrate dir = do
    mf <- FS.joinPath [ dir, "Cargo.toml" ]
    FS.exists mf >>= case _ of
      true -> FS.resolvePath [] dir
      false -> throw
        ("purvasm-rt crate not found at " <> mf <> " (needed to bundle a --rust-ffi crate). Set $PURVASM_RT_CRATE.")

-- | A crate's `package.name`, from the first `name = "…"` line of its `Cargo.toml` (a full TOML parse
-- | buys nothing) — the bundle's path dependency renames it to `dep0` (ADR-0078 §5's `crate_name`).
crateName :: forall r. FilePath -> Run (FS + EXCEPT String + r) String
crateName dir = do
  mf <- FS.joinPath [ dir, "Cargo.toml" ]
  FS.readText mf >>= case _ of
    Nothing -> throw ("cannot read " <> mf <> " (the --rust-ffi crate needs a Cargo.toml)")
    Just raw -> case Array.findMap nameOf (String.split (Pattern "\n") raw) of
      Just n -> pure n
      Nothing -> throw ("no package name in " <> mf)
  where
  nameOf line = do
    afterName <- String.stripPrefix (Pattern "name") (String.trim line)
    afterEq <- String.stripPrefix (Pattern "=") (String.trim afterName)
    val <- String.stripPrefix (Pattern "\"") (String.trim afterEq)
    String.stripSuffix (Pattern "\"") val

-- | A TOML basic string of `s` (for the generated bundle `Cargo.toml` path deps), char by char like boot's
-- | `toml_string`: `"`→`\"`, `\`→`\\`, control bytes (`< 0x20`) → `\u00XX`. `Filename.quote`-style shell
-- | escaping would break as TOML on a path containing a quote/backslash.
tomlString :: String -> String
tomlString s = "\"" <> foldMap esc (SCU.toCharArray s) <> "\""
  where
  esc c
    | c == '"' = "\\\""
    | c == '\\' = "\\\\"
    | toCharCode c < 0x20 =
        let
          h = Int.toStringAs Int.hexadecimal (toCharCode c)
        in
          "\\u00" <> (if SCU.length h == 1 then "0" <> h else h)
    | otherwise = SCU.singleton c

-- | Synthesize the `purvasm-bundle` staticlib crate under `<workDir>/bundle` and `cargo build --release`
-- | it: ONE `crate-type = ["staticlib"]` folding the runtime rlib and the app's foreign crate (ADR-0078
-- | §5 — two Rust staticlibs cannot co-link, duplicate `libstd`). Release matches the inline-ABI objects'
-- | `pv_ctx_abi_v<N>` pairing (ADR-0079 §2). Returns the archive path. Ports `native_bundle.build_bundle`.
buildBundle
  :: forall r
   . { workDir :: FilePath, runtimeCrate :: FilePath, crateDir :: FilePath, crateNm :: String }
  -> Run (PROC + FS + EXCEPT String + r) FilePath
buildBundle b = do
  bundleDir <- FS.joinPath [ b.workDir, "bundle" ]
  srcDir <- FS.joinPath [ bundleDir, "src" ]
  FS.mkdirP srcDir
  cargoToml <- FS.joinPath [ bundleDir, "Cargo.toml" ]
  FS.writeText cargoToml cargoTomlText
  libRs <- FS.joinPath [ srcDir, "lib.rs" ]
  FS.writeText libRs libRsText
  targetDir <- FS.joinPath [ b.workDir, "cargo" ]
  Proc.exec "cargo" [ "build", "--release", "--manifest-path", cargoToml, "--target-dir", targetDir ] >>= case _ of
    Left e -> throw (Fmt.fmt @"cargo build of the --rust-ffi bundle failed: {e}" { e })
    Right _ -> FS.joinPath [ targetDir, "release", "libpurvasm_bundle.a" ]
  where
  cargoTomlText =
    "[package]\n\
    \name = \"purvasm-bundle\"\n\
    \version = \"0.0.0\"\n\
    \edition = \"2021\"\n\
    \publish = false\n\n\
    \[lib]\n\
    \crate-type = [\"staticlib\"]\n\n\
    \[dependencies]\n\
    \purvasm-rt = { path = "
      <> tomlString b.runtimeCrate
      <>
        " }\n\
        \dep0 = { path = "
      <> tomlString b.crateDir
      <> ", package = "
      <> tomlString b.crateNm
      <>
        " }\n\n\
        \[workspace]\n\n\
        \[profile.release]\n\
        \overflow-checks = true\n"
  libRsText =
    "//! Generated by `purvasm`: co-links the runtime rlib and the app's Rust foreign\n\
    \//! crate into ONE staticlib.\n\
    \pub use purvasm_rt as _rt;\n\
    \pub use dep0 as _d0;\n"

-- | Compile each `.ll` object and any **referenced** ulib native-foreign `.c` (its `@pvf_<key>` symbol
-- | appears in an emitted `.ll`), then link them with the runtime staticlib into `<output>/app`. An
-- | unreferenced leaf's `.c` is not compiled (the linker would dead-strip it, but compiling it wastes work
-- | and can pull unrelated libs).
link :: forall r. LinkOptions -> Run (ENV + PROC + FS + LOG + EXCEPT String + r) Unit
link opts = do
  macos <- hostIsMacos
  let
    -- GNU ld's `--gc-sections` strips only when each function/datum is in its own section; ld64
    -- dead-strips per symbol and needs nothing (ADR-0072 §3).
    sectionFlags = if macos then [] else [ "-ffunction-sections", "-fdata-sections" ]
    deadStrip = if macos then "-Wl,-dead_strip" else "-Wl,--gc-sections"
    tags = map (\i -> "mod_" <> show i) (0 .. (opts.moduleCount - 1)) <> [ "entry" ]
  llTexts <- traverse readLl tags
  objs <- traverse (compileObj sectionFlags) tags
  let
    referencedSyms = Set.fromFoldable (Array.concatMap pvfSymbolsIn llTexts)
    -- The referenced symbols in a **local workspace module's** namespace (ADR-0091 §2) — the app FFI keys we
    -- own the exactly-one-provider check for. A project native leaf is referenced iff it is used (its
    -- eta lowers a `@pvf_`), so unlike dependency/runtime leaves — whose dead etas the linker's
    -- `--gc-sections`/`-dead_strip` removes — an app symbol reaching here is genuinely to be provided.
    appSyms = Array.filter inAppNamespace (Set.toUnfoldable referencedSyms)
  -- ulib providers (ADR-0073): the referenced `ulib.json` `.c`, deduped by source path (a manifest may
  -- map many keys to one `.c`; compiling it once per key would multiply-define its `pvf_` symbols).
  foreignSources <- loadForeignSources opts.ulibDir
  let
    referencedUlib = Array.filter
      (\(Tuple key _) -> Set.member (mangleForeign key) referencedSyms)
      (Map.toUnfoldable foreignSources :: Array (String /\ FilePath))
    ulibSyms = Set.fromFoldable (map (\(key /\ _) -> mangleForeign key) referencedUlib)
    bySource =
      Map.toUnfoldable (Map.fromFoldableWith (<>) (map (\(key /\ src) -> src /\ [ key ]) referencedUlib))
        :: Array (FilePath /\ Array String)
  ulibObjs <-
    if Array.null bySource then pure []
    else do
      inc <- resolveInclude
      traverse (compileForeign sectionFlags inc) (Array.mapWithIndex Tuple bySource)
  -- app providers (ADR-0091): C siblings XOR one Rust crate. This yields the runtime archive to link —
  -- the plain staticlib, or (under `--rust-ffi`) the bundle that folds a fresh runtime rlib (§Addendum) —
  -- plus extra objects.
  Tuple archive extraObjs <- case opts.rustFfiDir of
    Nothing -> do
      rt <- resolveRuntimeLib opts.runtimeLib
      appCObjs <- compileAppCProviders sectionFlags referencedSyms
      -- Provider map (ADR-0091 §1/§4), scoped to app keys: audit exported symbols, require exactly one.
      when (not (Array.null appSyms)) do
        runtimeSyms <- nmDefinedPvf rt
        appCSets <- traverse nmDefinedPvf appCObjs
        enforceAppProviders appCClass appSyms runtimeSyms ulibSyms (Array.foldl Set.union Set.empty appCSets)
      pure (Tuple rt appCObjs)
    Just crateDir -> do
      -- `--runtime-lib` names a *staticlib* that app-Rust never links — the bundle folds a fresh runtime
      -- rlib built from the crate (`$PURVASM_RT_CRATE`). Reject it so it cannot silently disagree with the
      -- runtime that actually ships in the bundle.
      when (isJust opts.runtimeLib) $ throw
        "--runtime-lib is not used with --rust-ffi: the runtime is built from its crate. Set $PURVASM_RT_CRATE instead."
      bundle <- resolveAppRust appSyms ulibSyms crateDir
      pure (Tuple bundle [])
  linkExe archive deadStrip objs (ulibObjs <> extraObjs)
  where
  inAppNamespace s =
    any (\m -> isJust (String.stripPrefix (Pattern ("pvf_" <> escapeIdent m.key <> "_2e")) s)) opts.appModules

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

  -- Discover + compile the referenced app-C sibling `.c` providers (ADR-0091 §2).
  compileAppCProviders sectionFlags referencedSyms = do
    candidates <- appCCandidates referencedSyms
    if Array.null candidates then pure []
    else do
      inc <- resolveInclude
      traverse (compileAppC sectionFlags inc) candidates

  -- The app-Rust path (ADR-0091 §Addendum / ADR-0078 §5): reject an ambiguous co-located `.c`, bundle the
  -- crate with a fresh runtime rlib, audit the bundle's exports, and return the **bundle** archive to link
  -- in place of the plain staticlib. The runtime-shadow collision is caught by the count audit below (a
  -- shadowed key is then defined by both the runtime rlib and the crate → count > 1), on the *actual*
  -- bundled runtime — never a separate prebuilt staticlib that could disagree with `$PURVASM_RT_CRATE`.
  resolveAppRust appSyms ulibSyms crateDir = do
    -- C-xor-Rust is a workspace policy: **any** workspace module with a sibling `.c` alongside --rust-ffi is
    -- mixing FFI languages — scanned in full, not referenced-only (an unused C sibling still signals the mix).
    ambiguous <- appCSiblings
    when (not (Array.null ambiguous)) $ throw
      ( "ambiguous foreign FFI: --rust-ffi " <> crateDir
          <> " was given, but these workspace modules have a sibling `.c`: "
          <> String.joinWith ", " (map _.key ambiguous)
      )
    crateAbs <- FS.resolvePath [] crateDir
    nm <- crateName crateAbs
    rtCrate <- resolveRuntimeCrate
    bundle <- buildBundle { workDir: opts.buildDir, runtimeCrate: rtCrate, crateDir: crateAbs, crateNm: nm }
    -- Bundle nm is the audit *substrate* (ADR-0078 §5 / boot `audit_bundle`): each expected app key must be
    -- defined by EXACTLY ONE bundle member — a **count**, not set membership: 0 = the crate did not export
    -- it, > 1 = two members (the crate and the runtime rlib, or two crates) define it, which archive member
    -- selection would resolve silently at the final link.
    bundleList <- nmDefinedPvfList bundle
    let
      countIn sym = Array.length (Array.filter (_ == sym) bundleList)
      auditProblem sym = case countIn sym of
        0 -> Just (Fmt.fmt @"no native provider for {key}: {hint}" { key: demangleKey sym, hint: appRustClass.zeroHint (demangleKey sym) })
        1 -> Nothing
        n -> Just (Fmt.fmt @"{key} is defined {n} times in the --rust-ffi bundle (a Rust foreign shadowing the runtime or another leaf)" { key: demangleKey sym, n: show n })
      -- app-vs-ulib exactly-one (runtime-shadow is the count audit above): an app key a ulib `.c` also provides.
      ulibDup sym = if Set.member sym ulibSyms then Just (Fmt.fmt @"{key} has multiple providers: ulib + --rust-ffi crate" { key: demangleKey sym }) else Nothing
      problems = Array.mapMaybe auditProblem appSyms <> Array.mapMaybe ulibDup appSyms
    when (not (Array.null problems))
      $ throw ("native provider map:\n  " <> String.joinWith "\n  " problems)
    pure bundle

  -- The two app-provider classes' labels + zero-provider hints for `enforceAppProviders`.
  appCClass =
    { label: "app-C sibling .c"
    , zeroHint: \key -> "add a sibling `.c` exporting PVF_EXPORT(" <> identOf key <> ") (or pass --rust-ffi)"
    }
  appRustClass =
    { label: "--rust-ffi crate"
    , zeroHint: \key -> "the --rust-ffi crate does not export it — add `#[pv_foreign(module = \"…\", name = \"" <> identOf key <> "\")]`"
    }

  -- Every local workspace module (ADR-0091 §2, non-`.spago`) with an **existing** sibling `.c` — the
  -- C-xor-Rust ambiguity scan (all of them, not referenced-only). `{ key, cPath }`.
  appCSiblings =
    Array.filterA (\c -> FS.exists c.cPath)
      ( Array.mapMaybe
          (\m -> (\stem -> { key: m.key, cPath: stem <> ".c" }) <$> String.stripSuffix (Pattern ".purs") m.sourcePath)
          opts.appModules
      )

  -- The subset of `appCSiblings` whose namespace a referenced symbol falls in — the referenced-only
  -- providers to actually compile.
  appCCandidates referencedSyms =
    Array.filter (\c -> moduleReferenced referencedSyms c.key) <$> appCSiblings

  moduleReferenced referencedSyms key =
    let
      prefix = "pvf_" <> escapeIdent key <> "_2e"
    in
      any (isJust <<< String.stripPrefix (Pattern prefix)) (Set.toUnfoldable referencedSyms :: Array String)

  -- Compile a workspace module `M`'s sibling `.c` with `-DPVF_MODULE=<escapeIdent(M)>` (ADR-0091 §2), so a
  -- provider naming the leaf via `PVF_EXPORT(foo)` exports `pvf_<escapeIdent("M.foo")>`.
  compileAppC sectionFlags inc c = do
    obj <- FS.joinPath [ opts.buildDir, "appc_" <> escapeIdent c.key <> ".o" ]
    runClang (Fmt.fmt @"app foreign {key} ({cPath}) (clang -c)" { key: c.key, cPath: c.cPath })
      ([ "-c", "-O2", "-DPVF_MODULE=" <> escapeIdent c.key ] <> sectionFlags <> [ "-I" <> inc, c.cPath, "-o", obj ])
    pure obj

  -- Exactly one provider per referenced app native key (ADR-0091 §1) across `{runtime, ulib, <app class>}`;
  -- a zero/duplicate is a key-named error (the `appClass` supplies the class label + the zero-provider hint),
  -- so the app author sees "add a sibling `.c`" / "the crate does not export …", never a raw linker symbol.
  enforceAppProviders appClass appSyms runtimeSyms ulibSyms appProvSyms =
    let
      classesOf sym =
        (if Set.member sym runtimeSyms then [ "runtime" ] else [])
          <> (if Set.member sym ulibSyms then [ "ulib" ] else [])
          <> (if Set.member sym appProvSyms then [ appClass.label ] else [])
      check sym = case classesOf sym of
        [] -> Just (Fmt.fmt @"no native provider for {key}: {hint}" { key: demangleKey sym, hint: appClass.zeroHint (demangleKey sym) })
        [ _ ] -> Nothing
        cs -> Just (Fmt.fmt @"{key} has multiple providers: {cs}" { key: demangleKey sym, cs: String.joinWith " + " cs })
      problems = Array.mapMaybe check appSyms
    in
      when (not (Array.null problems))
        $ throw ("native provider map:\n  " <> String.joinWith "\n  " problems)

  -- The last `.`-segment of a qualified key, for the `PVF_EXPORT(ident)` hint.
  identOf key = fromMaybe key (Array.last (String.split (Pattern ".") key))

  linkExe rt deadStrip objs extraObjs = do
    exe <- FS.joinPath [ opts.output, "app" ]
    -- `-lm` for libm calls a native foreign `.c` may emit; harmless with none.
    runClang "link" ([ deadStrip ] <> objs <> extraObjs <> [ rt, "-lm", "-o", exe ])
    Log.info $ Fmt.fmt @"✓ Linked native executable → {exe}" { exe }

  runClang label args = Proc.exec "clang" args >>= case _ of
    Right _ -> pure unit
    Left e -> throw (Fmt.fmt @"{label} failed: {e}" { label, e })
