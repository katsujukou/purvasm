-- | `purvasm foreign-sigs`: dump the reconstructed foreign surface (ADR-0080) of an entry's
-- | closure as deterministic JSON on stdout — the Level-2 half of the §2 consistency
-- | differential (`tools/foreign-sigs-diff.sh` joins it against boot's registry lookup).
-- |
-- | **Transitional (ADR-0080 §2).** This command exists to cross-check the Level-2
-- | reconstruction against boot's hand-maintained registry *while boot is still in the build
-- | path* (the frozen golden reference). Once the native-codegen port retires boot, the
-- | differential loses its second source and this command — together with boot's
-- | `foreign-sig-dump` and `tools/foreign-sigs-diff.sh` — is scaffolding to remove. The shapes'
-- | real consumers (the port, diagnostics) call `ForeignSigs.moduleForeignSigs` directly and do
-- | not go through this dump.
module Purvasm.CLI.ForeignSigsCmd where

import Prelude

import ArgParse.Basic (ArgParser, fromRecord)
import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Console as Console
import Purvasm.CLI.Build (depOrder, loadClosure)
import Purvasm.CLI.Effect.Env (ENV)
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.ForeignSigs (ForeignSigMap)
import Purvasm.CLI.ForeignSigs as ForeignSigs
import Purvasm.CLI.Ulib (requireUlibDir)
import Run (EFFECT, Run, liftEffect)
import Run.Except (EXCEPT)
import Type.Row (type (+))

type Options =
  { corefnDir :: FilePath
  , entryModule :: String
  }

options :: ArgParser Options
options = fromRecord
  { corefnDir:
      ArgParser.argument [ "--corefn-dir" ]
        "Path to the PureScript compiler's output directory.\n\
        \Defaults to './output'."
        # ArgParser.default "output"
  , entryModule:
      ArgParser.argument [ "--entry" ]
        "Entry module whose import closure is scanned. Defaults to `Main`."
        # ArgParser.default "Main"
  }

cmd :: forall r. Options -> Run (ENV + LOG + FS + EXCEPT String + EFFECT + r) Unit
cmd opts = do
  ulibDir <- requireUlibDir
  mods <- loadClosure ulibDir opts.corefnDir opts.entryModule
  env <- ForeignSigs.loadEnv { ulibDir, corefnDir: opts.corefnDir }
  -- Resolve each module independently (ADR-0033), then union for the whole-program dump — the
  -- aggregation is only at this tool's output boundary, not in the resolution.
  perModule <- traverse (ForeignSigs.moduleForeignSigs env) (depOrder mods)
  let sigs = Array.foldl Map.union Map.empty perModule :: ForeignSigMap
  -- Deterministic by construction: `Map.toUnfoldable` yields keys in ascending order.
  let
    b v = if v then "true" else "false"
    entry (key /\ shape) =
      "  " <> show key <> ": {\"arity\": " <> show shape.arity
        <> ", \"vsat\": "
        <> b shape.vsat
        <> ", \"retVsat\": "
        <> b shape.retVsat
        <> "}"
    body = joinWith ",\n" (map entry (Map.toUnfoldable sigs :: Array _))
  liftEffect $ Console.log ("{\n" <> body <> "\n}")
