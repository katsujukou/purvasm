-- | The `--emit-ir <module>` trace hooks (ADR-0087 §3.1), shared by `purvasm build` (native) and
-- | `purvasm run` (VM): for the named module, bracket the driver's optimiser fixpoint with
-- | `onEnterOptimizeIter` (the pre-optimised fixpoint input) / `onContinueOptimizeIter` (each round's
-- | result) / `onLeaveOptimizeIter` (the converged module), appending each round's pretty-printed ANF
-- | to a host `Ref` buffer (a `purs-backend-es --trace-rewrite` analogue), then flush it to
-- | `<workdir>/<module>.ir` when the module finishes (`onCleanUp`). A *trace*, not a stop — the build
-- | runs to completion. Under `--no-opt` the fixpoint never runs, so the trace records that. Without
-- | `--emit-ir`, the no-op hooks.
module Purvasm.CLI.EmitIr (irHooks) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String.Common (joinWith)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Fmt as Fmt
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Log as Log
import Purvasm.Compiler (CompilerActionHooks, defaultHooks)
import Purvasm.Compiler.MiddleEnd.ANF.Pretty (printModuleAnf)
import Run (EFFECT, Run, liftEffect)
import Type.Row (type (+))

irHooks
  :: forall r
   . Maybe String
  -> FilePath
  -> Ref (Array String)
  -> CompilerActionHooks (Run (LOG + FS + EFFECT + r))
irHooks emitIr workdir irBuf = case emitIr of
  Nothing -> defaultHooks
  Just target -> defaultHooks
    { onEnterOptimizeIter = \am -> appendFor target am "pre-optimised (fixpoint input)"
    , onContinueOptimizeIter = \n am -> appendFor target am ("round " <> show n)
    , onLeaveOptimizeIter = \am -> appendFor target am "converged"
    , onCleanUp = \name -> when (name == target) (flush target)
    }
  where
  appendFor target am label =
    when (am.name == target) do
      let chunk = "=== " <> label <> " ===\n" <> printModuleAnf am.name am.decls
      liftEffect (Ref.modify_ (\cs -> Array.snoc cs chunk) irBuf)

  flush target = do
    chunks <- liftEffect (Ref.read irBuf)
    liftEffect (Ref.write [] irBuf)
    irPath <- FS.joinPath [ workdir, target <> ".ir" ]
    let body = if Array.null chunks then "(no optimiser rounds — built with --no-opt)\n" else joinWith "\n\n" chunks
    FS.writeText irPath body
    Log.info $ Fmt.fmt @"  traced ANF IR → {target}.ir" { target }
