-- | The LLVM emitter's mutable state (`Ctx`) and its `Codegen` monad. boot's `codegen_llvm.ml` threads
-- | a single `ctx` of mutable ref cells + `Buffer.t`s; here that is a `State Ctx` over an immutable
-- | record (maintainer decision), which keeps codegen pure (like `compileModule`) and testable.
-- |
-- | Deterministic emission — the L2-owned goldens and the ADR-0104 §2 stage fixpoint compare emitted
-- | text — depends entirely on emission order and the counter discipline, so those invariants are
-- | pinned here, exactly as boot's `ctx` had them:
-- |
-- | * `ssa` resets to 0 **per function** (`beginFn`); `lbl`/`fns`/`strs` are **module-global monotonic**
-- |   (never reset) — so a label/lifted-fn number depends on the whole module's emission order.
-- | * `fresh`/`freshLabel`/`freshFn` **pre-increment**, so the first SSA temp is `%t1`.
-- | * buffers are reversed `List String` of lines (each `emit` conses — O(1), ADR-0049); a buffer
-- |   renders as every line followed by `"\n"`, reproducing boot's `Buffer` byte-for-byte (an empty
-- |   buffer renders `""`). The render is a single `joinWith` (O(n)), not a fold (O(n^2)).
module Purvasm.Compiler.Backend.LLVM.Monad
  ( Ctx
  , Codegen
  , MakeCxOptions
  , makeCx
  , runCodegen
  , execCodegen
  , fresh
  , freshLabel
  , freshFn
  , emit
  , unsafeEmitRawCall
  , emitModule
  , emitDefine
  , unsafeEmitRawModule
  , FnBody
  , renderFnBody
  , emitStringConstant
  , containsCallText
  , currentEpoch
  , bumpEpoch
  , unsafeUseVal
  , unsafeMintFresh
  , mintParam
  , mintEnvWord
  , mintCloWord
  , mintLoad
  , emitGuestStore
  , emitGuestRet
  , emitPayloadAshr
  , emitLowBitAnd
  , emitGuestSwitch
  , PhiIncoming
  , closeHopArm
  , armIncomingAt
  , armIncomingClosing
  , emitPhi
  , beginFn
  , takeFn
  , renderBuffer
  , renderChunks
  , forA
  , forA_
  , forWithIndexA
  , foldA
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (State, execState, runState)
import Control.Monad.State.Class (modify_, state)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith)
import Data.String as String
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.Backend.LLVM.Mangle (escapeStringBytes)
import Purvasm.Compiler.Backend.LLVM.Value (Val, mintAt, verifyAt)
import Data.Tuple (Tuple(..))
import Purvasm.Compiler.Backend.LLVM.Types (FnInfo, Lifted, SelfCtx)

-- | The emitter state (boot's `ctx`). `fn` is a reversed **line** buffer (each `emit` is one line);
-- | `md`/`globals` are reversed **chunk** buffers (each write is a pre-formatted raw string carrying
-- | its own newlines — a whole `define` block or a `@…` global line), matching boot's `emit` (→ `fn`)
-- | vs `Buffer.add_string` (→ `md`/`globals`). The counters and reference/foreign/cross-module sets are
-- | the byte-identity-relevant state; `inlineAbi` selects the release inline ABI fast paths vs the
-- | `--debug` entry-call IR (ADR-0079).
type Ctx =
  { md :: List String -- ^ the whole module: raw chunks (reversed)
  , globals :: List String -- ^ module-level byte constants: raw chunks (reversed)
  , fn :: List String -- ^ the current function body: lines (reversed)
  , ssa :: Int
  , lbl :: Int
  , fns :: Int -- ^ lifted-function counter
  , strs :: Int -- ^ string-constant counter
  , pending :: List Lifted -- ^ lambdas to emit (LIFO)
  , gkeys :: Set String -- ^ top-level qualified keys (referenced as `@<mangle>$root`)
  , foreignArity :: Map String Int -- ^ native-leaf key → closure arity (ADR-0073/0080, from FSR)
  , externs :: Set String -- ^ referenced globals not defined here
  , foreigns :: Set String -- ^ referenced native foreign keys
  , gfns :: Map String FnInfo -- ^ this module's own top-level function bindings
  , xfns :: Map String FnInfo -- ^ the program's cross-module export surface (ADR-0077 §2)
  , xdecls :: Map String Int -- ^ referenced cross-module direct entries (`$d` symbol → arity)
  , selfCtx :: Maybe SelfCtx -- ^ the binding whose lambda is being emitted
  , inDirect :: Boolean -- ^ emitting a `tailcc` direct entry (`%env` exists, `musttail` legal)
  , inlineAbi :: Boolean -- ^ release inline ABI fast paths (ADR-0079); `false` under `--debug`
  , spEpoch :: Int -- ^ ADR-0105 §6.2: the safepoint epoch — an emission-order monotone bumped ONLY by `sp = true` seam/guest emissions (verify-then-bump); value tokens are verified against it at consumption
  , rootAll :: Boolean -- ^ ADR-0105: `true` = root-on-create fallback (init bodies, `LClosure` wrappers); `false` = the activation plan drives rooting
  , crossing :: Set String -- ^ ADR-0105: the activation plan's crossing set (consulted only when `rootAll = false`)
  }

-- | The emitter monad: a pure `State` over `Ctx`. `State` is `MonadRec`, so the deep linear spines
-- | (`Let` chains, the `pending` drain, multi-operand folds) stay stack-safe when written with
-- | `tailRecM` or the [`forA`]/[`foldA`] family below — and **only** then: a sequenced
-- | `Data.Foldable.foldM`/`Data.Traversable.traverse` step is a live host frame per element
-- | (2026-07-16 stack-safety bugfix; see the combinators' doc).
type Codegen = State Ctx

-- | The three knobs boot's `make_cx` takes; the rest of `Ctx` starts empty/zero.
type MakeCxOptions =
  { gkeys :: Set String
  , xfns :: Map String FnInfo
  , foreignArity :: Map String Int
  , inlineAbi :: Boolean
  }

-- | A fresh emitter state: all counters 0, all buffers empty, all reference sets/maps empty
-- | (boot's `make_cx`).
makeCx :: MakeCxOptions -> Ctx
makeCx opts =
  { md: Nil
  , globals: Nil
  , fn: Nil
  , ssa: 0
  , lbl: 0
  , fns: 0
  , strs: 0
  , pending: Nil
  , gkeys: opts.gkeys
  , foreignArity: opts.foreignArity
  , externs: Set.empty
  , foreigns: Set.empty
  , gfns: Map.empty
  , xfns: opts.xfns
  , xdecls: Map.empty
  , selfCtx: Nothing
  , inDirect: false
  , inlineAbi: opts.inlineAbi
  , spEpoch: 0
  , rootAll: true
  , crossing: Set.empty
  }

-- | Run an emission, returning the value and the final state.
runCodegen :: forall a. Ctx -> Codegen a -> Tuple a Ctx
runCodegen ctx m = runState m ctx

-- | Run an emission for its final state only.
execCodegen :: forall a. Ctx -> Codegen a -> Ctx
execCodegen ctx m = execState m ctx

-- | A fresh SSA temporary `%tN` (pre-increment: first is `%t1`).
fresh :: Codegen String
fresh = state \c -> let n = c.ssa + 1 in Tuple ("%t" <> show n) c { ssa = n }

-- | A fresh label `<prefix>N` off the module-global label counter (never reset per function).
freshLabel :: String -> Codegen String
freshLabel prefix = state \c -> let n = c.lbl + 1 in Tuple (prefix <> show n) c { lbl = n }

-- | A fresh lifted-function name `<prefix>N` off the module-global function counter (e.g. `fn_`,
-- | `recfn_`, `susp_`).
freshFn :: String -> Codegen String
freshFn prefix = state \c -> let n = c.fns + 1 in Tuple (prefix <> show n) c { fns = n }

-- | A fresh module-level string-constant name `@.str.N` off the module-global string counter (never
-- | reset per function), matching boot's `Printf.sprintf "@.str.%d" cx.strs`.
freshStrName :: Codegen String
freshStrName = state \c -> let n = c.strs + 1 in Tuple ("@.str." <> show n) c { strs = n }

-- | Does the text contain a call instruction? The line-start is normalised (a leading-space
-- | prepend, plus newline/tab starts inside chunks) so a column-zero `call i64 …` cannot slip
-- | past a naive ` call ` substring test. (String-literal bytes go through
-- | [`emitStringConstant`], never here, so the guard cannot false-positive on guest data.)
containsCallText :: String -> Boolean
containsCallText s =
  String.contains (String.Pattern " call ") (" " <> s)
    || String.contains (String.Pattern "\ncall ") s
    || String.contains (String.Pattern "\tcall ") s

-- | Emit one line into the current function body (boot's `emit`, which appends the line + `'\n'`).
-- | REJECTS call instructions (ADR-0105 §1): every classifiable call must be rendered by the
-- | classified seam (`Backend.LLVM.Safepoint`), so a raw `call` line here is a structural error
-- | caught at emission time, not a convention.
emit :: String -> Codegen Unit
emit line =
  if containsCallText line then
    unsafeCrashWith
      ("Backend.LLVM.Monad.emit: raw call text bypasses the classified seam (ADR-0105 §1); route it through Safepoint: " <> line)
  else unsafeEmitRawCall line

-- | The unchecked line emitter. The `unsafe` prefix marks the seam invariant it can break, not
-- | a type hole: ONLY the classified seam (`Backend.LLVM.Safepoint`, which renders from its row
-- | table) and the one documented ctx-birth line (`pv_runtime_new` in `Program`) may call this —
-- | `tools/seam-audit.sh` (CI) pins that allowlist by file and expected count.
unsafeEmitRawCall :: String -> Codegen Unit
unsafeEmitRawCall line = modify_ \c -> c { fn = line : c.fn }

-- | Append a pre-formatted raw chunk to the module buffer (boot's `Buffer.add_string cx.md`).
-- | REJECTS call text like [`emit`] — an assembled chunk carrying its own `call` lines would
-- | bypass the seam through the module buffer. Guest-code `define` blocks (whose bodies
-- | legitimately carry calls, each already validated line-by-line) go through [`emitDefine`];
-- | the one legitimately call-carrying skeleton chunk (`pv_init_all`'s body) goes through
-- | [`unsafeEmitRawModule`].
emitModule :: String -> Codegen Unit
emitModule chunk =
  if containsCallText chunk then
    unsafeCrashWith
      ("Backend.LLVM.Monad.emitModule: raw call text bypasses the classified seam (ADR-0105 §1); use emitDefine (validated body) or the audited unsafeEmitRawModule: " <> chunk)
  else unsafeEmitRawModule chunk

-- | Assemble a `define` block around a validated function body: the wrapper text (header, and
-- | the fixed `}` footer) is call-checked here, while the [`FnBody`] between them is
-- | validated-by-construction (every line passed [`emit`]'s guard or an audited raw site).
emitDefine :: String -> FnBody -> Codegen Unit
emitDefine header (FnBody body) =
  if containsCallText header then
    unsafeCrashWith
      ("Backend.LLVM.Monad.emitDefine: raw call text in a define header (ADR-0105 §1): " <> header)
  else unsafeEmitRawModule (header <> body <> "}\n\n")

-- | The unchecked module-chunk emitter (see [`unsafeEmitRawCall`] for the `unsafe` contract):
-- | ONLY `Program.emitInitAll`'s assembled `pv_init_all` skeleton may pass call-carrying text —
-- | `tools/seam-audit.sh` pins that allowlist.
unsafeEmitRawModule :: String -> Codegen Unit
unsafeEmitRawModule chunk = modify_ \c -> c { md = chunk : c.md }

-- | Materialise a guest string as a module-level `@.str.N` byte constant — the ONLY
-- | module-globals emitter (boot's `string_constant` global). Takes the RAW guest string only:
-- | the name, byte length and escaped bytes are ALL derived internally (`freshStrName` /
-- | `Mangle.escapeStringBytes`), so no caller-supplied fragment reaches the globals buffer and
-- | the fixed `c"…"` constant shape cannot express instruction text (round-5 closure:
-- | caller-supplied name/escaped parts could have smuggled raw IR past the fixed suffix). The
-- | empty string emits nothing — `Nothing`, boot's early return; callers pass a null pointer.
emitStringConstant :: String -> Codegen (Maybe { name :: String, len :: Int })
emitStringConstant s =
  let
    { escaped, len } = escapeStringBytes s
  in
    if len == 0 then pure Nothing
    else do
      name <- freshStrName
      modify_ \c -> c
        { globals = (name <> " = private unnamed_addr constant [" <> show len <> " x i8] c\"" <> escaped <> "\"\n") : c.globals }
      pure (Just { name, len })

-- | Start a new function body: reset the SSA counter and clear the current-function line buffer.
-- | `lbl`/`fns`/`strs` are deliberately untouched (module-global). The ADR-0105 per-activation
-- | rooting policy resets to the conservative root-on-create fallback (`rootAll = true`) —
-- | `emitFunction` overrides it when an activation plan exists. (Frame state is NOT here: the
-- | open frame is the lexical `Root.FrameToken` the emitters thread.)
beginFn :: Codegen Unit
beginFn = modify_ \c -> c { ssa = 0, fn = Nil, spEpoch = 0, rootAll = true, crossing = Set.empty }

-- | A rendered function body whose every line went through the guarded emitters — the
-- | constructor is private, so call-carrying body text can only re-enter the module buffer via
-- | [`emitDefine`] (never re-injected through the validated [`emitModule`]).
newtype FnBody = FnBody String

-- | The body's text, for the pure final-assembly templates (`entryLl`'s `@main` block).
renderFnBody :: FnBody -> String
renderFnBody (FnBody s) = s

-- | Take the current function body (validated-by-construction) and clear the line buffer (boot
-- | reads `Buffer.contents cx.fn` into a `define` template). The counters are left alone;
-- | `beginFn` resets `ssa` at the next function.
takeFn :: Codegen FnBody
takeFn = state \c -> Tuple (FnBody (renderBuffer c.fn)) c { fn = Nil }

-- | The current safepoint epoch (ADR-0105 §6.2) — for the phi-arm snapshots the branching
-- | emitters record (each phi incoming is verified against the epoch at its source block's
-- | END, not the merge point's: the emission-order monotone over-approximates across
-- | mutually exclusive arms, and the snapshot is the path-sensitive join §6.4 pins).
currentEpoch :: Codegen Int
currentEpoch = state \c -> Tuple c.spEpoch c

-- | Bump the safepoint epoch — called ONLY by the classified seam's `sp = true` renderers and
-- | the guest-call emitters, exactly once per emission, AFTER the emission's operands verified
-- | (verify-then-bump); `tools/seam-audit.sh` pins the call sites. Overflow is fail-closed
-- | (unreachable per function — `beginFn` resets — but an epoch that wrapped would let stale
-- | tokens verify).
bumpEpoch :: Codegen Unit
bumpEpoch = modify_ \c ->
  if c.spEpoch >= 2147483646 then
    unsafeCrashWith "Backend.LLVM.Monad.bumpEpoch: safepoint epoch overflow (fail-closed, ADR-0105 §6.4)"
  else c { spEpoch = c.spEpoch + 1 }

-- | Verify a value token against the CURRENT epoch and yield its operand text — PRIVATE
-- | (2b-0 round 2): a public verify-to-`String` re-opens the read→use gap it exists to close
-- | (hold the string across a bump, then interpolate). The exported surface is the fused
-- | verify+emit renderers below, plus the audit-caged [`unsafeUseVal`] bridge for the seam's
-- | own arg rendering.
useVal :: Val -> Codegen String
useVal v = state \c -> Tuple (verifyAt c.spEpoch v) c

-- | Mint a token at the current epoch — PRIVATE; the exported mints are the shaped wrappers
-- | ([`mintParam`]/[`mintEnvWord`]/[`mintCloWord`]/[`mintLoad`]) and the audit-caged
-- | [`unsafeMintFresh`] (a free-form re-stamp is the laundering primitive).
mintFresh :: String -> Codegen Val
mintFresh ssa = state \c -> Tuple (mintAt c.spEpoch ssa) c

-- | The seam's arg-rendering bridge (ADR-0105 §6.2): verify-to-text, `unsafe`-prefixed because
-- | the yielded `String` must be interpolated into the SAME emission with no intervening bump —
-- | `tools/seam-audit.sh` cages it to `Safepoint`/`Prim` (whose sequences cannot bump before
-- | the interpolation).
unsafeUseVal :: Val -> Codegen String
unsafeUseVal = useVal

-- | The seam's result-mint bridge (post-bump call results, inline-prim results) — audit-caged
-- | to `Safepoint`/`Prim`; anywhere else a free-form mint could re-stamp a stale SSA.
unsafeMintFresh :: String -> Codegen Val
unsafeMintFresh = mintFresh

-- | A function-entry parameter's token (`%pN`, minted at the prologue epoch).
mintParam :: Int -> Codegen Val
mintParam i = mintFresh ("%p" <> show i)

-- | The `%env` word's token (a function-entry value; capture reads and self-calls inherit it).
mintEnvWord :: Codegen Val
mintEnvWord = mintFresh "%env"

-- | The `%clo` word's token (the generic wrapper's closure parameter).
mintCloWord :: Codegen Val
mintCloWord = mintFresh "%clo"

-- | Emit `load i64` from a pointer and mint the loaded value's token — the reload shape
-- | (`abiGet`'s inline tail, the generic wrapper's argument loads).
mintLoad :: String -> Codegen Val
mintLoad ptr = do
  t <- fresh
  emit ("  " <> t <> " = load i64, ptr " <> ptr)
  mintFresh t

-- | Fused verify+emit renderers (ADR-0105 §6.2 round 2): each takes the guest operand as a
-- | token and emits its consuming instruction in one step — there is no window in which a
-- | verified-but-bare `String` can be held across a bump.
emitGuestStore :: Val -> String -> Codegen Unit
emitGuestStore v ptr = do
  vS <- useVal v
  emit ("  store i64 " <> vS <> ", ptr " <> ptr)

emitGuestRet :: Val -> Codegen Unit
emitGuestRet v = do
  vS <- useVal v
  emit ("  ret i64 " <> vS)

-- | `dest = ashr i64 <v>, 1` — the tagged-word payload read.
emitPayloadAshr :: String -> Val -> Codegen Unit
emitPayloadAshr dest v = do
  vS <- useVal v
  emit ("  " <> dest <> " = ashr i64 " <> vS <> ", 1")

-- | `dest = and i64 <v>, 1` — the immediate-bit read (force fast path, ctor dispatch).
emitLowBitAnd :: String -> Val -> Codegen Unit
emitLowBitAnd dest v = do
  vS <- useVal v
  emit ("  " <> dest <> " = and i64 " <> vS <> ", 1")

-- | `switch i64 <v>, label %<default> [ <cases> ]` over a guest scrutinee.
emitGuestSwitch :: Val -> String -> String -> Codegen Unit
emitGuestSwitch v defaultLbl cases = do
  vS <- useVal v
  emit ("  switch i64 " <> vS <> ", label %" <> defaultLbl <> " [ " <> cases <> " ]")

-- | A verified phi incoming (ADR-0105 §6.2 round 2): minted AT ITS ARM'S END — `phiArm` runs
-- | the verification at call time, so the constructor-private carrier can only hold a value
-- | that was fresh when its source block closed (only a `br` separates that from the merge).
-- | `useValAt`-style caller-chosen epochs no longer exist on the public surface.
newtype PhiIncoming = PhiIncoming { ssa :: String, block :: String }

-- | Verify `v` and freeze it as an incoming from `block` — PRIVATE (round 3): a public
-- | freeze does not guarantee the arm actually ENDS at the freeze, so the exported forms fuse
-- | the freeze with the arm's close ([`closeHopArm`]) or with the next arm's start
-- | ([`armIncomingAt`]/[`armIncomingClosing`]) — nothing of the same arm can be emitted
-- | after its incoming is frozen.
phiArm :: String -> Val -> Codegen PhiIncoming
phiArm block v = do
  vS <- useVal v
  pure (PhiIncoming { ssa: vS, block })

-- | Close a value-producing arm through a fresh single-predecessor hop block (the CIf/CCase
-- | idiom): verify+freeze the arm's value, then emit `br %hop`, `hop:`, `br %merge` in one
-- | step — the arm is OVER the moment its incoming exists.
closeHopArm :: { hop :: String, merge :: String } -> Val -> Codegen PhiIncoming
closeHopArm b v = do
  inc <- phiArm b.hop v
  emit ("  br label %" <> b.hop)
  emit (b.hop <> ":")
  emit ("  br label %" <> b.merge)
  pure inc

-- | Freeze an arm's incoming fused with STARTING the next arm's block (the Abi helpers' fast
-- | arm, whose own terminator — the conditional branch — is already emitted): nothing of the
-- | sealed arm can follow, because the next label opens immediately.
armIncomingAt :: { from :: String, startNext :: String } -> Val -> Codegen PhiIncoming
armIncomingAt b v = do
  inc <- phiArm b.from v
  emit (b.startNext <> ":")
  pure inc

-- | Freeze the LAST arm's incoming fused with its closing branch and the merge label — the
-- | phi must be the merge block's first instruction, so an interloping emission between this
-- | and [`emitPhi`] is invalid LLVM by construction (clang rejects it deterministically).
armIncomingClosing :: { from :: String, merge :: String } -> Val -> Codegen PhiIncoming
armIncomingClosing b v = do
  inc <- phiArm b.from v
  emit ("  br label %" <> b.merge)
  emit (b.merge <> ":")
  pure inc

-- | Render `dest = phi i64 [ ssa, %block ], …` from verified incomings and mint the merged
-- | value's token at the merge epoch (sound: each incoming was fresh at its arm's end).
emitPhi :: String -> Array PhiIncoming -> Codegen Val
emitPhi dest incomings = do
  let
    entries = joinWith ", "
      (map (\(PhiIncoming i) -> "[ " <> i.ssa <> ", %" <> i.block <> " ]") (Array.fromFoldable incomings))
  emit ("  " <> dest <> " = phi i64 " <> entries)
  mintFresh dest

-- | Stack-safe per-element sequencing for **data-sized** spines (2026-07-16 stack-safety bugfix):
-- | a sequenced `State` step is a live host frame on the JS backend even inside a right-nested
-- | `do`, so `Data.Traversable.traverse`/`Data.Foldable.foldM` over anything whose length grows
-- | with the source — operand lists, arities, captures, recursive-group widths, case arms, module
-- | binding counts — stacks one frame per element. These `tailRecM` loops do not (`StateT`'s
-- | `tailRecM` delegates to the base monad's flat loop). Element order is left to right, exactly
-- | `traverse`'s. Use the standard combinators only for genuinely bounded spans.
forA :: forall a b. Array a -> (a -> Codegen b) -> Codegen (Array b)
forA xs f = tailRecM go { i: 0, acc: Nil }
  where
  go st = case Array.index xs st.i of
    Nothing -> pure (Done (Array.fromFoldable (List.reverse st.acc)))
    Just x -> f x <#> \b -> Loop { i: st.i + 1, acc: b : st.acc }

forA_ :: forall a b. Array a -> (a -> Codegen b) -> Codegen Unit
forA_ xs f = tailRecM go 0
  where
  go i = case Array.index xs i of
    Nothing -> pure (Done unit)
    Just x -> f x $> Loop (i + 1)

forWithIndexA :: forall a b. Array a -> (Int -> a -> Codegen b) -> Codegen (Array b)
forWithIndexA xs f = tailRecM go { i: 0, acc: Nil }
  where
  go st = case Array.index xs st.i of
    Nothing -> pure (Done (Array.fromFoldable (List.reverse st.acc)))
    Just x -> f st.i x <#> \b -> Loop { i: st.i + 1, acc: b : st.acc }

foldA :: forall a b. (b -> a -> Codegen b) -> b -> Array a -> Codegen b
foldA f z xs = tailRecM go { i: 0, acc: z }
  where
  go st = case Array.index xs st.i of
    Nothing -> pure (Done st.acc)
    Just x -> f st.acc x <#> \acc -> Loop { i: st.i + 1, acc }

-- | Render a reversed **line** buffer (the `fn` body): every line followed by `"\n"`, byte-for-byte
-- | with boot's `Buffer.contents` (an empty buffer renders `""`). A single `joinWith` keeps it O(n).
renderBuffer :: List String -> String
renderBuffer revLines =
  let
    lines = Array.reverse (Array.fromFoldable revLines)
  in
    if Array.null lines then "" else joinWith "\n" lines <> "\n"

-- | Render a reversed **chunk** buffer (`md`/`globals`): raw concatenation, since each chunk carries
-- | its own newlines.
renderChunks :: List String -> String
renderChunks revChunks = joinWith "" (Array.reverse (Array.fromFoldable revChunks))
