-- | The ADR-0105 §2 root-lifetime API: the ONLY module that can emit a `pv_root` (or its
-- | inline fast path) — and, since round 3, the only module that can emit a frame POP at all.
-- | Two tiers, closed differently:
-- |
-- | * **Transient tier** — `rootLocal` requires a [`FrameToken`], minted only by [`openFrame`]:
-- |   possession proves a frame was opened on this emission path. The token is a possession
-- |   WITNESS, not an affine ownership proof — PureScript has no affine types, and emission
-- |   order is not execution order (one activation legitimately emits several `pop` sites, one
-- |   per control path), so "no root after pop" is a per-path property the type system cannot
-- |   carry. What IS structural: `popFrame` is private — every pop is fused with what may
-- |   legally follow it: a path terminator ([`retWith`]/[`musttailWith`]/[`tailcallWith`]),
-- |   the entry teardown ([`entryTeardown`]), or a framed-init epilogue
-- |   ([`emitInitFnFramed`] for `Grec`, [`emitGcafInitEngine`] for `Gcaf` — in both, only
-- |   the wrapper-owned permanent tier follows). Pop-then-anything-else is not expressible
-- |   outside this module.
-- | * **Permanent tier** — there is NO capability value, and the phase order is wrapper-owned:
-- |   the frameless `Gfun` init is a FIXED SHAPE with no body callback at all
-- |   ([`emitGfunInit`]), and a framed init body does not root permanently — it RETURNS its
-- |   candidate(s) and the wrapper ([`emitInitFnFramed`] for `Grec`, the plan-driven
-- |   [`emitGcafInitEngine`] for `Gcaf`) plants the permanent roots only after ITS
-- |   transient frame is popped. The framed wrapper is additionally robust to a body
-- |   that opens (and leaks) extra frames: the pop restores `roots_len` to the WRAPPER's mark,
-- |   subsuming anything the body opened, so the permanent roots still land in the init
-- |   region (pinned by the wrapper-mark golden). `openFrame` call sites are themselves pinned
-- |   by `tools/seam-audit.sh`.
-- |
-- | The emission bodies are the ADR-0079 inline fast paths moved verbatim from `Abi`
-- | (`abi_frame_open`/`abi_pop_frame`/`abi_root` in boot's `codegen_llvm.ml`): under `--debug`
-- | (`inlineAbi = false`) each operation is a single runtime entry call, routed through the
-- | classified seam (`Safepoint` — all four are machinery rows, never guest safepoints).
module Purvasm.Compiler.Backend.LLVM.Root
  ( FrameToken
  , openFrame
  , ensureRooted
  , retWith
  , musttailWith
  , tailcallWith
  , entryTeardown
  , emitGfunInit
  , emitInitFnFramed
  , emitGcafInitEngine
  ) where

import Prelude

import Control.Monad.State.Class (gets)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.Backend.LLVM.Abi (headerField, offRootsCap, offRootsLen)
import Purvasm.Compiler.Backend.LLVM.Mangle (immUnit, mangle)
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, beginFn, emit, emitDefine, emitGuestRet, emitGuestStore, emitRetResolved, forA, forA_, fresh, freshLabel, mintFrameOwner, resolveGuest, snapshotVal, takeFn, touchVal, unsafeEmitChainLabel)
import Purvasm.Compiler.Backend.LLVM.Safepoint (RtArg(..), RtOp(..), emitPreparedMusttail, machineryHandleCall, prepareMusttail, rtCall, rtCallVoid)
import Purvasm.Compiler.Backend.LLVM.Value (FrameOwner, RootSrc(..), RootedVal, Val, mkRootedLocal, rootedFromVal, rootedSrc, sameOwner, vImm)

-- | Witness of THIS activation's open shadow-stack frame, carrying the frame's mark operand
-- | and its two-tier [`FrameOwner`] (ADR-0106 slice 1). Opaque: minted only by [`openFrame`]
-- | (see the module preamble for what the witness does and does not guarantee).
newtype FrameToken = FrameToken { mark :: String, owner :: FrameOwner }

-- | Open a shadow-stack frame, returning its token: inline reads `roots_len`, `--debug` calls
-- | `pv_frame`. The token carries the frame's freshly-minted owner (ADR-0106 slice 1).
openFrame :: Codegen FrameToken
openFrame = do
  owner <- mintFrameOwner
  gets _.inlineAbi >>= case _ of
    -- the mark is a HOST index (roots_len) — the machinery row returns it raw (2b-0 round 2:
    -- no token-unwrap escape exists; handle-returning rows bypass the guest-token surface).
    false -> machineryHandleCall RtFrame [] <#> \m -> FrameToken { mark: m, owner }
    true -> do
      m <- fresh
      lenp <- headerField offRootsLen
      emit ("  " <> m <> " = load i64, ptr " <> lenp)
      pure (FrameToken { mark: m, owner })

-- | Pop the frame back to its mark: inline stores `roots_len`, `--debug` calls `pv_pop_frame`.
-- | PRIVATE — a pop is only ever emitted fused with its legal continuation (module preamble).
popFrame :: FrameToken -> Codegen Unit
popFrame (FrameToken t) = gets _.inlineAbi >>= case _ of
  false -> rtCallVoid RtPopFrame [ I64 t.mark ]
  true -> do
    lenp <- headerField offRootsLen
    emit ("  store i64 " <> t.mark <> ", ptr " <> lenp)

-- | Root a value into the activation's frame — or REUSE its existing slot (ADR-0106 slice 1,
-- | the reload-reroot elision). The branch semantics are total per token arm:
-- |
-- | * `GlobalSlot` → reuse, no frame required (permanent root);
-- | * `LocalSlot` → reuse iff its [`FrameOwner`] equals the passed token's EXACTLY; a
-- |   different owner, or no frame, is fail-closed (a cross-activation/cross-frame token is
-- |   a soundness bug, not a recoverable state);
-- | * `Fresh`/raw → a frame is REQUIRED: root into it and mint the owned `LocalSlot`.
-- |
-- | Returns the BY-TYPE-rooted [`RootedVal`]; the raw-handle form (`rootLocal`) is private.
ensureRooted :: Maybe FrameToken -> Val -> Codegen RootedVal
ensureRooted frame v = case rootedFromVal v, rootedSrc v of
  Just rv, Just (GlobalSlot _) -> pure rv
  Just rv, Just (LocalSlot l) -> case frame of
    Just (FrameToken t)
      | sameOwner t.owner l.owner -> pure rv
      | otherwise -> unsafeCrashWith
          ("Backend.LLVM.Root.ensureRooted: LocalSlot owned by a different frame (ADR-0106 fail-closed): " <> l.handle)
    Nothing -> unsafeCrashWith
      ("Backend.LLVM.Root.ensureRooted: LocalSlot reuse with no open frame (ADR-0106 fail-closed): " <> l.handle)
  _, _ -> case frame of
    Just tok@(FrameToken t) -> do
      h <- rootLocal tok v
      pure (mkRootedLocal h t.owner)
    Nothing -> unsafeCrashWith
      "Backend.LLVM.Root.ensureRooted: transient root with no open frame (ADR-0105: an under-declared may-root lowering recipe)"

-- | Root a transient value into the open frame and return its raw handle — PRIVATE since
-- | ADR-0106 slice 1 (the public form is [`ensureRooted`], which returns the owned token).
rootLocal :: FrameToken -> Val -> Codegen String
rootLocal (FrameToken _) = emitRoot

popIfOpen :: Maybe FrameToken -> Codegen Unit
popIfOpen = case _ of
  Just tok -> popFrame tok
  Nothing -> pure unit

-- | Terminate the current path with `ret`, popping the activation's frame iff it opened one —
-- | the fused pop+terminator (a pop with a live continuation is not expressible through this).
retWith :: Maybe FrameToken -> Val -> Codegen Unit
retWith frame v = do
  -- resolve BEFORE the pop (§6.4): under the Rooted arm this is where a reload would emit,
  -- and the handle must still be live. Emission-identical today (resolution emits nothing).
  r <- resolveGuest v
  popIfOpen frame
  emitRetResolved r

-- | Terminate the current path with a `musttail` direct call (ADR-0076 §3): pop this frame
-- | first — the callee opens its own — with every operand computed before the pop; no safepoint
-- | in between (the ADR-0064 §4 pop-before-`musttail` discipline), then the mandatory `ret`.
musttailWith :: Maybe FrameToken -> { dsym :: String, env :: Val, args :: Array Val } -> Codegen Unit
musttailWith frame c = do
  -- two-phase (§6.4, the pinned 2b-2 blocker): operands resolve/verify BEFORE the pop; after
  -- it only the sealed call renders, then the mandatory ret of its fresh result.
  prepared <- prepareMusttail c
  popIfOpen frame
  r <- emitPreparedMusttail prepared
  emitGuestRet r

-- | Terminate the current path via the trampoline (ADR-0071 §4): stash the pending tail
-- | (`pv_tailcall` — pinned NOT a safepoint), pop this frame, return unit to the trampoline.
tailcallWith :: Maybe FrameToken -> { fv :: Val, argp :: String, nargs :: Int } -> Codegen Unit
tailcallWith frame c = do
  rtCallVoid RtTailcall [ V c.fv, Ptr c.argp, I64 (show c.nargs) ]
  popIfOpen frame
  emitGuestRet (vImm immUnit)

-- | The entry stub's fused epilogue: pop the entry frame, free the runtime, return from
-- | `@main`. Nothing can be emitted between the pop and the process teardown.
entryTeardown :: FrameToken -> Codegen Unit
entryTeardown tok = do
  popFrame tok
  rtCallVoid RtRuntimeFree []
  emit "  ret i32 0"

-- | Emit the frameless `Gfun` `$init` — a FIXED SHAPE fully determined by the key and arity
-- | (build the closed top-level closure, permanent-root it into `@<key>$root`): there is no
-- | body callback at all, so a frameless init cannot open a frame the wrapper would never pop,
-- | nor emit anything else — the round-4 closure of the ADR-0105 §2 phase order for the
-- | frameless shape.
emitGfunInit :: String -> Int -> Codegen Unit
emitGfunInit key arity = do
  beginFn
  addr <- fresh
  emit ("  " <> addr <> " = ptrtoint ptr @" <> mangle key <> " to i64")
  clo <- rtCall RtMakeClosure [ I64 addr, I32 (show arity), V (vImm immUnit) ]
  storePermanentRoots [ Tuple key clo ]
  finishInitFn key

-- | Emit a framed `define void @<mangle name>$init(ptr %ctx)` (the `Grec` shape — `Gcaf`
-- | goes through the plan-driven [`emitGcafInitEngine`] since ADR-0106 slice 2): the
-- | wrapper owns the WHOLE phase order — open the transient frame, run the body (which roots
-- | transiently through the token and returns its permanent-root candidates as raw values read
-- | back before this returns), pop the frame, and only then plant the permanent roots. A
-- | "permanent" handle inside the transient frame is not expressible.
emitInitFnFramed :: String -> (FrameToken -> Codegen (Array (Tuple String Val))) -> Codegen Unit
emitInitFnFramed name body = do
  beginFn
  tok <- openFrame
  pairs <- body tok
  -- read each candidate back into an epoch-checked Fresh token BEFORE the pop (ADR-0105
  -- §6.4): after it the transient slots are dead — the permanent tier's own rooting stores
  -- overwrite that region — so a rooted candidate must not resolve through its slot later
  -- (and a stale one crashes instead of reloading). A GlobalSlot candidate is EXEMPT
  -- (ADR-0106 slice-1 round 2): it is already permanently rooted, so the token survives the
  -- pop as-is and the permanent tier below COPIES its handle instead of re-rooting.
  snapped <- forA pairs
    ( \(Tuple key v) -> case rootedSrc v of
        Just (GlobalSlot _) -> pure (Tuple key v)
        _ -> Tuple key <$> snapshotVal v
    )
  popFrame tok
  storePermanentRoots snapped
  finishInitFn name

-- | The `Gcaf` init ENGINE (ADR-0106 slice 2) — the one internal-callback form under the
-- | fixed-shape public surface `Emit.emitGcafInit(key, Expr)`. The engine owns the WHOLE
-- | phase order for both frame shapes: `framed` (plan says roots exist) opens the transient
-- | frame, runs the body, snapshot-reads the candidate pre-pop (a `GlobalSlot` candidate is
-- | kept as-is for the handle-copy), pops, then plants the permanent root; frameless (the
-- | plan elided every root) runs the body with no token — the body structurally cannot root
-- | (`ensureRooted`'s fresh arm crashes on `Nothing`) — and plants the permanent root
-- | directly. Per the ADR this callback is an AUDIT guarantee, not a structural one: the
-- | `allow0106` table pins its use sites to `Emit.emitGcafInit` exactly (a caller passing
-- | a frame-leaking body would have to appear there).
emitGcafInitEngine :: { key :: String, framed :: Boolean, body :: Maybe FrameToken -> Codegen Val } -> Codegen Unit
emitGcafInitEngine c = do
  beginFn
  if c.framed then do
    tok <- openFrame
    v <- c.body (Just tok)
    snapped <- case rootedSrc v of
      Just (GlobalSlot _) -> pure v
      _ -> snapshotVal v
    popFrame tok
    storePermanentRoots [ Tuple c.key snapped ]
  else do
    v <- c.body Nothing
    storePermanentRoots [ Tuple c.key v ]
  finishInitFn c.key

-- | Plant each `(globalKey, value)` candidate as a permanent init-region handle stored into
-- | its `@<mangle key>$root` global — the ADR-0105 §2 permanent tier, reachable only from
-- | the init wrappers above (`Gfun` fixed shape, the `Grec` framed wrapper, the `Gcaf`
-- | engine — in each, after any transient frame is popped, so the handle survives).
storePermanentRoots :: Array (Tuple String Val) -> Codegen Unit
storePermanentRoots pairs =
  forA_ pairs \(Tuple key v) -> case rootedSrc v of
    -- ADR-0106 GlobalSlot→reuse, applied to the permanent tier (slice-1 round 2): the
    -- candidate IS another global's value, already held by a permanent slot whose index is
    -- stable — the new `$root` aliases that slot by copying the INDEX. ABI soundness: a
    -- reader loads its `$root` index then dereferences the slot (`pv_get`/inline), so a
    -- shared index dereferences identically; init-region slots are never popped and CAF
    -- values are never re-stored after their init. No root block, no reload.
    Just (GlobalSlot sym) -> do
      h <- fresh
      emit ("  " <> h <> " = load i64, ptr " <> sym)
      emit ("  store i64 " <> h <> ", ptr @" <> mangle key <> "$root")
    _ -> do
      h <- emitRoot v
      emit ("  store i64 " <> h <> ", ptr @" <> mangle key <> "$root")

finishInitFn :: String -> Codegen Unit
finishInitFn name = do
  emit "  ret void"
  text <- takeFn
  emitDefine ("define void @" <> mangle name <> "$init(ptr %ctx) {\nentry:\n") text

-- | The rooting emission (private — reachable only through `rootLocal` and the init wrappers'
-- | post-body permanent tier). Inline: the in-capacity store is the fast path (a 4-block
-- | `rchk`/`rfast`/`rslow`/`rdone` with a phi); `len == cap` falls to `pv_root`, which grows
-- | and returns the same bare-index handle. `--debug` is a single `pv_root`.
emitRoot :: Val -> Codegen String
emitRoot v = gets _.inlineAbi >>= case _ of
  false -> machineryHandleCall RtRoot [ V v ]
  true -> do
    -- resolve the operand while emission is still legal (ADR-0105 §6.4): its consumptions
    -- below sit inside this chain's arms (the rfast store, the rslow pv_root), where a
    -- rooted miss's reload would not dominate the join.
    touchVal v
    chk <- freshLabel "rchk"
    fast <- freshLabel "rfast"
    slow <- freshLabel "rslow"
    done <- freshLabel "rdone"
    emit ("  br label %" <> chk)
    unsafeEmitChainLabel chk
    lenp <- headerField offRootsLen
    len <- fresh
    emit ("  " <> len <> " = load i64, ptr " <> lenp)
    cap <- fresh
    capAddr <- headerField offRootsCap
    emit ("  " <> cap <> " = load i64, ptr " <> capAddr)
    full <- fresh
    emit ("  " <> full <> " = icmp eq i64 " <> len <> ", " <> cap)
    emit ("  br i1 " <> full <> ", label %" <> slow <> ", label %" <> fast)
    unsafeEmitChainLabel fast
    base <- fresh
    emit ("  " <> base <> " = load ptr, ptr %ctx")
    slot <- fresh
    emit ("  " <> slot <> " = getelementptr i64, ptr " <> base <> ", i64 " <> len)
    emitGuestStore v slot
    len1 <- fresh
    emit ("  " <> len1 <> " = add i64 " <> len <> ", 1")
    emit ("  store i64 " <> len1 <> ", ptr " <> lenp)
    emit ("  br label %" <> done)
    unsafeEmitChainLabel slow
    -- the handle is a raw index (both arms; `pv_root` returns the same bare index).
    hs <- machineryHandleCall RtRoot [ V v ]
    emit ("  br label %" <> done)
    unsafeEmitChainLabel done
    h <- fresh
    emit ("  " <> h <> " = phi i64 [ " <> len <> ", %" <> fast <> " ], [ " <> hs <> ", %" <> slow <> " ]")
    pure h
