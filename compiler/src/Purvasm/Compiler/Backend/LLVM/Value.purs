-- | The value-token kernel (ADR-0105 §6.2, 2b-0 round 2): [`Val`]'s constructors are PRIVATE —
-- | outside this module a token can neither be forged (wrapping a stale SSA as an epoch-immune
-- | raw) nor unwrapped (extracting the bare operand text and re-interpolating it after a
-- | safepoint). What IS exported:
-- |
-- | * [`vImm`] — the raw-word smart constructor, which REJECTS `%`-prefixed text: an SSA
-- |   register can never be epoch-immune, so immediates/label-ids/tags are constructible and
-- |   laundering is not;
-- | * [`verifyAt`]/[`mintAt`] — the PURE epoch primitives the `Monad` renderers wrap with the
-- |   tracked epoch. They are audit-caged (`tools/seam-audit.sh`: `Monad.purs` only): calling
-- |   them with a fabricated epoch is the remaining spoof surface PureScript's module system
-- |   cannot type away, so the audit pins their call sites instead;
-- | * [`vRootedLocal`]/[`vRootedGlobal`] — the `Rooted` arm's constructors (ADR-0105 §6.4, the
-- |   2b-2 reload cache): a rooted token names its root SLOT (a frame handle or a global's
-- |   `$root` cell), never an SSA value — its current value is materialised lazily by the
-- |   renderer-owned reload in `Monad.useVal` (cache hit → reuse, miss → reload). Audit-caged
-- |   to `Emit.purs` (the only binding-realisation reader);
-- | * [`rootedSrc`] — the `Rooted` projection the renderer resolves through (audit-caged to
-- |   `Monad.purs`; the slot text is raw metadata — an index register or a symbol — so this is
-- |   not an operand-text escape);
-- | * [`keyOf`] — the identity projection for binding bookkeeping (audit-caged to
-- |   `Types.purs`, which stamps a comparison key at bind time; never an operand);
-- | * [`unsafeTestVal`] — the test-only forge (the `unsafe` naming convention; the audit pins
-- |   it to zero uses under `src`).
module Purvasm.Compiler.Backend.LLVM.Value
  ( Val
  , RootSrc(..)
  , vImm
  , vRootedLocal
  , vRootedGlobal
  , rootedSrc
  , rootSrcKey
  , verifyAt
  , mintAt
  , keyOf
  , unsafeTestVal
  , unsafeValText
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Partial.Unsafe (unsafeCrashWith)

data Val
  = VRaw String
  | VFresh { ssa :: String, epoch :: Int }
  | VRooted RootSrc

-- | Where a rooted token's current value lives (ADR-0105 §6.4): a transient frame slot named
-- | by its raw-index handle register, or a top-level global's `@<mangle>$root` cell (whose
-- | stable index the reload loads first). The slot text is the token's identity — the cache
-- | key pin (§6.4 d): shadowing mints a different handle and therefore misses, never aliases.
data RootSrc
  = LocalSlot String
  | GlobalSlot String

-- | An epoch-immune raw word. The ONLY production inputs are signed decimal immediates
-- | (scalar literals, label ids, ctor tags), so the constructor validates that WHOLE grammar —
-- | optional `-`, then one or more digits, nothing else (round 3: a prefix check alone let
-- | ` %t9` launder a stale SSA register behind leading whitespace).
vImm :: String -> Val
vImm s =
  let
    digits = case toCharArray s of
      [ '-' ] -> []
      cs -> case Array.uncons cs of
        Just { head: '-', tail } -> tail
        _ -> cs
    isDigit c = c >= '0' && c <= '9'
  in
    if not (Array.null digits) && Array.all isDigit digits then VRaw s
    else unsafeCrashWith ("Backend.LLVM.Value.vImm: not a signed decimal immediate (ADR-0105 §6.2): " <> s)

-- | The `Rooted` arm's slot, when the token is one — the projection `Monad`'s renderer-owned
-- | reload resolves through (audit-caged there; a rooted token has NO SSA text to verify, so
-- | the pure kernel below never accepts it).
rootedSrc :: Val -> Maybe RootSrc
rootedSrc = case _ of
  VRooted src -> Just src
  _ -> Nothing

-- | A rooted token over a transient frame slot (audit-caged to `Emit`).
vRootedLocal :: String -> Val
vRootedLocal = VRooted <<< LocalSlot

-- | A rooted token over a global's `$root` cell (audit-caged to `Emit`).
vRootedGlobal :: String -> Val
vRootedGlobal = VRooted <<< GlobalSlot

-- | Verify a token against an epoch and yield its operand text — the kernel primitive under
-- | `Monad`'s tracked-epoch renderers (audit-caged there; see the module preamble). A rooted
-- | token crashes: its value is materialised only by the renderer-owned reload (`Monad.useVal`),
-- | never by the pure verifier — reaching here means a consumption path skipped the resolver.
verifyAt :: Int -> Val -> String
verifyAt e = case _ of
  VRaw s -> s
  VFresh f ->
    if f.epoch == e then f.ssa
    else unsafeCrashWith
      ( "Backend.LLVM.Value.verifyAt: stale value token (ADR-0105 §6.2 — read/use separated by a safepoint): "
          <> f.ssa
          <> " valid at epoch "
          <> show f.epoch
          <> ", required "
          <> show e
      )
  VRooted src -> unsafeCrashWith
    ("Backend.LLVM.Value.verifyAt: rooted token reached the pure verifier (ADR-0105 §6.4 — the renderer-owned reload is the only consumption path): " <> rootSrcKey src)

-- | Mint a token for an SSA value that just became valid at `e` (audit-caged to `Monad`).
mintAt :: Int -> String -> Val
mintAt e ssa = VFresh { ssa, epoch: e }

-- | A root source's identity text — the §6.4(d) cache key (a handle register vs a `$root`
-- | symbol live in disjoint namespaces, so one map serves both).
rootSrcKey :: RootSrc -> String
rootSrcKey = case _ of
  LocalSlot h -> h
  GlobalSlot sym -> sym

-- | The token's identity text WITHOUT verification — bookkeeping only (the bind-time
-- | comparison keys `Types` stamps); the renderers are the only emission path. A rooted
-- | token's identity is its SLOT, matching the cache key.
keyOf :: Val -> String
keyOf = case _ of
  VRaw s -> s
  VFresh f -> f.ssa
  VRooted src -> rootSrcKey src

-- | Forge a token for unit-test goldens (`%`-named operands at epoch 0). The `unsafe` prefix
-- | marks the §6.2 invariant it can break; the audit pins it to zero uses under `src`.
unsafeTestVal :: String -> Val
unsafeTestVal ssa = VFresh { ssa, epoch: 0 }

-- | Project a token's operand text for unit-test ASSERTIONS (result-temp goldens) — the
-- | extraction escape the production surface deliberately lacks; the audit pins it to zero
-- | uses under `src`.
unsafeValText :: Val -> String
unsafeValText = keyOf
