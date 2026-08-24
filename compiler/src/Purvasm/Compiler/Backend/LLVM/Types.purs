-- | The pure data types the LLVM backend threads through emission (ADR-0072/0076/0077), transcribed
-- | from boot's `codegen_llvm.ml` (`env_src`, `fn_info`, `lifted`/`lifted_body`, `call_fact`, `gdef`,
-- | `split_output`, `env_entry`/`env`). The mutable emitter state (`Ctx`) and the `Codegen` monad live
-- | in `Purvasm.Compiler.Backend.LLVM.Monad`; this module is only the type vocabulary.
-- |
-- | ADR-0113 §1/§2 adds the bind-time vocabulary: [`BindOrigin`] (WHERE a binding was made, total over
-- | every bind site) and [`LocalFact`] (WHAT was known there), which together replace the old
-- | `knownFn` field. The module deliberately does NOT export `CapturableFact`'s constructor — see
-- | [`capturableFact`].
-- |
-- | **The export list is load-bearing, not documentation.** [`CapturableFact`] is exported as a TYPE
-- | ONLY — its constructor stays inside this module so [`capturableFact`] is the single, fail-closed
-- | way to make one (ADR-0113 §2). With an open module the `SSelf` exclusion would be a convention;
-- | with this list it is a type.
module Purvasm.Compiler.Backend.LLVM.Types
  ( EnvSrc(..)
  , FnInfo
  -- the ADR-0113 §2 capture boundary: the type, its two total projections, and NOT its constructor
  , CapturableFact
  , capturableFact
  , unFact
  , CandidateKind(..)
  , candidateKinds
  , candidateKindName
  , CandidateFact
  , Lifted(..)
  , Capture
  , LiftedBody(..)
  , CallFact(..)
  , Gdef(..)
  , BindingV(..)
  , BindOrigin(..)
  , bindOrigins
  , bindOriginName
  , LocalFact(..)
  , EnvEntry
  , activeFn
  , candidateOf
  , Env
  , bindVar
  , bindDirectVar
  , bindFnVar
  , bindDirectFnVar
  , lookupEnv
  , SelfCtx
  , SplitOutput
  ) where

import Prelude

import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Tuple (Tuple(..), snd)
import Purvasm.Compiler.Backend.LLVM.Value (RootedVal, Val, keyOf, rootedVal)
import Purvasm.Compiler.MiddleEnd.ANF (Expr)

-- | How a direct call site obtains the callee's env word (ADR-0076 §2).
data EnvSrc
  = SSelf -- ^ the enclosing direct entry's own `%env` (a self-call)
  | SSentinel -- ^ the no-capture immediate sentinel (top-level `Gfun`, no-capture lambdas)
  | SClosureEnv -- ^ read the closure value's env slot (a capturing let-bound lambda)
  | SForceCell -- ^ force the `ByNeed` cell, then read the forced closure's env slot (a `Grec` member)

derive instance Eq EnvSrc

-- | For test diagnostics: an `FnInfo` mismatch is unreadable without it (ADR-0108's classification
-- | matrix compares whole `FnInfo`s).
instance showEnvSrc :: Show EnvSrc where
  show = case _ of
    SSelf -> "SSelf"
    SSentinel -> "SSentinel"
    SClosureEnv -> "SClosureEnv"
    SForceCell -> "SForceCell"

-- | A statically-known function a saturated call can enter directly: its direct-entry symbol, its
-- | arity, and how to obtain the env operand.
type FnInfo =
  { dsym :: String
  , arity :: Int
  , src :: EnvSrc
  }

-- | An `FnInfo` that stays valid when the callee is read from a capturing activation's `%env`
-- | (ADR-0113 §2). `SSelf` is the one [`EnvSrc`] for which that does NOT hold — it names the
-- | ENCLOSING direct entry's own env word, which a lifted body does not have — so the constructor is
-- | not exported and [`capturableFact`] is the only way in.
-- |
-- | The exclusion is STRUCTURAL, not observational: arguing that today's binder call sites happen not
-- | to stamp `SSelf` pins nothing a refactor cannot silently break, and breaking it makes capture
-- | propagation unsound.
newtype CapturableFact = CapturableFact FnInfo

derive instance Eq CapturableFact

instance showCapturableFact :: Show CapturableFact where
  show (CapturableFact fn) = "(CapturableFact " <> show fn.dsym <> " arity=" <> show fn.arity <> ")"

-- | The only constructor of [`CapturableFact`], fail-closed on `SSelf`.
capturableFact :: FnInfo -> Maybe CapturableFact
capturableFact fn = case fn.src of
  SSelf -> Nothing
  _ -> Just (CapturableFact fn)

-- | Project the fact back out. Total — every [`CapturableFact`] came through [`capturableFact`].
unFact :: CapturableFact -> FnInfo
unFact (CapturableFact fn) = fn

-- | Which derivation produced a candidate fact (ADR-0113 §2). This is the key the deferred/direct/
-- | arity counters are keyed by — NOT [`BindOrigin`], because `AliasLocal` and `AliasGlobal` share
-- | the origin `OLetValue` and could not be told apart by it.
data CandidateKind
  = Capture -- ^ a positional `%env` read whose enclosing binding had a fact
  | AliasLocal -- ^ `Let x (CAtom (AtomVar y))` where `y` is a local binding with a fact
  | AliasGlobal -- ^ the same, where `y` resolved through `gfns`/`xfns`

derive instance Eq CandidateKind
derive instance Ord CandidateKind

instance showCandidateKind :: Show CandidateKind where
  show = case _ of
    Capture -> "Capture"
    AliasLocal -> "AliasLocal"
    AliasGlobal -> "AliasGlobal"

-- | Every [`CandidateKind`], in report order. The census columns, the profile slots and the
-- | `--paired localfacts` transfer are all stated over this array, so a new kind updates them
-- | together (ADR-0113 §3).
candidateKinds :: Array CandidateKind
candidateKinds = [ Capture, AliasLocal, AliasGlobal ]

-- | The report name (also the census's TSV token and the profile slot's suffix).
candidateKindName :: CandidateKind -> String
candidateKindName = case _ of
  Capture -> "capture"
  AliasLocal -> "alias-local"
  AliasGlobal -> "alias-global"

-- | A candidate fact carries the derivation that produced it, because the kind is not recoverable
-- | from the binding's [`BindOrigin`] at classification time (ADR-0113 §1).
type CandidateFact =
  { fact :: CapturableFact
  , kind :: CandidateKind
  }

-- | A lifted lambda awaiting emission (ADR-0076 §1): its global name, params, captured free vars (in a
-- | fixed order), and body. Emitted as two entries — the `tailcc` `@<name>$d` and the generic
-- | `@<name>` wrapper. `selfName` is the source binding this lambda is the RHS of (a recursive-group
-- | member), enabling the self-call shortcut. `exported` gives the `$d` external linkage (ADR-0077 §3).
-- |
-- | ADR-0113 §2: a capture's NAME and its [`LocalFact`] are one element, not two parallel arrays.
-- | `makeClosure` reads the element's `name` to fetch the value and the capture prologue reads the
-- | same element's `fact` to stamp the binding, so the correspondence between `%env[i]`, its value
-- | and its fact is structural rather than maintained. The old `captureFns` side-array is gone: it
-- | could express a missing fact, a duplicate, or a fact attached to the wrong name.
newtype Lifted = Lifted
  { name :: String
  , params :: Array String
  , captures :: Array Capture
  , body :: LiftedBody
  , selfName :: Maybe String
  , exported :: Boolean
  }

-- | One captured free variable: the name whose value goes into `%env[i]`, and what was known about
-- | it at the point `lift` was called (ADR-0113 §2).
type Capture =
  { name :: String
  , fact :: LocalFact
  }

-- | `LBody` is an ordinary lambda body. `LClosure lm` is a `Grec` function member's suspension body:
-- | forcing the member's cell builds `lm`'s closure (ADR-0076 §2).
data LiftedBody
  = LBody Expr
  | LClosure Lifted

-- | A dependency export's call-relevant fact as its `.pmi` publishes it (ADR-0077 §2): a non-recursive
-- | function of an arity (`Cfn` = `Efn`, sentinel-env entry) or a recursive-group function member of an
-- | arity (`Crecfn` = `Erecfn`, force-cell entry). Value exports carry no call fact.
data CallFact
  = Cfn Int
  | Crecfn Int

derive instance Eq CallFact

-- | A classified top-level binding (ADR-0072 §3): a syntactic lambda is a `Gfun` (a closed global
-- | closure); any other non-recursive value is a strict `Gcaf`; a recursive group is a `Grec` built
-- | by-need (ADR-0070 §4).
data Gdef
  = Gfun String (Array String) Expr -- ^ key, params, body
  | Gcaf String Expr -- ^ key, strict value
  | Grec (Array (Tuple String Expr)) -- ^ recursive-group members: keys + bodies

-- | How a variable binding is realised (ADR-0105 §2/§6, ADR-0106): a non-crossing definition
-- | holds its value token DIRECTLY (alias bindings inherit the token unchanged — never
-- | re-stamped); a crossing definition holds its BY-TYPE-rooted token (`RootedVal`, carrying
-- | the owned slot); a read yields the rooted token and the renderer-owned reload cache
-- | materialises its current value at consumption (§6.4).
data BindingV
  = DirectV Val
  | RootedV RootedVal

-- | WHERE a binding was made (ADR-0113 §1). Total over every bind site the emitter has: a REQUIRED
-- | argument of every binder below, never a defaulted field, so a bind site added later fails to
-- | compile until it names its origin.
-- |
-- | Four binders are called at six places, which produce these seven origins — the `Let` and `Grec`
-- | places each split on whether the right-hand side is a lambda; the capture place does not split,
-- | because whether a fact came with it is [`LocalFact`]'s question, not this one's.
data BindOrigin
  = OParam -- ^ `emitFunction`'s parameter prologue
  | OCapture -- ^ `emitFunction`'s positional `%env` reads
  | OLetLambda -- ^ `expr`'s `Let x (CLam …)` arm
  | OLetValue -- ^ `expr`'s other `Let` arm
  | OGrecLambda -- ^ `buildGrec`, member whose RHS is a lambda
  | OGrecValue -- ^ `buildGrec`, member whose RHS is not
  | OMatchBinder -- ^ the decision-tree leaf binder

derive instance Eq BindOrigin
derive instance Ord BindOrigin

instance showBindOrigin :: Show BindOrigin where
  show = case _ of
    OParam -> "OParam"
    OCapture -> "OCapture"
    OLetLambda -> "OLetLambda"
    OLetValue -> "OLetValue"
    OGrecLambda -> "OGrecLambda"
    OGrecValue -> "OGrecValue"
    OMatchBinder -> "OMatchBinder"

-- | Every [`BindOrigin`], in report order — the census's reason columns are stated over this array.
bindOrigins :: Array BindOrigin
bindOrigins = [ OParam, OCapture, OLetLambda, OLetValue, OGrecLambda, OGrecValue, OMatchBinder ]

-- | The report name (also the census's TSV token).
bindOriginName :: BindOrigin -> String
bindOriginName = case _ of
  OParam -> "param"
  OCapture -> "capture"
  OLetLambda -> "let-lambda"
  OLetValue -> "let-value"
  OGrecLambda -> "grec-lambda"
  OGrecValue -> "grec-value"
  OMatchBinder -> "match-binder"

-- | WHAT was known at a bind site (ADR-0113 §1/§2). One channel, not two: a second field beside an
-- | active fact would make "active AND candidate", and "one updated without the other",
-- | representable states.
-- |
-- | The distinction that matters is USE, not knowledge. `directTarget` reads only the ACTIVE arm, so
-- | a candidate is inert — derived, counted, and never lowered against until the ADR-0113 §4 knob
-- | says otherwise. That is what keeps slices 1–2 emission-identical.
data LocalFact
  = FActive CapturableFact -- ^ a fact `directTarget` may use today
  | FCandidate CandidateFact -- ^ derivable at bind time; NOT used while the knob is off
  | FNone -- ^ nothing was derivable here

derive instance Eq LocalFact

instance showLocalFact :: Show LocalFact where
  show = case _ of
    FActive f -> "(FActive " <> show f <> ")"
    FCandidate c -> "(FCandidate " <> show c.kind <> " " <> show c.fact <> ")"
    FNone -> "FNone"

type EnvEntry =
  { bind :: BindingV
  , key :: String
  -- ^ identity KEY stamped at bind time (comparison bookkeeping for directTarget, never an
  -- operand; the caged Value.keyOf runs only here)
  , origin :: BindOrigin
  , fact :: LocalFact
  }

-- | The ACTIVE fact of an entry — what `knownFn` used to be, now a projection rather than a field
-- | (ADR-0113 §1). A candidate deliberately does NOT answer here.
activeFn :: EnvEntry -> Maybe FnInfo
activeFn e = case e.fact of
  FActive f -> Just (unFact f)
  _ -> Nothing

-- | The fact available for PROPAGATION — active or candidate alike (ADR-0113 §2). Returns the fact
-- | only: the [`CandidateKind`] is re-stamped by whichever bind site republishes it, so a capture of
-- | a local alias classifies as `Capture` (where a lowering would have to fix it) rather than
-- | inheriting `AliasLocal` from two links back.
-- |
-- | Reading THIS, not [`activeFn`], is what makes the derivation transitive: without it a chain
-- | `x = y; z = x` stops at one level and the measurement under-counts the very lever it sizes.
candidateOf :: EnvEntry -> Maybe CapturableFact
candidateOf e = case e.fact of
  FActive f -> Just f
  FCandidate c -> Just c.fact
  FNone -> Nothing

-- | The local scope: an assoc list, most-recent binding first (`List.lookup` finds it first), matching
-- | boot's `(string * env_entry) list` with `List.assoc_opt`.
type Env = List (Tuple String EnvEntry)

-- | Bind a variable to its rooted token (boot's `bind`). Named `bindVar`, not `bind`, so it never
-- | shadows `Prelude`'s `bind` in a consumer that opens this module — do-notation's implicit `bind`
-- | would otherwise become ambiguous.
bindVar :: Env -> BindOrigin -> String -> RootedVal -> LocalFact -> Env
bindVar env origin x rv fact =
  Tuple x { bind: RootedV rv, key: keyOf (rootedVal rv), origin, fact } : env

-- | Bind a NON-CROSSING definition to its value token directly (ADR-0105 §3: no slot, no store,
-- | no reload) — only sound when the activation plan proves no safepoint sits inside its live
-- | range; the token is stored AS GIVEN (§6.2 alias inheritance — binding is not a validity
-- | event and must not re-stamp).
bindDirectVar :: Env -> BindOrigin -> String -> Val -> LocalFact -> Env
bindDirectVar env origin x v fact = Tuple x { bind: DirectV v, key: keyOf v, origin, fact } : env

-- | Bind a variable that is statically a known lambda — its saturated calls may go direct.
bindFnVar :: Env -> BindOrigin -> String -> RootedVal -> CapturableFact -> Env
bindFnVar env origin x rv fn = bindVar env origin x rv (FActive fn)

-- | `bindDirectVar` for a known-lambda binding (ADR-0105): the value token directly plus the
-- | direct-call info.
bindDirectFnVar :: Env -> BindOrigin -> String -> Val -> CapturableFact -> Env
bindDirectFnVar env origin x v fn = bindDirectVar env origin x v (FActive fn)

-- | Look a variable up in the local scope, most-recent binding first.
lookupEnv :: String -> Env -> Maybe EnvEntry
lookupEnv x = map snd <<< List.find (\(Tuple k _) -> k == x)

-- | The binding whose lambda is currently being emitted (boot's `self_ctx` tuple): the source name, its
-- | entry-time capture identity when the name is captured (`Nothing` when it resolves as a global —
-- | compared via `valKey`/handle, bookkeeping only), this activation's `%env` word binding (a
-- | direct token when the plan proved `%env` non-crossing, else a rooted handle resolved
-- | through the reload cache at self-calls), and its own direct-entry info.
type SelfCtx =
  { name :: String
  , captureHandle :: Maybe String
  , envBind :: BindingV
  , fnInfo :: FnInfo
  }

-- | The result of the native backend split (`Driver.nativeSplit`, ADR-0072 §1/§3): the per-module `.ll`
-- | objects, the init/entry object `.ll`, and the native foreign keys the program references (ADR-0073 §3).
type SplitOutput =
  { modules :: Array (Tuple String String) -- ^ (module name, its `.ll`)
  , entry :: String -- ^ the init/entry object `.ll`
  , foreigns :: Set String
  }
