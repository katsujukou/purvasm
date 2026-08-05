-- | The pure data types the LLVM backend threads through emission (ADR-0072/0076/0077), transcribed
-- | from boot's `codegen_llvm.ml` (`env_src`, `fn_info`, `lifted`/`lifted_body`, `call_fact`, `gdef`,
-- | `split_output`, `env_entry`/`env`). The mutable emitter state (`Ctx`) and the `Codegen` monad live
-- | in `Purvasm.Compiler.Backend.LLVM.Monad`; this module is only the type vocabulary.
module Purvasm.Compiler.Backend.LLVM.Types where

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

-- | A statically-known function a saturated call can enter directly: its direct-entry symbol, its
-- | arity, and how to obtain the env operand.
type FnInfo =
  { dsym :: String
  , arity :: Int
  , src :: EnvSrc
  }

-- | A lifted lambda awaiting emission (ADR-0076 §1): its global name, params, captured free vars (in a
-- | fixed order), and body. Emitted as two entries — the `tailcc` `@<name>$d` and the generic
-- | `@<name>` wrapper. `selfName` is the source binding this lambda is the RHS of (a recursive-group
-- | member), enabling the self-call shortcut. `captureFns` carries direct-call info for captures that
-- | are known recursive-group function members. `exported` gives the `$d` external linkage (ADR-0077 §3).
newtype Lifted = Lifted
  { name :: String
  , params :: Array String
  , captures :: Array String
  , body :: LiftedBody
  , selfName :: Maybe String
  , captureFns :: Array (Tuple String FnInfo)
  , exported :: Boolean
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

-- | An environment entry: the variable's binding realisation, plus static function info when
-- | the binding is a known lambda (a direct-call candidate, ADR-0076 §2).
type EnvEntry =
  { bind :: BindingV
  , key :: String
  -- ^ identity KEY stamped at bind time (comparison bookkeeping for directTarget, never an
  -- operand; the caged Value.keyOf runs only here)
  , knownFn :: Maybe FnInfo
  }

-- | The local scope: an assoc list, most-recent binding first (`List.lookup` finds it first), matching
-- | boot's `(string * env_entry) list` with `List.assoc_opt`.
type Env = List (Tuple String EnvEntry)

-- | Bind a variable to its rooted token (boot's `bind`). Named `bindVar`, not `bind`, so it never
-- | shadows `Prelude`'s `bind` in a consumer that opens this module — do-notation's implicit `bind`
-- | would otherwise become ambiguous.
bindVar :: Env -> String -> RootedVal -> Env
bindVar env x rv = Tuple x { bind: RootedV rv, key: keyOf (rootedVal rv), knownFn: Nothing } : env

-- | Bind a NON-CROSSING definition to its value token directly (ADR-0105 §3: no slot, no store,
-- | no reload) — only sound when the activation plan proves no safepoint sits inside its live
-- | range; the token is stored AS GIVEN (§6.2 alias inheritance — binding is not a validity
-- | event and must not re-stamp).
bindDirectVar :: Env -> String -> Val -> Env
bindDirectVar env x v = Tuple x { bind: DirectV v, key: keyOf v, knownFn: Nothing } : env

-- | Bind a variable that is statically a known lambda — its saturated calls may go direct.
bindFnVar :: Env -> String -> RootedVal -> FnInfo -> Env
bindFnVar env x rv fn = Tuple x { bind: RootedV rv, key: keyOf (rootedVal rv), knownFn: Just fn } : env

-- | `bindDirectVar` for a known-lambda binding (ADR-0105): the value token directly plus the
-- | direct-call info.
bindDirectFnVar :: Env -> String -> Val -> FnInfo -> Env
bindDirectFnVar env x v fn = Tuple x { bind: DirectV v, key: keyOf v, knownFn: Just fn } : env

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
