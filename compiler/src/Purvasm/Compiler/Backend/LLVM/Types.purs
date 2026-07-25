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

-- | An environment entry: the variable's rooted `pv_get` handle, plus static function info when the
-- | binding is a known lambda (a direct-call candidate, ADR-0076 §2).
type EnvEntry =
  { handle :: String
  -- | ADR-0105 slice 2: `true` = `handle` is the value's SSA operand DIRECTLY (a non-crossing
  -- | definition: no root slot exists, no reload happens — valid because no safepoint can
  -- | intervene inside its live range); `false` = `handle` is a shadow-stack root handle and
  -- | every use reloads through it.
  , direct :: Boolean
  , knownFn :: Maybe FnInfo
  }

-- | The local scope: an assoc list, most-recent binding first (`List.lookup` finds it first), matching
-- | boot's `(string * env_entry) list` with `List.assoc_opt`.
type Env = List (Tuple String EnvEntry)

-- | Bind a variable to its rooted handle (boot's `bind`). Named `bindVar`, not `bind`, so it never
-- | shadows `Prelude`'s `bind` in a consumer that opens this module — do-notation's implicit `bind`
-- | would otherwise become ambiguous.
bindVar :: Env -> String -> String -> Env
bindVar env x handle = Tuple x { handle, direct: false, knownFn: Nothing } : env

-- | Bind a NON-CROSSING definition to its SSA operand directly (ADR-0105 §3: no slot, no store,
-- | no reload) — only sound when the activation plan proves no safepoint sits inside its live range.
bindDirectVar :: Env -> String -> String -> Env
bindDirectVar env x v = Tuple x { handle: v, direct: true, knownFn: Nothing } : env

-- | Bind a variable that is statically a known lambda — its saturated calls may go direct.
bindFnVar :: Env -> String -> String -> FnInfo -> Env
bindFnVar env x handle fn = Tuple x { handle, direct: false, knownFn: Just fn } : env

-- | `bindDirectVar` for a known-lambda binding (ADR-0105): the SSA operand directly plus the
-- | direct-call info.
bindDirectFnVar :: Env -> String -> String -> FnInfo -> Env
bindDirectFnVar env x v fn = Tuple x { handle: v, direct: true, knownFn: Just fn } : env

-- | Look a variable up in the local scope, most-recent binding first.
lookupEnv :: String -> Env -> Maybe EnvEntry
lookupEnv x = map snd <<< List.find (\(Tuple k _) -> k == x)

-- | The binding whose lambda is currently being emitted (boot's `self_ctx` tuple): the source name, its
-- | entry-time capture handle when the name is captured (`Nothing` when it resolves as a global), the
-- | rooted handle of this activation's `%env` word (reloaded at self-calls, since the raw `%env` SSA
-- | value is stale after any safepoint), and its own direct-entry info.
type SelfCtx =
  { name :: String
  , captureHandle :: Maybe String
  , envHandle :: String
  -- | ADR-0105: `true` = `%env` never crosses a safepoint before a self-call, so `envHandle`
  -- | is the raw `%env` SSA operand itself (no slot); `false` = a rooted handle, reloaded at use.
  , envDirect :: Boolean
  , fnInfo :: FnInfo
  }

-- | The result of the native backend split (`Driver.nativeSplit`, ADR-0072 §1/§3): the per-module `.ll`
-- | objects, the init/entry object `.ll`, and the native foreign keys the program references (ADR-0073 §3).
type SplitOutput =
  { modules :: Array (Tuple String String) -- ^ (module name, its `.ll`)
  , entry :: String -- ^ the init/entry object `.ll`
  , foreigns :: Set String
  }
