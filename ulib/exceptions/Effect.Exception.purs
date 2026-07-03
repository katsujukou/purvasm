-- | ulib SHADOW of `exceptions`' `Effect.Exception` (ADR-0074), targeting exceptions 6.1.0.
-- |
-- | The native backend has no catchable exception (real unwinding is a future record): `Error` is
-- | an ordinary PureScript value, `throwException` is a **fatal, unrecoverable abort** — its
-- | rendering to stderr then `exit 1`, matching Node's uncaught-exception observable behaviour
-- | minus the stack trace — and `catchException` passes the action through. The pass-through is
-- | coherent because every throw is fatal: no exception ever reaches a handler, so the one
-- | divergence from JS (a throw-catch-recover program aborts at the throw instead of recovering)
-- | is always loud, never a silent wrong value. All 10 upstream foreigns are dropped; the export
-- | list is unchanged.
module Effect.Exception
  ( Error
  , catchException
  , error
  , errorWithCause
  , errorWithName
  , message
  , name
  , stack
  , throw
  , throwException
  , try
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Purvasm.Stdio (writeErrLine)
import Purvasm.System.Process (exit)

-- | The type of errors. Opaque, as upstream's foreign `data Error`.
data Error = MkError { name :: String, message :: String, cause :: Maybe Error }

-- Upstream shows the JS stack when present; there is none natively — a documented cosmetic
-- divergence (ADR-0074 §1).
instance showError :: Show Error where
  show (MkError e) = e.name <> ": " <> e.message

-- | Create an error, specifying a message
error :: String -> Error
error msg = MkError { name: "Error", message: msg, cause: Nothing }

-- | Create an error, specifying a message and a cause
errorWithCause :: String -> Error -> Error
errorWithCause msg cause = MkError { name: "Error", message: msg, cause: Just cause }

-- | Create an error, specifying a message and a name
errorWithName :: String -> String -> Error
errorWithName nm msg = MkError { name: nm, message: msg, cause: Nothing }

-- | Get the error message from an error
message :: Error -> String
message (MkError e) = e.message

-- | Get the error name when defined, or fallback to 'Error'
name :: Error -> String
name (MkError e) = e.name

-- | Get the stack trace from an error — always `Nothing` natively (no stack trace exists).
stack :: Error -> Maybe String
stack _ = Nothing

-- | Throw an exception: fatal and unrecoverable on the native backend (ADR-0074 §2) — the
-- | rendered error to stderr, then `exit 1`. No handler can intercept it. `exit` is typed
-- | `Effect Void` (it never returns), so mapping `absurd` gives the polymorphic result totally —
-- | no unsafe anything.
throwException :: forall a. Error -> Effect a
throwException e = do
  writeErrLine (show e)
  absurd <$> exit 1

-- | A shortcut allowing you to throw an error in one step. Defined as
-- | `throwException <<< error`.
throw :: forall a. String -> Effect a
throw = throwException <<< error

-- | Catch an exception by providing an exception handler — which, on the native backend, never
-- | runs: every throw is fatal (ADR-0074 §3), so the action passes through unobserved.
catchException :: forall a. (Error -> Effect a) -> Effect a -> Effect a
catchException _ action = action

-- | Runs an Effect and returns eventual Exceptions as a `Left` value. If the
-- | computation succeeds the result gets wrapped in a `Right`.
try :: forall a. Effect a -> Effect (Either Error a)
try action = catchException (pure <<< Left) (Right <$> action)
