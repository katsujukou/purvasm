module Purvasm.ECore.Transform where

import Prelude

import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.ECore.Syntax (Expr(..))
import Purvasm.Types (Ident(..))

-- Replace all occurrence of Var with some expression.
-- This function is *unsafe* because no 
unsafeSubstitute :: forall a. Ident -> Expr a -> Expr a -> Expr a
unsafeSubstitute ident expr = go
  where
  go = case _ of
    ExprVar _ var | var == ident -> expr
    ExprApp a f args -> ExprApp a (go f) (go <$> args)
    ExprAbs a args body -> ExprAbs a args (go body)
    ExprAccess a exp prop -> ExprAccess a (go exp) prop
    ExprUpdate a exp updators -> ExprUpdate a (go exp) ((\(prop /\ e) -> prop /\ go e) <$> updators)
    _ -> unsafeCrashWith "unsafeSubstitute"