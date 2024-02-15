module PureScript.CoreFn.Transform where

import Prelude

import PureScript.CoreFn as CF

alpha :: forall a. CF.Expr a -> CF.Expr a
alpha = identity