-- | Classify an ANF binding's right-hand side into a global definition (ADR-0030/0033):
-- | a syntactic lambda is a `Gfun` (a global closure, seeing the whole table — recursion
-- | included); any other binding is a CAF — strict (`Gcaf`, built once at start-up in
-- | dependency order) or, in a recursive group, by-need (`Grec`, so a cyclic
-- | instance-dictionary group can close, ADR-0024). Ported from boot's
-- | `Vm.Codegen.gdef_of_expr`.
module Purvasm.Compiler.Bytecode.Codegen where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Purvasm.Compiler.Bytecode.Instruction (CodeBlock)
import Purvasm.Compiler.Bytecode.Lower (fnChunk, lowerExpr)
import Purvasm.Compiler.MiddleEnd.ANF (CExpr(..), Expr(..))

-- | A top-level binding: a function (params + body chunk), a strict CAF, or a member of a
-- | by-need recursive group.
data Gdef
  = Gfun (Array String) CodeBlock
  | Gcaf CodeBlock
  | Grec CodeBlock

derive instance Eq Gdef
derive instance Generic Gdef _
instance Show Gdef where
  show g = genericShow g

gdefOfExpr :: Boolean -> Expr -> Gdef
gdefOfExpr recursive = case _ of
  Ret (CLam ps b) -> Gfun ps (fnChunk b)
  e -> let chunk = lowerExpr true e in if recursive then Grec chunk else Gcaf chunk
