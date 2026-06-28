-- | Classify an ANF binding's right-hand side into a global definition (ADR-0030/0033):
-- | a syntactic lambda is a `Gfun` (a global closure, seeing the whole table — recursion
-- | included); any other binding is a CAF — strict (`Gcaf`, built once at start-up in
-- | dependency order) or, in a recursive group, by-need (`Grec`, so a cyclic
-- | instance-dictionary group can close, ADR-0024). Ported from boot's
-- | `Vm.Codegen.gdef_of_expr`.
module Purvasm.Compiler.Bytecode.Codegen where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
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

-- | Split a whole-program ANF spine (the linked term, ADR-0016) into its global
-- | definitions and the `main` chunk (the first non-binding expression). Ported from
-- | boot's `Vm.Codegen.program`.
program :: Expr -> { gdefs :: Array (String /\ Gdef), main :: CodeBlock }
program = go Nil
  where
  -- `acc` holds the gdefs in *reverse* spine order (cons is O(1), ADR-0049), reversed once
  -- at the end; the `LetRec` arm conses its group in order so the final reverse restores it.
  go :: List (String /\ Gdef) -> Expr -> { gdefs :: Array (String /\ Gdef), main :: CodeBlock }
  go acc = case _ of
    Let k c rest -> go ((k /\ gdefOfExpr false (Ret c)) : acc) rest
    LetRec binds rest -> go (foldl (\a b -> (b.var /\ gdefOfExpr true b.rhs) : a) acc binds) rest
    e -> { gdefs: Array.fromFoldable (List.reverse acc), main: lowerExpr true e }
