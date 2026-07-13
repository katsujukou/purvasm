-- | A human-readable pretty-printer for the lower IR (ADR-0025 ANF), for inspecting a module's
-- | terms while investigating optimiser blow-up (the `build --emit-ir <module>` seam; the purs-wasm
-- | analogue was invaluable for the same job). Not a parseable surface — a *debug* rendering:
-- |
-- |   * a call is uncurried `f(a, b)`; a lambda `\x y -> body`; a primop `AddInt(x, y)`;
-- |   * `let x = c in body` lays out multi-line (`let` … `in` at the enclosing indent), so a deep
-- |     `let`-spine reads as a flat column — the shape blow-up produces;
-- |   * `case s0, s1 of` with one `binders -> rhs` alternative per line.
-- |
-- | The primary entry is `printModuleAnf`; `printExpr` renders a single binding's term.
module Purvasm.Compiler.MiddleEnd.ANF.Pretty
  ( printModuleAnf
  , printExpr
  ) where

import Prelude

import Data.Array (null)
import Data.Monoid (power)
import Data.String.Common (joinWith)
import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Alt, Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.Module (Decl)

-- | Render a whole module's ANF: `module <name> where` then a blank-line-separated binding per decl.
printModuleAnf :: String -> Array Decl -> String
printModuleAnf name decls =
  "module " <> name <> " where\n\n" <> joinWith "\n\n" (map printDecl decls) <> "\n"

-- | Render a single binding's term at the left margin (the `key = ` prefix is the caller's).
printExpr :: Expr -> String
printExpr = printExprAt 0

-- internals ----------------------------------------------------------------------------

-- `n` spaces of indentation.
sp :: Int -> String
sp n = power " " n

printDecl :: Decl -> String
printDecl d
  | d.recursive =
      "rec\n" <> joinWith "\n\n" (d.members <#> \(k /\ e) -> sp 2 <> k <> " = " <> printExprAt 2 e)
  | otherwise =
      joinWith "\n\n" (d.members <#> \(k /\ e) -> k <> " = " <> printExprAt 0 e)

-- The `Int` is the indent the term's *continuation* lines (the `in`, `else`, alternatives) sit at;
-- the first line is emitted inline after whatever prefix the caller wrote.
printExprAt :: Int -> Expr -> String
printExprAt i = case _ of
  Ret c -> printCExprAt i c
  Let x c body ->
    "let\n" <> sp (i + 2) <> x <> " = " <> printCExprAt (i + 2) c
      <> "\n"
      <> sp i
      <> "in "
      <> printExprAt i body
  LetRec binds body ->
    "let rec\n"
      <> joinWith "\n" (binds <#> \b -> sp (i + 2) <> b.var <> " = " <> printExprAt (i + 2) b.rhs)
      <> "\n"
      <> sp i
      <> "in "
      <> printExprAt i body

printCExprAt :: Int -> CExpr -> String
printCExprAt i = case _ of
  CAtom a -> printAtom a
  CLam ps body -> "\\" <> joinWith " " ps <> " -> " <> printExprAt i body
  CApp f args -> printAtom f <> args' args
  CPrim op args -> show op <> args' args
  CCtor name _ args -> if null args then name else name <> args' args
  CArray args -> "[" <> joinWith ", " (map printAtom args) <> "]"
  CRecord fs -> record (fs <#> \f -> f.prop <> ": " <> printAtom f.val)
  CAccessor a p -> printAtom a <> "." <> p
  CPerform a -> "perform(" <> printAtom a <> ")"
  CUpdate a ups -> printAtom a <> " " <> record (ups <#> \u -> u.prop <> " = " <> printAtom u.val)
  CIf c t e ->
    "if " <> printAtom c <> " then\n"
      <> sp (i + 2)
      <> printExprAt (i + 2) t
      <> "\n"
      <> sp i
      <> "else\n"
      <> sp (i + 2)
      <> printExprAt (i + 2) e
  CCase scruts alts ->
    "case " <> joinWith ", " (map printAtom scruts) <> " of\n"
      <> joinWith "\n" (map (printAltAt (i + 2)) alts)
  where
  args' as = "(" <> joinWith ", " (map printAtom as) <> ")"
  record fields = if null fields then "{}" else "{ " <> joinWith ", " fields <> " }"

printAltAt :: Int -> Alt -> String
printAltAt i a =
  let
    pats = joinWith ", " (map printBinder a.binders)
  in
    case a.result of
      Uncond e -> sp i <> pats <> " -> " <> printExprAt i e
      Guarded gs ->
        sp i <> pats <> "\n"
          <> joinWith "\n"
            (gs <#> \g -> sp (i + 2) <> "| " <> printExprAt (i + 2) g.guard <> " -> " <> printExprAt (i + 2) g.rhs)

printBinder :: Binder -> String
printBinder = case _ of
  BNull -> "_"
  BVar x -> x
  BLit l -> printLit l
  BCtor name bs -> if null bs then name else name <> "(" <> joinWith ", " (map printBinder bs) <> ")"
  BArray bs -> "[" <> joinWith ", " (map printBinder bs) <> "]"
  BRecord fs -> "{ " <> joinWith ", " (fs <#> \f -> f.prop <> ": " <> printBinder f.binder) <> " }"
  BNamed x b -> x <> "@" <> printBinder b

printAtom :: Atom -> String
printAtom = case _ of
  AtomVar x -> x
  AtomLit l -> printLit l
  AtomForeign x -> x

printLit :: Literal -> String
printLit = case _ of
  LInt n -> show n
  LBool b -> if b then "true" else "false"
  LNumber n -> show n
  LString s -> show s
