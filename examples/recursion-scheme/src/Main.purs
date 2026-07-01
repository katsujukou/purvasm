module Example.RecursionScheme.Main where

import Prelude hiding (add, mul)

import Effect (Effect)
import Purvasm.Stdio (writeLine)
import Example.RecursionScheme.Cata (Fix(..), cata)
import Fmt as Fmt

data ExprF a
  = Lit Int
  | Add a a
  | Mul a a
  | Neg a

derive instance functorExprF :: Functor ExprF

type Expr = Fix ExprF

lit :: Int -> Expr
lit n = Fix (Lit n)

add :: Expr -> Expr -> Expr
add x y = Fix (Add x y)

mul :: Expr -> Expr -> Expr
mul x y = Fix (Mul x y)

neg :: Expr -> Expr
neg x = Fix (Neg x)

print :: Expr -> String
print = cata alg
  where
  alg :: ExprF String -> String
  alg = case _ of
    Lit n -> show n
    Add x y -> "(" <> x <> " + " <> y <> ")"
    Mul x y -> "(" <> x <> " * " <> y <> ")"
    Neg x -> "-(" <> x <> ")"

eval :: Expr -> Int
eval = cata alg
  where
  alg :: ExprF Int -> Int
  alg = case _ of
    Lit n -> n
    Add x y -> x + y
    Mul x y -> x * y
    Neg x -> (-1) * x

main :: Effect Unit
main = do
  let expr1 = add (lit 1) (mul (lit 2) (lit 3))
  writeLine $ Fmt.fmt @"{exp} = {val}" { exp: print expr1, val: eval expr1 }

  let expr2 = mul (add (lit 3) (neg (mul (lit 4) (lit 5)))) (add (lit 1) (lit 2))
  writeLine $ Fmt.fmt @"{exp} = {val}" { exp: print expr2, val: eval expr2 }

