module Sample.ConstrainedInstance where

class X x where
  x :: x -> Int

class Y y

class V y

class W w where
  w :: w

class W z <= Z z where
  z :: z -> z -> z

instance X Int where
  x _ = 42

instance Y Int

instance V Int

instance W Int where
  w = 0

instance
  ( X z
  , Y z
  , V z
  , W z
  ) =>
  Z z where
  z l r = if isZero (x l) then w else r

isZero :: Int -> Boolean
isZero 0 = true
isZero _ = false

it :: Int
it = z 0 1