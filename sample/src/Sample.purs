module Sample where

import Prelude

even :: Int -> Boolean
even n
  | n == 0 = true
  | otherwise = odd (n - 1)

odd :: Int -> Boolean
odd n = even (n - 1)

-- import Data.Boolean (otherwise)

-- data Foo = Foo Int | FooBar Bar
-- data Bar = BarFoo Foo
-- newtype A a = A String

-- class X a where
--   x :: a

-- class X a <= Y a where
--   y :: a

-- ya :: forall a. Y a => a
-- ya = y

-- instance X Char where
--   x = 'a'

-- instance X Int where
--   x = if isZero ya then 114 else 514

-- instance Y Int where
--   y = 0

-- isZero :: Int -> Boolean
-- isZero 0 = true
-- isZero _ = false