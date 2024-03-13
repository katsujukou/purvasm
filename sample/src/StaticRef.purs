module Sample.StaticRef where

f :: Int -> Int -> Int
f _ _ = 42

g :: Int -> Int
g = f 0

h :: Int -> Int
h = g

v = 0
x _ _ = y v
y _ = x z 1

z _ = id y 2

w _ = z y 3

id :: forall a. a -> a
id x = x

j :: Int
j = id h 1