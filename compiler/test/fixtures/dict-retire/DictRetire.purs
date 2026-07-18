-- ADR-0104 §3 retirement fixture: a local class whose instance member references a TOP-LEVEL impl
-- (`speak = speakDogImpl`) — the exact liftable shape the former boot-parity bridge collapsed to a
-- direct call. Self-contained (Prim only), so the unit-test harness's single-module closure holds.
module DictRetire where

class Speak a where
  speak :: a -> Int

data Dog = Dog

speakDogImpl :: Dog -> Int
speakDogImpl _ = 42

instance speakDog :: Speak Dog where
  speak = speakDogImpl

use :: Int
use = speak Dog
