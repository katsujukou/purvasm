module Example.EffectRef.Main where

import Prelude

import Effect (Effect, whileE)
import Effect.Ref as Ref
import Purvasm.Stdio as Stdio

main ∷ Effect Unit
main = do
  r <- Ref.new 0
  Ref.write 1 r

  whenM (Ref.read r <#> (_ >= 0)) do
    Stdio.writeLine "The ref is non-negative!"

  whileE (Ref.read r <#> (_ < 10)) do
    Ref.modify_ (_ * 2) r

  Ref.read r >>= \v -> Stdio.writeLine ("The final result is " <> show v)