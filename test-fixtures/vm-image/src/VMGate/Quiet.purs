-- | The smallest program that is still a program: a foreign-free `Effect` entry, for the ADR-0110
-- | slice-2 image gate's "a real image, produced by `purvasm run`" leg. It prints nothing on purpose —
-- | what is under test is that a linked image *decodes and runs*, and any output would be the
-- | runtime's stdio leaf rather than the reader.
module VMGate.Quiet where

import Prelude

import Effect (Effect)

main :: Effect Unit
main = pure unit
