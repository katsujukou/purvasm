module Node.Cbor
  ( decodeAll
  , decodeFirst
  )
  where

import Prelude

import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Foreign (Foreign)
import Node.Buffer (Buffer)
import Promise.Aff (Promise, toAffE)

decodeAll :: Buffer -> Aff (Array Foreign)
decodeAll = runEffectFn1 decodeAllImpl >>> toAffE

decodeFirst :: Buffer -> Aff Foreign
decodeFirst = runEffectFn1 decodeFirstImpl >>> toAffE

foreign import decodeAllImpl :: EffectFn1 Buffer (Promise (Array Foreign))

foreign import decodeFirstImpl :: EffectFn1 Buffer (Promise Foreign)