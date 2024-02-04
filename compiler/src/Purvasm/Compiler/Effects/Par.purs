module Purvasm.Compiler.Effects.Par where

import Prelude

import Control.Parallel (parSequence_)
import Data.Array as Array
import Data.Either (Either)
import Data.Exists (Exists, mkExists, runExists)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
import Prim.Row as Row
import Run (AFF, Run, EFFECT)
import Run as Run
import Run.Except (EXCEPT)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type PAR r r' = (par :: Par r | r')

data Par r a = All (Exists (AllX r a))

instance Functor (Par r) where
  map f = case _ of
    All ws -> All (ws # runExists \(AllX tup) -> mkExists <<< AllX $ map (map f) tup)

data AllX r a b = AllX (Tuple (Array (Run r b)) (Array b -> a))

_par :: Proxy "par"
_par = Proxy

all :: forall r r' a. Array (Run r a) -> Run (PAR r + r') (Array a)
all workers = Run.lift _par $ All $ mkExists (AllX $ Tuple workers identity)

-- interpret :: forall r' r a. (Par r ~> ?) -> Run (PAR r + r') a -> Run r' a
-- interpret handler = Run.interpret (Run.on _par (runParAff handler) Run.send)

interpret :: forall r r' a. (Par r ~> Run r') -> Run (PAR r + r') a -> Run r' a
interpret handler = Run.interpret (Run.on _par handler Run.send)

interpretAff :: forall r r' a. (Run r ~> Aff) -> Run (AFF + PAR r + r') a -> Run (AFF + r') a
interpretAff affHandler = interpret (runParAff affHandler)

-- interpretExceptAff :: forall r r' e. (forall a. Run r a -> Aff (Either e a)) -> Run (AFF + PAR r + r') ~> Run (AFF + EXCEPT e + r')
-- interpretExceptAff affHandler = interpret (runParExceptAff affHandler)

runParAff :: forall r r' a. (Run r ~> Aff) -> Par r a -> Run (AFF + r') a
runParAff affHandler = case _ of
  All workers -> workers # runExists \(AllX (Tuple ws reply)) -> do
    Run.liftAff do
      results <- AVar.new []
      parSequence_
        ( ws <#> \w -> do
            b <- affHandler w
            AVar.take results >>=
              \bs -> AVar.put (Array.snoc bs b) results
        )
      AVar.take results >>= reply >>> pure

-- runParExceptAff :: forall r r' e. (forall a. Run r a -> Aff (Either e a)) -> Par r ~> Run (AFF + EXCEPT e + r')
-- runParExceptAff affHandler = case _ of
--   All workers -> workers # runExists \(AllX (Tuple ws reply)) -> do
--     Run.liftAff do
--       results <- AVar.new []
--       parSequence_
--         ( ws <#> \w -> do
--             b <- affHandler w
--             AVar.take results >>=
--               \bs -> AVar.put (Array.snoc bs b) results
--         )
--       AVar.take results >>= reply >>> pure