module Purvasm.Compiler.Effects.WEnv where

import Prelude

import Effect.AVar (AVar)
import Effect.Aff.AVar as AVar
import Run (AFF, Run)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type WENV e r = (wenv :: WEnv e | r)
data WEnv e a
  = Read (e -> a)
  | Update (e -> e) (e -> a)

derive instance Functor (WEnv e)

_wenv :: Proxy "wenv"
_wenv = Proxy

read :: forall r e. Run (WENV e + r) e
read = Run.lift _wenv $ Read identity

update :: forall r e. (e -> e) -> Run (WENV e + r) e
update f = Run.lift _wenv $ Update f identity

write :: forall r e. e -> Run (WENV e + r) e
write e = update (const e)

interpret :: forall r e a. (WEnv e ~> Run r) -> Run (WENV e + r) a -> Run r a
interpret handler = Run.interpret (Run.on _wenv handler Run.send)

avarHandler :: forall r e a. AVar e -> WEnv e a -> Run (AFF + r) a
avarHandler avar = case _ of
  Read reply -> do
    e <- Run.liftAff (AVar.read avar)
    pure $ reply e
  Update f reply -> do
    Run.liftAff do
      a <- AVar.take avar
      let a' = f a
      AVar.put a' avar
      pure $ reply a'