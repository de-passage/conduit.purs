module Utils where

import Prelude

import Data.Maybe (Maybe, maybe)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as C
import Halogen as H
import Unsafe.Coerce as Unsafe
import Web.Event.Event (Event, preventDefault) as W

preventDefault :: forall s a c o m.
    MonadEffect m => 
    W.Event -> Maybe a -> (a -> H.HalogenM s a c o m Unit) -> H.HalogenM s a c o m Unit
preventDefault event action handle = do
    H.liftEffect $ W.preventDefault event
    maybe (pure unit) handle action

log :: forall s a c o m.
    MonadEffect m =>
    String -> H.HalogenM s a c o m Unit
log = H.liftEffect <<< C.log

unsafeLog :: forall s a c o m i.
    MonadEffect m =>
    i -> H.HalogenM s a c o m Unit
unsafeLog = H.liftEffect <<< C.log <<< Unsafe.unsafeCoerce