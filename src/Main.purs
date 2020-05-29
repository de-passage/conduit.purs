module Main where

import Prelude

import App as App
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as Str
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.Event.EventTarget (eventListener, addEventListener) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Event.HashChangeEvent as HCE
import Web.HTML.Event.HashChangeEvent.EventTypes as HCET
import Web.HTML.Window as Window

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    io <- runUI App.component unit body
    CR.runProcess (hashChangeProducer CR.$$ hashChangeConsumer io.query)

-- taken from the Halogen examples 
hashChangeProducer :: CR.Producer HCE.HashChangeEvent Aff Unit
hashChangeProducer = CRA.produce \emitter -> do
  listener <- DOM.eventListener (traverse_ (CRA.emit emitter) <<< HCE.fromEvent)
  liftEffect $
    DOM.window
      >>= Window.toEventTarget
      >>> DOM.addEventListener HCET.hashchange listener false

hashChangeConsumer
  :: (forall a. App.Query a -> Aff (Maybe a))
  -> CR.Consumer HCE.HashChangeEvent Aff Unit
hashChangeConsumer query = CR.consumer \event -> do
  let hash = Str.drop 1 $ Str.dropWhile (_ /= '#') $ HCE.newURL event
  void $ query $ H.tell $ App.ChangeRoute hash
  pure Nothing
