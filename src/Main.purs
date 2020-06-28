module Main where

import Prelude
import API as API
import API.Response as R
import App as App
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Data.Either (Either(..))
import Data.Foldable (intercalate, traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.String.CodeUnits as Str
import Data.User as User
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Storage as S
import Web.Event.EventTarget (eventListener, addEventListener) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Event.HashChangeEvent as HCE
import Web.HTML.Event.HashChangeEvent.EventTypes as HCET
import Web.HTML.Location (hash) as DOM
import Web.HTML.Window (location) as DOM
import Web.HTML.Window as Window

main :: Effect Unit
main = do
  url <- DOM.window >>= DOM.location >>= DOM.hash <#> dropHost
  decoded <- S.retrieveUser
  repo <- S.retrieveRepository
  perPage <- S.retrievePerPage
  HA.runHalogenAff do
    body <- HA.awaitBody
    user <-
      decoded
        # maybe (pure Nothing) \{ token } -> do
            req <- API.request $ API.currentUser repo token
            case req of
              Left err -> do
                Console.log $ intercalate "\n" $ R.fromError err
                pure $ Nothing :: Aff (Maybe User.User)
              Right user -> pure $ Just user.user :: Aff (Maybe User.User)
    io <- runUI App.component { url, user, repo, perPage } body
    CR.runProcess (hashChangeProducer CR.$$ hashChangeConsumer io.query)

-- taken from the Halogen examples 
hashChangeProducer :: CR.Producer HCE.HashChangeEvent Aff Unit
hashChangeProducer =
  CRA.produce \emitter -> do
    listener <- DOM.eventListener (traverse_ (CRA.emit emitter) <<< HCE.fromEvent)
    liftEffect
      $ DOM.window
      >>= Window.toEventTarget
      >>> DOM.addEventListener HCET.hashchange listener false

hashChangeConsumer ::
  (forall a. App.Query a -> Aff (Maybe a)) ->
  CR.Consumer HCE.HashChangeEvent Aff Unit
hashChangeConsumer query =
  CR.consumer \event -> do
    let
      hash = dropHost $ HCE.newURL event
    void $ query $ H.tell $ App.ChangeRoute hash
    pure Nothing

dropHost :: String -> String
dropHost = Str.drop 1 <<< Str.dropWhile (_ /= '#')
