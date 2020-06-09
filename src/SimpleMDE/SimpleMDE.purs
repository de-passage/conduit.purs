module SimpleMDE where

import Prelude

import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn2, runFn1)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as HQ
import Utils as Utils
import Web.HTML (HTMLElement)

foreign import data SimpleMDE :: Type

foreign import setHTML :: HTMLElement -> String -> Effect Unit

foreign import marked :: Fn2 String String (Effect Unit)

foreign import simpleMDE :: Fn1 String (Effect SimpleMDE)

foreign import onSimpleMDEChange :: forall a. SimpleMDE -> (String -> a) -> (Effect Unit)

foreign import getSimpleMDEValue :: SimpleMDE -> String

foreign import setSimpleMDEValue :: SimpleMDE -> String -> (Effect Unit)

foreign import debug :: forall a. String -> a -> a

data Query a
  = Value (String -> a)

data Output
  = ValueChanged String

type Input
  = { contextName :: String }

data Action
  = Init
  | ChangeValue String

type State
  = { contextName :: String, simpleMDE :: Effect SimpleMDE, sid :: Maybe H.SubscriptionId }

type Slot = H.Slot Query Output

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Init
              }
    }
  where
  initialState :: Input -> State
  initialState { contextName } =
    { contextName: "halogen-simpleMDE-" <> contextName
    , simpleMDE: simpleMDE contextName
    , sid: Nothing
    }

  render :: forall i. State -> HH.HTML i Action
  render s =
    HH.div [ HP.ref (H.RefLabel s.contextName) ]
      [ HH.textarea [ HP.id_ s.contextName ]
      ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Init -> do
      ctx <- H.gets _.contextName
      el <- H.getHTMLElementRef (H.RefLabel ctx)
      smde <- H.liftEffect $ simpleMDE ctx
      sid <-
        H.subscribe $ HQ.EventSource
          $ pure
              { producer: producer smde, finalizer: mempty
              }
      H.modify_ _ { sid = Just sid }
      pure unit
    ChangeValue value -> do
      H.raise $ ValueChanged value

  handleQuery :: forall a. Query a -> H.HalogenM State Action () Output m (Maybe a)
  handleQuery = case _ of
    Value sendValue -> do
      Utils.log "query handled"
      mde <- H.gets _.simpleMDE
      value <- H.liftEffect $ mde <#> runFn1 getSimpleMDEValue
      pure $ Just $ sendValue value

  producer :: SimpleMDE -> CR.Producer Action m Unit
  producer smde = CRA.produce' \emitter ->
    H.liftEffect $ onSimpleMDEChange smde \str ->
      CRA.emit emitter (ChangeValue str)
