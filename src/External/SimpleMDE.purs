module SimpleMDE (component, Query(..), Input(..), Output(..), Slot) where

import Prelude

import Control.Bind (bindFlipped)
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Data.Function.Uncurried (Fn1)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as HQ
import Web.HTML (HTMLElement)

foreign import data SimpleMDE :: Type

foreign import simpleMDE :: Fn1 String (Effect SimpleMDE)

foreign import createSimpleMDE :: Fn1 HTMLElement (Effect SimpleMDE)

foreign import onSimpleMDEChange :: forall a. SimpleMDE -> (String -> a) -> (Effect Unit)

foreign import getSimpleMDEValue :: SimpleMDE -> String

foreign import setSimpleMDEValue :: String -> SimpleMDE -> (Effect Unit)

data Query a
  = GetContent (String -> a)
  | SetContent String (Unit -> a)

data Output
  = ValueChanged String

type Input
  = { contextName :: String, placeholder :: String }

data Action
  = Init
  | ChangeValue String

type State
  = { contextName :: String
    , simpleMDE :: Maybe (Effect SimpleMDE)
    , sid :: Maybe H.SubscriptionId
    , placeholder :: String
    }

type Slot
  = H.Slot Query Output

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
              , handleQuery = handleQuery
              }
    }
  where
  initialState :: Input -> State
  initialState { contextName, placeholder } =
    { contextName: "halogen-simpleMDE-" <> contextName
    , simpleMDE: Nothing
    , sid: Nothing
    , placeholder
    }

  render :: forall i. State -> HH.HTML i Action
  render s =
    HH.div_
      [ HH.textarea [ HP.ref (H.RefLabel s.contextName), HP.placeholder s.placeholder ]
      ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Init -> do
      ctx <- H.gets _.contextName
      mel <- H.getHTMLElementRef (H.RefLabel ctx)
      mel
        # maybe (pure unit) \el -> do
            smde <- H.liftEffect $ createSimpleMDE el
            sid <-
              H.subscribe $ HQ.EventSource
                $ pure
                    { producer: producer smde
                    , finalizer: mempty
                    }
            H.modify_ _ { sid = Just sid, simpleMDE = Just $ pure smde }
    ChangeValue value -> do
      H.raise $ ValueChanged value

  handleQuery :: forall a. Query a -> H.HalogenM State Action () Output m (Maybe a)
  handleQuery = case _ of
    GetContent sendValue -> do
      state <- H.get
      --mel <- H.getHTMLElementRef (H.RefLabel state.contextName)
      state.simpleMDE
        # maybe (pure Nothing) \mde -> do
            value <- H.liftEffect $ getSimpleMDEValue <$> mde
            pure $ Just $ sendValue value
    SetContent newValue a -> do
      mmde <- H.gets _.simpleMDE
      H.liftEffect $ mmde # maybe (pure unit) (setContent newValue)
      pure $ Just $ a unit
    where
      setContent :: String -> Effect SimpleMDE -> Effect Unit
      setContent newValue = bindFlipped $ setSimpleMDEValue newValue 

  producer :: SimpleMDE -> CR.Producer Action m Unit
  producer smde =
    CRA.produce' \emitter ->
      H.liftEffect
        $ onSimpleMDEChange smde \str ->
            CRA.emit emitter (ChangeValue str)
