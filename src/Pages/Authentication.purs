module Pages.Authentication where

import Prelude
import API as API
import API.Response as R
import Classes as C
import Data.Array (snoc)
import Data.ButtonStatus (ButtonStatus(..), disabled)
import Data.Const (Const)
import Data.Either (Either(..), either)
import Data.GlobalState (WithUrls)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (sequence)
import Data.User (User)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Router (loginUrl, registerUrl)
import Utils as Utils
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Query
  = Const Void

data Output
  = LoginPerformed User

type Slot
  = H.Slot Query Output

data AuthenticationAction
  = Login
  | Register

type Input
  = Record (WithUrls ( action :: AuthenticationAction ))

type State
  = Record
      ( WithUrls
          ( action :: AuthenticationAction
          , errorMessages :: Maybe R.Error
          , name :: Maybe String
          , password :: Maybe String
          , email :: Maybe String
          , buttonStatus :: ButtonStatus
          )
      )

data Action
  = Receive Input
  | PostRequest
  | EmailChanged String
  | NameChanged String
  | PasswordChanged String
  | PreventDefault Event (Maybe Action)

type ChildSlots
  = ()

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , receive = Just <<< Receive
              }
    }

initialState :: Input -> State
initialState { action, urls } =
  { action
  , urls
  , errorMessages: Nothing
  , password: Nothing
  , email: Nothing
  , name: Nothing
  , buttonStatus: Active
  }

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
render state =
  let
    fieldset input placeholder action value =
      HH.fieldset [ HP.class_ BS.formGroup ]
        [ HH.input
            [ HP.classes [ BS.formControl, BS.formControlLg ]
            , HP.type_ input
            , HP.placeholder placeholder
            , HE.onValueChange (Just <<< action)
            , HP.id_ placeholder
            , HP.value value
            ]
        ]

    namefield = fieldset HP.InputText "Name" NameChanged $ fromMaybe "" state.name

    emailfield = fieldset HP.InputEmail "Email" EmailChanged $ fromMaybe "" state.email

    passwordfield = fieldset HP.InputPassword "Password" PasswordChanged $ fromMaybe "" state.password

    button text =
      HH.button
        [ HP.classes [ BS.btn, BS.btnLg, BS.btnPrimary, C.pullXsRight ]
        , HE.onClick $ preventDefault PostRequest
        , disabled state.buttonStatus
        ]
        [ HH.text text
        ]

    formContent text fields = fields `snoc` button text

    content text fields redirectMessage =
      HH.div [ HP.class_ C.authPage ]
        [ HH.div [ HP.classes [ BS.container, C.page ] ]
            [ HH.div [ HP.class_ BS.row ]
                [ HH.div [ HP.classes [ BS.colMd6, BS.offsetMd3, C.colXs12 ] ]
                    [ HH.h1 [ HP.class_ C.textXsCenter ] [ HH.text text ]
                    , HH.p [ HP.class_ C.textXsCenter ]
                        [ redirectMessage
                        ]
                    , maybe (HH.div_ []) Utils.errorDisplay state.errorMessages
                    , HH.form_ (formContent text fields)
                    ]
                ]
            ]
        ]
  in
    case state.action of
      Login ->
        content "Sign in" [ emailfield, passwordfield ]
          $ HH.a [ HP.href registerUrl ] [ HH.text "Do not have an account yet?" ]
      Register ->
        content "Sign up" [ namefield, emailfield, passwordfield ]
          $ HH.a [ HP.href loginUrl ] [ HH.text "Have an account already?" ]

type Context m r
  = H.HalogenM State Action ChildSlots Output m r

handleAction ∷
  forall m.
  MonadAff m =>
  Action ->
  Context m Unit
handleAction = case _ of
  Receive { action, urls } -> H.modify_ (_ { action = action, urls = urls })
  PostRequest -> do
    H.modify_ _ { buttonStatus = Inactive }
    state <- H.get
    case state.action of
      Login -> case sequence [ state.email, state.password ] of
        Just [ email, password ] -> do
          user <- login state.urls email password
          user
            # either
                (\v -> H.modify_ _ { errorMessages = Just v, buttonStatus = Active })
                loggedIn
        _ -> pure unit
      Register -> case sequence [ state.name, state.email, state.password ] of
        Just [ username, email, password ] -> do
          req <- H.liftAff $ API.request $ API.registration state.urls { user: { username, email, password } }
          case req of
            Left error -> H.modify_ _ { errorMessages = Just error, buttonStatus = Active }
            Right result -> loggedIn result.user
        _ -> pure unit
  NameChanged name -> H.modify_ _ { name = Just name, errorMessages = Nothing }
  EmailChanged email -> H.modify_ _ { email = Just email, errorMessages = Nothing }
  PasswordChanged password -> H.modify_ _ { password = Just password, errorMessages = Nothing }
  PreventDefault event action -> Utils.preventDefault event action handleAction
  where
  login urls email password = H.liftAff $ API.loginR urls { email, password }

  loggedIn :: User -> Context m Unit
  loggedIn user = do
    H.modify_ _ { errorMessages = Nothing, buttonStatus = Active }
    H.raise $ LoginPerformed user

preventDefault :: Action -> MouseEvent -> Maybe Action
preventDefault action e = Just $ PreventDefault (toEvent e) $ Just action
