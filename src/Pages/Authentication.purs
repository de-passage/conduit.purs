module Pages.Authentication where

import Prelude
import API as API
import Classes as C
import Data.Array (snoc)
import Data.Const (Const)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.User (User, storeUser)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Router (loginUrl, registerUrl)

type Query
  = Const Void

type Output
  = Void

type Slot
  = H.Slot Query Output

data Input
  = Login
  | Register

type State
  = { action :: Input
    , errorMessages :: Array String
    , name :: Maybe String
    , password :: Maybe String
    , email :: Maybe String
    }

data Action
  = ActionChanged Input
  | PostRequest
  | EmailChanged String
  | NameChanged String
  | PasswordChanged String

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
              , receive = Just <<< ActionChanged
              }
    }

initialState :: Input -> State
initialState action =
  { action
  , errorMessages: []
  , password: Nothing
  , email: Nothing
  , name: Nothing
  }

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
render state =
  let
    fieldset input placeholder action =
      HH.fieldset [ HP.class_ BS.formGroup ]
        [ HH.input
            [ HP.classes [ BS.formControl, BS.formControlLg ]
            , HP.type_ input
            , HP.placeholder placeholder
            , HE.onValueChange (Just <<< action)
            , HP.id_ placeholder
            ]
        ]

    namefield = fieldset HP.InputText "Name" NameChanged

    emailfield = fieldset HP.InputEmail "Email" EmailChanged

    passwordfield = fieldset HP.InputPassword "Password" PasswordChanged

    button text =
      HH.button
        [ HP.classes [ BS.btn, BS.btnLg, BS.btnPrimary, C.pullXsRight ]
        , HE.onClick (\_ -> Just PostRequest)
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
                    , HH.ul [ HP.class_ C.errorMessages ]
                        (map (\err -> HH.li_ [ HH.text err ]) state.errorMessages)
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

handleAction ∷
  forall o m.
  MonadAff m =>
  Action ->
  H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  ActionChanged action -> H.modify_ (_ { action = action })
  PostRequest -> do
    state <- H.modify (_ { errorMessages = [] })
    case state.action of
      Login -> case sequence [ state.email, state.password ] of
        Just [ email, password ] -> do
          user <- login email password
          user # either
            (H.modify_ <<< (\v -> _ { errorMessages = [ v ] }))
            (H.liftEffect <<< storeUser)
        _ -> pure unit
      Register -> case sequence [ state.name, state.email, state.password ] of
        Just [ name, email, password ] -> pure unit
        _ -> pure unit
  NameChanged name -> H.modify_ _ { name = Just name }
  EmailChanged email -> H.modify_ _ { email = Just email }
  PasswordChanged password -> H.modify_ _ { password = Just password }
  where
  login :: String -> String -> H.HalogenM State Action ChildSlots o m (Either String User)
  login email password = H.liftAff $ API.login { email, password }
