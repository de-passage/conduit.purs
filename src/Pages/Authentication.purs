module Pages.Authentication where

import Prelude

import API as API
import Classes as C
import Data.Const (Const)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.User (User, storeUser)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Router (loginUrl, registerUrl)
import Unsafe.Coerce (unsafeCoerce)

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
    formContent text =
      [ HH.fieldset [ HP.class_ BS.formGroup ]
          [ HH.input
              [ HP.classes [ BS.formControl, BS.formControlLg ]
              , HP.type_ HP.InputEmail
              , HP.placeholder "Email"
              , HE.onValueChange (Just <<< EmailChanged)
              ]
          ]
      , HH.fieldset [ HP.class_ BS.formGroup ]
          [ HH.input
              [ HP.classes [ BS.formControl, BS.formControlLg ]
              , HP.type_ HP.InputPassword
              , HP.placeholder "Password"
              , HE.onValueChange (Just <<< PasswordChanged)
              ]
          ]
      , HH.button
          [ HP.classes [ BS.btn, BS.btnLg, BS.btnPrimary, C.pullXsRight ]
          , HE.onClick (\_ -> Just PostRequest)
          ]
          [ HH.text text
          ]
      ]

    content text redirectMessage =
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
                    , HH.form_ (formContent text)
                    ]
                ]
            ]
        ]
  in
    case state.action of
      Login -> content "Sign in" $ HH.a [ HP.href registerUrl ] [ HH.text "Do not have an account yet?" ]
      Register -> content "Sign up" $ HH.a [ HP.href loginUrl ] [ HH.text "Have an account already?" ]

handleAction ∷
  forall o m.
  MonadAff m =>
  Action ->
  H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  ActionChanged action -> H.modify_ (_ { action = action })
  PostRequest -> do
    state <- H.get
    case state.action of
      Login -> case sequence [ state.email, state.password ] of
        Just [ email, password ] -> 
          do 
            user <- login email password
            H.liftEffect $ log $ unsafeCoerce user
            either
              (H.modify_ <<< (\v -> _ { errorMessages = [ v ]}))
              (H.liftEffect <<< storeUser)
              user
          
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
