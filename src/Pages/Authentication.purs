module Pages.Authentication where

import Prelude
import Classes (errorMessages)
import Classes as C
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
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
    }

data Action
  = ActionChanged Input

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
  }

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
render state =
  let
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
                        (map (\text -> HH.li_ [ HH.text text ]) state.errorMessages)
                    , HH.form_
                        [ HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.classes [ BS.formControl, BS.formControlLg ]
                                , HP.type_ HP.InputText
                                , HP.placeholder "Your Name"
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.classes [ BS.formControl, BS.formControlLg ]
                                , HP.type_ HP.InputEmail
                                , HP.placeholder "Email"
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.classes [ BS.formControl, BS.formControlLg ]
                                , HP.type_ HP.InputPassword
                                , HP.placeholder "Password"
                                ]
                            ]
                        , HH.button [ HP.classes [ BS.btn, BS.btnLg, BS.btnPrimary, C.pullXsRight ] ]
                            [ HH.text text
                            ]
                        ]
                    ]
                ]
            ]
        ]
    in
        case state.action of
            Login -> 
                content "Sign in" $ HH.a [ HP.href registerUrl ] [ HH.text "Do not have an account yet?" ]
            Register -> 
                content "Sign up" $ HH.a [ HP.href loginUrl ] [ HH.text "Have an account already?" ]
  

handleAction ∷
  forall o m.
  MonadAff m =>
  Action ->
  H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  ActionChanged action ->
    H.modify_ (_ { action = action})
