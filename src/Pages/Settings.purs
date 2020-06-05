module Pages.Settings where

import Prelude

import Classes as C
import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.User (Email(..), Image, User, fromImage)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Utils as Utils
import Web.Event.Event (Event, preventDefault)
import Web.UIEvent.MouseEvent (toEvent)

type Input
  = User

data Output
  = LogOutRequested
  | UserUpdated User

data Action
  = ChangeBio String
  | ChangeName String
  | ChangePicture String
  | ChangePassword String
  | ChangeEmail String
  | UpdateSettings
  | LogOut
  | PreventDefault Event (Maybe Action)

type Query
  = Const Void

type State
  = { currentUser :: User
    , editedUser ::
        { bio :: Maybe String
        , email :: Maybe String
        , password :: Maybe String
        , image :: Maybe String
        , username :: Maybe String
        }
    }

type ChildSlots
  = ()

type Slot
  = H.Slot Query Output

component :: forall m q. MonadAff m => H.Component HH.HTML q Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: Input -> State
  initialState currentUser =
    { currentUser
    , editedUser:
        { bio: Nothing
        , username: Nothing
        , email: Nothing
        , image: Nothing
        , password: Nothing
        }
    }

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
render state =
  HH.div [ HP.class_ C.settingsPage ]
    [ HH.div [ HP.classes [ BS.container, C.page ] ]
        [ HH.div [ HP.class_ BS.row ]
            [ HH.div [ HP.classes [ BS.colMd6, BS.offsetMd3, C.colXs12 ] ]
                [ HH.h1 [ HP.class_ C.textXsCenter ] [ HH.text "Your Settings" ]
                , HH.form_
                    [ HH.fieldset_
                        [ HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.class_ BS.formControl
                                , HP.type_ HP.InputText
                                , HP.placeholder "URL of profile picture"
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.classes [ BS.formControl, BS.formControlLg ]
                                , HP.type_ HP.InputText
                                , HP.placeholder "Your Name"
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.textarea
                                [ HP.classes [ BS.formControl, BS.formControlLg ]
                                , HP.placeholder "Short bio about you"
                                , HP.rows 8
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
                            [ HH.text "Update Settings"
                            ]
                        ]
                        , HH.button [ HP.classes [ BS.btn, BS.btnOutlineDanger ]
                            , HE.onClick \e -> Just (PreventDefault (toEvent e) (Just LogOut)) ]
                            [ HH.text "Log out"]
                    ]
                ]
            ]
        ]
    ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  ChangeEmail str -> H.modify_ _ { editedUser { email = Just str } }
  ChangeBio str -> H.modify_ _ { editedUser { bio = Just str } }
  ChangePicture str -> H.modify_ _ { editedUser { image = Just str } }
  ChangeName str -> H.modify_ _ { editedUser { username = Just str } }
  ChangePassword str -> H.modify_ _ { editedUser { password = Just str } }
  LogOut -> H.raise LogOutRequested
  PreventDefault event action -> Utils.preventDefault event action handleAction
  UpdateSettings ->
    pure unit
