module Pages.Settings where

import Prelude
import API as API
import API.Response as R
import Classes as C
import Control.Alt ((<|>))
import Data.Const (Const)
import Data.Either (Either(..))
import Data.GlobalState (WithUrls)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.User (User, toMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Utils as Utils
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent (toEvent)

type Input
  = Record (WithUrls ( currentUser :: User ))

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

type EditedUser
  = { bio :: Maybe String
    , email :: Maybe String
    , password :: Maybe String
    , image :: Maybe String
    , username :: Maybe String
    }

type State
  = Record
      ( WithUrls
          ( currentUser :: User
          , editedUser :: EditedUser
          , errorMessages :: Maybe R.Error
          )
      )

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
  initialState { urls, currentUser } =
    { urls
    , currentUser
    , editedUser:
        { bio: Nothing
        , username: Nothing
        , email: Nothing
        , image: Nothing
        , password: Nothing
        }
    , errorMessages: Nothing
    }

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
render state =
  HH.div [ HP.class_ C.settingsPage ]
    [ HH.div [ HP.classes [ BS.container, C.page ] ]
        [ HH.div [ HP.class_ BS.row ]
            [ HH.div [ HP.classes [ BS.colMd6, BS.offsetMd3, C.colXs12 ] ]
                [ HH.h1 [ HP.class_ C.textXsCenter ] [ HH.text "Your Settings" ]
                , maybe (HH.div_ []) Utils.errorDisplay state.errorMessages
                , HH.form_
                    [ HH.fieldset_
                        [ HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.class_ BS.formControl
                                , HP.type_ HP.InputText
                                , HP.placeholder "URL of profile picture"
                                , HP.value (maybe (show state.currentUser.image) show state.editedUser.image)
                                , HE.onValueChange (Just <<< ChangePicture)
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.classes [ BS.formControl, BS.formControlLg ]
                                , HP.type_ HP.InputText
                                , HP.placeholder "Your Name"
                                , HP.value (maybe (show state.currentUser.username) identity state.editedUser.username)
                                , HE.onValueChange (Just <<< ChangeName)
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.textarea
                                [ HP.classes [ BS.formControl, BS.formControlLg ]
                                , HP.placeholder "Short bio about you"
                                , HP.rows 8
                                , HP.value (maybe (maybe "" identity state.currentUser.bio) identity state.editedUser.bio)
                                , HE.onValueChange (Just <<< ChangeBio)
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.classes [ BS.formControl, BS.formControlLg ]
                                , HP.type_ HP.InputEmail
                                , HP.placeholder "Email"
                                , HP.value (maybe (show state.currentUser.email) identity state.editedUser.email)
                                , HE.onValueChange (Just <<< ChangeEmail)
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.classes [ BS.formControl, BS.formControlLg ]
                                , HP.type_ HP.InputPassword
                                , HP.placeholder "Password"
                                , HE.onValueChange (Just <<< ChangePassword)
                                ]
                            ]
                        , HH.button
                            [ HP.classes [ BS.btn, BS.btnLg, BS.btnPrimary, C.pullXsRight ]
                            , HE.onClick $ preventDefault UpdateSettings
                            ]
                            [ HH.text "Update Settings"
                            ]
                        ]
                    , HH.button
                        [ HP.classes [ BS.btn, BS.btnOutlineDanger ]
                        , HE.onClick $ preventDefault LogOut
                        ]
                        [ HH.text "Log out" ]
                    ]
                ]
            ]
        ]
    ]
  where
  preventDefault action e = Just (PreventDefault (toEvent e) (Just action))

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  ChangeEmail str -> H.modify_ _ { editedUser { email = Just str } }
  ChangeBio str -> H.modify_ _ { editedUser { bio = Just str } }
  ChangePicture str -> H.modify_ _ { editedUser { image = Just str } }
  ChangeName str -> H.modify_ _ { editedUser { username = Just str } }
  ChangePassword str -> H.modify_ _ { editedUser { password = Just str } }
  LogOut -> H.raise LogOutRequested
  PreventDefault event action -> Utils.preventDefault event action handleAction
  UpdateSettings -> do
    { currentUser, editedUser, urls } <- H.get
    req <-
      H.liftAff
        $ API.request
        $ API.updateUser urls (mkPayload currentUser editedUser)
            currentUser.token
    case req of
      Left error -> H.modify_ _ { errorMessages = Just error }
      Right user -> do
        H.modify_ _ { errorMessages = Nothing }
        H.raise (UserUpdated user.user)
  where
  mkPayload :: User -> EditedUser -> API.UserUpdatePayload
  mkPayload cur edited =
    { user:
        { username: fromMaybe (show cur.username) edited.username
        , bio: edited.bio <|> cur.bio
        , email: fromMaybe (show cur.email) edited.email
        , password: edited.password
        , image: edited.image <|> toMaybe cur.image
        }
    }
