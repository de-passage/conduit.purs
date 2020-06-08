module Pages.Edition where

import Prelude

import API as API
import API.Response (Error)
import Classes as C
import Data.Array (filter, nub, null, snoc)
import Data.Article (Slug)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.User (User)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Utils as Utils
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (toEvent)

data Action
  = ChangeTitle String
  | ChangeDescription String
  | ChangeContent String
  | ChangeTag String
  | AddTag String
  | RemoveTag String
  | Publish
  | PreventDefault Event (Maybe Action)

type State
  = { currentUser :: User
    , currentAction :: EditionType
    , article ::
        { description :: String
        , body :: String
        , title :: String
        , tagList :: Maybe (Array String)
        }
    , currentTag :: String
    , errorMessages :: Maybe Error
    }

data EditionType
  = New
  | Edit Slug

type Input
  = { currentUser :: User
    , currentAction :: EditionType
    }

type ChildSlots
  = ()

data Output
  = Redirect Slug

type Query
  = Const Void

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
              }
    }

initialState :: Input -> State
initialState { currentUser } =
  { currentUser: currentUser
  , currentAction: New
  , article:
      { body: ""
      , title: ""
      , tagList: Nothing
      , description: ""
      }
  , currentTag: ""
  , errorMessages : Nothing
  }

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
render state =
  HH.div [ HP.class_ C.editorPage ]
    [ HH.div [ HP.classes [ BS.container, C.page ] ]
        [ HH.div [ HP.class_ BS.row ]
            [ HH.div [ HP.classes [ BS.colMd10, BS.offsetMd1 ] ]
                [ HH.form_
                    [ HH.fieldset_
                        [ HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.classes [ BS.formControl, BS.formControlLg ]
                                , HP.type_ HP.InputText
                                , HP.placeholder "Article Title"
                                , HE.onValueChange (Just <<< ChangeTitle)
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.class_ BS.formControl
                                , HP.type_ HP.InputText
                                , HP.placeholder "What's this article about?"
                                , HE.onValueChange (Just <<< ChangeDescription)
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.textarea
                                [ HP.class_ BS.formControl
                                , HP.rows 8
                                , HP.placeholder "Write your article (in markdown)"
                                , HE.onValueChange (Just <<< ChangeContent)
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.class_ BS.formControl
                                , HP.type_ HP.InputText
                                , HP.placeholder "Enter tags"
                                , HE.onValueChange (Just <<< ChangeTag)
                                ]
                            , HH.div [ HP.class_ C.tagList ] []
                            ]
                        , HH.button
                            [ HP.classes [ BS.btn, BS.btnLg, BS.btnPrimary, C.pullXsRight ]
                            , HE.onClick $ preventDefault Publish
                            ]
                            [ HH.text "Publish Article"
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
  where
  preventDefault action event = Just $ PreventDefault (toEvent event) $ Just action

handleAction ∷
  forall m.
  MonadAff m =>
  Action ->
  H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  PreventDefault event action -> Utils.preventDefault event action handleAction
  ChangeContent content -> H.modify_ _ { article { body = content } }
  ChangeDescription description -> H.modify_ _ { article { description = description } }
  ChangeTag tag -> H.modify_ _ { currentTag = tag }
  ChangeTitle title -> H.modify_ _ { article { title = title } }
  AddTag tag -> H.modify_ \s -> s { article { tagList = Just $ add tag s.article.tagList } }
  RemoveTag tag -> H.modify_ \s -> s { article { tagList = remove tag s.article.tagList } }
  Publish -> do
    { currentUser, article, currentAction } <- H.get
    H.modify_ _{ errorMessages = Nothing }
    case currentAction of
      Edit _ -> pure unit
      New -> do
        req <- liftAff $ API.request $ API.articleCreation { article } currentUser.token
        case req of
            Left err -> H.modify_ _ { errorMessages = Just err }
            Right art -> H.raise (Redirect art.article.slug)
  where
  add tag = maybe [ tag ] (_ `snoc` tag) >>> nub

  remove tag = maybe [] (filter (_ /= tag)) >>> \a -> if null a then Nothing else Just a
