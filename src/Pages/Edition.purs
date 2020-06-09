module Pages.Edition where

import Prelude

import API as API
import API.Response (Error)
import Classes as C
import Data.Array (filter, nub, snoc)
import Data.Article (Slug)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Symbol (SProxy(..))
import Data.User (User)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import SimpleMDE as SimpleMDE
import Utils as Utils
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (toEvent)

data Action
  = Init
  | Receive Input
  | ChangeTitle String
  | ChangeDescription String
  | ChangeContent String
  | ChangeTag String
  | AddTag
  | RemoveTag String
  | Publish
  | PreventDefault Event (Maybe Action)

type State
  = { currentUser :: User
    , currentAction :: EditionType
    , article ::
        { description :: String
        , title :: String
        , tagList :: Array String
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
  = ( textEditor :: SimpleMDE.Slot Unit )

_textEditor = SProxy :: SProxy "textEditor"

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
              , receive = Just <<< Receive
              , initialize = Just Init
              }
    }

initialState :: Input -> State
initialState { currentUser, currentAction } =
  { currentUser: currentUser
  , currentAction: currentAction
  , article:
      { title: ""
      , tagList: []
      , description: ""
      }
  , currentTag: ""
  , errorMessages: Nothing
  }

render :: forall m. MonadAff m => State -> HH.ComponentHTML Action ChildSlots m
render state =
  HH.div [ HP.class_ C.editorPage ]
    [ HH.div [ HP.classes [ BS.container, C.page ] ]
        [ HH.div [ HP.class_ BS.row ]
            [ HH.div [ HP.classes [ BS.colMd10, BS.offsetMd1 ] ]
                [ maybe (HH.div_ []) Utils.errorDisplay state.errorMessages
                , HH.form_
                    [ HH.fieldset_
                        [ HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.classes [ BS.formControl, BS.formControlLg ]
                                , HP.type_ HP.InputText
                                , HP.placeholder "Article Title"
                                , HE.onValueChange (Just <<< ChangeTitle)
                                , HP.value state.article.title
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.class_ BS.formControl
                                , HP.type_ HP.InputText
                                , HP.placeholder "What's this article about?"
                                , HE.onValueChange (Just <<< ChangeDescription)
                                , HP.value state.article.description
                                ]
                            ]
                        , HH.slot _textEditor unit SimpleMDE.component
                            { placeholder: "Write your article here (in markdown)...", contextName: "article-body" }
                            (const Nothing)
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.div [ HP.class_ BS.formInline ]
                                [ HH.input
                                    [ HP.class_ BS.formControl
                                    , HP.type_ HP.InputText
                                    , HP.placeholder "Add a tag"
                                    , HE.onValueChange (Just <<< ChangeTag)
                                    , HP.value state.currentTag
                                    ]
                                , HH.button [ HP.classes [ BS.btn, BS.btnPrimary ], HE.onClick $ preventDefault AddTag ]
                                    [ HH.i [ HP.class_ C.ionAdd ] []
                                    ]
                                ]
                            , HH.div_ (map mkTag state.article.tagList)
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

  mkTag tag =
    HH.button
      [ HP.classes [ BS.btn, BS.btnOutlineInfo ]
      , HE.onClick $ preventDefault $ RemoveTag tag
      ]
      [ HH.span_ [ HH.text $ tag <> "     " ]
      , HH.i [ HP.class_ C.ionDelete ] []
      ]

handleAction ∷
  forall m.
  MonadAff m =>
  Action ->
  H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  Init -> do
    { currentAction, currentUser } <- H.get
    handleAction $ Receive { currentAction, currentUser }
  Receive { currentAction, currentUser } -> case currentAction of
    New -> pure unit
    Edit slug -> do
      req <- H.liftAff $ API.request $ API.article slug (Just currentUser.token)
      case req of
        Left err -> H.modify_ _ { errorMessages = Just err }
        Right { article } -> do
          void $ H.query _textEditor unit (H.request $ SimpleMDE.SetContent article.body)
          H.modify_
            _
              { article
                { description = article.description
                , tagList = article.tagList <#> show
                , title = article.title
                }
              }
  PreventDefault event action -> Utils.preventDefault event action handleAction
  ChangeContent content -> pure unit --H.modify_ _ { article { body = content } }
  ChangeDescription description -> H.modify_ _ { article { description = description } }
  ChangeTag tag -> H.modify_ _ { currentTag = tag }
  ChangeTitle title -> H.modify_ _ { article { title = title } }
  AddTag -> H.modify_ \s -> s { article { tagList = add s.currentTag s.article.tagList }, currentTag = "" }
  RemoveTag tag -> H.modify_ \s -> s { article { tagList = remove tag s.article.tagList } }
  Publish -> do
    { currentUser, article, currentAction } <- H.get
    body <- H.query _textEditor unit (H.request SimpleMDE.GetContent)
    let
      nart =
        { article:
            { body: fromMaybe "" body
            , title: article.title
            , description: article.description
            , tagList: article.tagList
            }
        }
    H.modify_ _ { errorMessages = Nothing }
    case currentAction of
      Edit slug -> request $ API.articleEdition slug nart currentUser.token
      New -> request $ API.articleCreation nart currentUser.token
  where
  add tag list = if tag /= "" then list `snoc` tag # nub else list

  remove tag = filter (_ /= tag)

  request r = do
    req <- liftAff $ API.request r
    case req of
      Left err -> H.modify_ _ { errorMessages = Just err }
      Right art -> H.raise (Redirect art.article.slug)
