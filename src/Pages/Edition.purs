module Pages.Edition where

import Prelude
import Classes as C
import Data.Article (Slug)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.User (User)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS

data Action
  = ChangeTitle String
  | ChangeDescription String
  | ChangeContent String
  | ChangeTag String
  | AddTag String
  | Publish

type State
  = { currentUser :: User
    , currentAction :: EditionType
    , article ::
        { description :: String
        , body :: String
        , title :: String
        , tagList :: Maybe (Array String)
        }
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

type Output
  = Void

type Query
  = Const Void

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
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.class_ BS.formControl
                                , HP.type_ HP.InputText
                                , HP.placeholder "What's this article about?"
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.textarea
                                [ HP.class_ BS.formControl
                                , HP.rows 8
                                , HP.placeholder "Write your article (in markdown)"
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.class_ BS.formControl
                                , HP.type_ HP.InputText
                                , HP.placeholder "Enter tags"
                                ]
                            , HH.div [ HP.class_ C.tagList ] []
                            ]
                        , HH.button [ HP.classes [ BS.btn, BS.btnLg, BS.btnPrimary, C.pullXsRight ] ]
                            [ HH.text "Publish Article"
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

handleAction ∷
  forall m.
  MonadAff m =>
  Action ->
  H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  _ -> pure unit
