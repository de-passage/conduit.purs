module Pages.Home where

import Prelude
import API as API
import Classes as C
import Control.Parallel (parSequence_, parTraverse_, parallel)
import Data.Article (Article)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tag (Tag(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Templates.ArticlePreview as ArticlePreview

type Query
  = Const Void

type Output
  = Void

type Slot
  = H.Slot Query Output

data LoadState a
  = Loading
  | LoadError String
  | Loaded a

type State
  = { tags :: LoadState (Array Tag)
    , articles :: LoadState (Array Article)
    }

data Action
  = Init

type ChildSlots
  = ()

component :: forall i m. MonadAff m => H.Component HH.HTML Query i Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Init
              }
    }

initialState :: forall i. i -> State
initialState _ = { articles: Loading, tags: Loading }

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
render state =
  let
    articles = case state.articles of
      Loading -> [ HH.text "Loading" ]
      Loaded as -> map ArticlePreview.render as
      LoadError error -> [ HH.div [ HP.class_ BS.alertDanger ] [ HH.text error ] ]

    tagList = case state.tags of
      Loading -> HH.div_ []
      Loaded tags ->
        HH.div [ HP.class_ C.sidebar ]
          [ HH.p_ [ HH.text "Popular Tags" ]
          , HH.div [ HP.class_ C.tagList ]
              ( map tagLink tags
              )
          ]
      LoadError error -> HH.div [ HP.class_ BS.alertDanger ] [ HH.text error ]
  in
    HH.div [ HP.class_ C.homePage ]
      [ HH.div [ HP.class_ C.banner ]
          [ HH.div [ HP.class_ BS.container ]
              [ HH.h1 [ HP.class_ C.logoFont ] [ HH.text "conduit" ]
              , HH.p_ [ HH.text "A place to share your knowledge." ]
              ]
          ]
      , HH.div [ HP.classes [ BS.container, C.page ] ]
          [ HH.div [ HP.class_ BS.row ]
              [ HH.div [ HP.class_ BS.colMd9 ]
                  [ HH.div [ HP.class_ C.feedToggle ]
                      ( [ HH.ul [ HP.classes [ BS.nav, BS.navPills, C.outlineActive ] ]
                            [ HH.li [ HP.class_ BS.navItem ]
                                [ HH.a [ HP.classes [ BS.navLink, BS.disabled ], HP.href "#/personalfeed" ]
                                    [ HH.text "Your Feed"
                                    ]
                                ]
                            , HH.li [ HP.class_ BS.navItem ]
                                [ HH.a [ HP.classes [ BS.navLink, BS.active ], HP.href "#/globalfeed" ]
                                    [ HH.text "Global Feed"
                                    ]
                                ]
                            ]
                        ]
                          <> articles
                      )
                  ]
              , HH.div [ HP.class_ BS.colMd3 ]
                  [ tagList
                  ]
              ]
          ]
      ]

tagLink :: forall w i. Tag -> HH.HTML w i
tagLink (Tag s) = HH.a [ HP.href "", HP.classes [ C.tagPill, C.tagDefault ] ] [ HH.text s ]

handleAction ∷
  forall o m.
  MonadAff m =>
  Action ->
  H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Init -> parSequence_ [ loadArticles, loadTags ]
  where
  load ::
    forall a.
    (Aff (Either String a)) ->
    (LoadState a -> State -> State) ->
    H.HalogenM State Action ChildSlots o m Unit
  load get set = do
    result <- liftAff $ get
    case result of
      (Left err) -> H.modify_ (set (LoadError err))
      (Right arts) -> H.modify_ (set (Loaded arts))

  loadArticles = load API.getArticles (\v -> _ { articles = v })

  loadTags = load API.getTags (\v -> _ { tags = v })
