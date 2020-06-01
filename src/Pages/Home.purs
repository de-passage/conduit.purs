module Pages.Home where

import Prelude
import API as API
import Classes as C
import Data.Article (Article)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
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

data State
  = Loading
  | LoadError String
  | Loaded (Array Article)

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
initialState _ = Loading

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
render state =
  let
    articles = case state of
      Loading -> [ HH.text "Loading" ]
      Loaded as -> map ArticlePreview.render as
      LoadError error -> [ HH.div [ HP.class_ BS.alertDanger ] [ HH.text error ] ]
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
                  [ HH.div [ HP.class_ C.sidebar ]
                      [ HH.p_ [ HH.text "Popular Tags" ]
                      , HH.div [ HP.class_ C.tagList ]
                          ( map tagLink
                              [ "programming"
                              , "javascript"
                              , "emberjs"
                              , "angularjs"
                              , "react"
                              , "mean"
                              , "node"
                              , "rails"
                              ]
                          )
                      ]
                  ]
              ]
          ]
      ]

tagLink :: forall w i. String -> HH.HTML w i
tagLink s = HH.a [ HP.href "", HP.classes [ C.tagPill, C.tagDefault ] ] [ HH.text s ]

handleAction ∷ forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Init -> do
    result <- liftAff $ API.getArticles
    case result of
      (Left err) -> H.put (LoadError err)
      (Right arts) -> H.put (Loaded arts.articles)
