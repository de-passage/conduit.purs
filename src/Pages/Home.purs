module Pages.Home where

import Prelude

import Classes as C
import Data.Article (Slug(..))
import Data.Const (Const(..))
import Data.User (Username(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Router (profileUrl, showArticleUrl)

type Query = Const Void
type Output = Void
type Slot = H.Slot Query Output
type State = Unit
type Action = Unit
type ChildSlots = ()

component :: forall i m. MonadAff m => H.Component HH.HTML Query i Output m 
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = unit

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
render state =
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
                    [ HH.ul [ HP.classes [ BS.nav, BS.navPills, C.outlineActive ] ]
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
                , HH.div [ HP.class_ C.articlePreview ]
                    [ HH.div [ HP.class_ C.articleMeta ]
                        [ HH.a [ HP.href (profileUrl (Username "whatever")) ] [ HH.img [ HP.src "http://i.imgur.com/Qr71crq.jpg" ] ]
                        , HH.div [ HP.class_ C.info ]
                            [ HH.a [ HP.href (profileUrl (Username "whatever")), HP.class_ C.author ] [ HH.text "Eric Simons" ]
                            , HH.span [ HP.class_ C.date ] [ HH.text "January 20th" ]
                            ]
                        , HH.button [ HP.classes [ BS.btn, BS.btnOutlinePrimary, BS.btnSm, C.pullXsRight ] ]
                            [ HH.i [ HP.class_ C.ionHeart ] [], HH.text " 29"
                            ]
                        ]
                    , HH.a [ HP.href (showArticleUrl (Slug "whatever")), HP.class_ C.previewLink ]
                        [ HH.h1_ [ HH.text "How to build webapps that scale" ]
                        , HH.p_ [ HH.text "This is the description for the post." ]
                        , HH.span_ [ HH.text "Read more..." ]
                        ]
                    ]
                , HH.div [ HP.class_ C.articlePreview ]
                    [ HH.div [ HP.class_ C.articleMeta ]
                        [ HH.a [ HP.href (profileUrl (Username "whatever")) ] [ HH.img [ HP.src "http://i.imgur.com/N4VcUeJ.jpg" ] ]
                        , HH.div [ HP.class_ C.info ]
                            [ HH.a [ HP.href (profileUrl (Username "whatever")), HP.class_ C.author ] [ HH.text "Albert Pai" ]
                            , HH.span [ HP.class_ C.date ] [ HH.text "January 20th" ]
                            ]
                        , HH.button [ HP.classes [ BS.btn, BS.btnOutlinePrimary, BS.btnSm, C.pullXsRight ] ]
                            [ HH.i [ HP.class_ C.ionHeart ] [], HH.text " 32"
                            ]
                        ]
                    , HH.a [ HP.href (showArticleUrl (Slug "whatever")), HP.class_ C.previewLink ]
                        [ HH.h1_ [ HH.text "The song you won't ever stop singing. No matter how hard you try." ]
                        , HH.p_ [ HH.text "This is the description for the post." ]
                        , HH.span_ [ HH.text "Read more..." ]
                        ]
                    ]
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

handleAction ∷ forall o m. Action → H.HalogenM State Action () o m Unit
handleAction _ = pure unit

