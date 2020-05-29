module Pages.Article where

import Classes as C
import Data.User (Username(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Router (profileUrl)

render :: forall w i. HH.HTML w i
render =
  HH.div [ HP.class_ C.articlePage ]
    [ HH.div [ HP.class_ C.banner ]
        [ HH.div [ HP.class_ BS.container ]
            [ HH.h1_ [ HH.text "How to build webapps that scale" ]
            , HH.div [ HP.class_ C.articleMeta ]
                [ HH.a [ HP.href (profileUrl (Username "whatever")) ]
                    [ HH.img [ HP.src "http://i.imgur.com/Qr71crq.jpg" ] ]
                , HH.div [ HP.class_ C.info ]
                    [ HH.a [ HP.href (profileUrl (Username "whatever")), HP.class_ C.author ] [ HH.text "Eric Simons" ]
                    , HH.span [ HP.class_ C.date ] [ HH.text "January 20th" ]
                    ]
                , HH.button [ HP.classes [ BS.btn, BS.btnSm, BS.btnOutlineSecondary ] ]
                    [ HH.i [ HP.class_ C.ionPlusRound ] []
                    , HH.text " Follow Eric Simons "
                    , HH.span [ HP.class_ C.counter ] [ HH.text "(10)" ]
                    ]
                , HH.text "  "
                , HH.button [ HP.classes [ BS.btn, BS.btnSm, BS.btnOutlinePrimary ] ]
                    [ HH.i [ HP.class_ C.ionHeart ] []
                    , HH.text " Favorite Pose "
                    , HH.span [ HP.class_ C.counter ] [ HH.text "(29) " ]
                    ]
                ]
            ]
        ]
    , HH.div [ HP.classes [ BS.container, C.page ] ]
        [ HH.div [ HP.classes [ BS.row, C.articleContent ] ]
            [ HH.div [ HP.class_ BS.colMd12 ]
                [ HH.p_
                    [ HH.text "Web development technologies have evolved at an incredible clip over the past few years."
                    ]
                , HH.h2 [ HP.id_ "introducing-ionic" ] [ HH.text "Introducing RealWorld." ]
                , HH.p_
                    [ HH.text "It's a great solution for learning how other frameworks work."
                    ]
                ]
            ]
        , HH.hr_
        , HH.div [ HP.class_ C.articleActions ]
            [ HH.div [ HP.class_ C.articleMeta ]
                [ HH.a [ HP.href (profileUrl (Username "whatever")) ] [ HH.img [ HP.src "http://i.imgur.com/Qr71crq.jpg" ] ]
                , HH.div [ HP.class_ C.info ]
                    [ HH.a [ HP.href (profileUrl (Username "whatever")), HP.class_ C.author ] [ HH.text "Eric Simons" ]
                    , HH.span [ HP.class_ C.date ] [ HH.text "January 20th" ]
                    ]
                , HH.button [ HP.classes [ BS.btn, BS.btnSm, BS.btnOutlineSecondary ] ]
                    [ HH.i [ HP.class_ C.ionPlusRound ] []
                    , HH.text " Follow Eric Simons "
                    , HH.span [ HP.class_ C.counter ] [ HH.text "(10)" ]
                    ]
                , HH.text " "
                , HH.button [ HP.classes [ BS.btn, BS.btnSm, BS.btnOutlinePrimary ] ]
                    [ HH.i [ HP.class_ C.ionHeart ] []
                    , HH.text " Favorite Post "
                    , HH.span [ HP.class_ C.counter ] [ HH.text "(20)" ]
                    ]
                ]
            ]
        , HH.div [ HP.class_ BS.row ]
            [ HH.div [ HP.classes [ C.colXs12, BS.colMd8, BS.offsetMd2 ] ]
                [ HH.form [ HP.classes [ BS.card, C.commentForm ] ]
                    [ HH.div [ HP.class_ C.cardBlock ]
                        [ HH.textarea [ HP.class_ BS.formControl, HP.rows 3, HP.placeholder "Write a comment..." ]
                        ]
                    , HH.div [ HP.class_ BS.cardFooter ]
                        [ HH.img [ HP.src "http://i.imgur.com/Qr71crq.jpg", HP.class_ C.commentAuthorImg ]
                        , HH.button [ HP.classes [ BS.btn, BS.btnSm, BS.btnPrimary ] ]
                            [ HH.text "Post Comment"
                            ]
                        ]
                    ]
                , HH.div [ HP.class_ BS.card ]
                    [ HH.div [ HP.class_ C.cardBlock ]
                        [ HH.p [ HP.class_ BS.cardText ] [ HH.text "With supporting text below as a natural lead-in to additional content." ]
                        ]
                    , HH.div [ HP.class_ BS.cardFooter ]
                        [ HH.a [ HP.href (profileUrl (Username "whatever")), HP.class_ C.commentAuthor ]
                            [ HH.img [ HP.src "http://i.imgur.com/Qr71crq.jpg", HP.class_ C.commentAuthorImg ]
                            ]
                        , HH.text " "
                        , HH.a [ HP.href (profileUrl (Username "whatever")), HP.class_ C.commentAuthor ] [ HH.text "John Schmidt" ]
                        , HH.span [ HP.class_ C.datePosted ] [ HH.text "Dec 29th" ]
                        ]
                    ]
                , HH.div [ HP.class_ BS.card ]
                    [ HH.div [ HP.class_ C.cardBlock ]
                        [ HH.p [ HP.class_ BS.cardText ] [ HH.text "With supporting text below as a natural lead-in to additional content." ]
                        ]
                    , HH.div [ HP.class_ BS.cardFooter ]
                        [ HH.a [ HP.href (profileUrl (Username "whatever")), HP.class_ C.commentAuthor ]
                            [ HH.img [ HP.src "http://i.imgur.com/Qr71crq.jpg", HP.class_ C.commentAuthorImg ]
                            ]
                        , HH.text " "
                        , HH.a [ HP.href (profileUrl (Username "whatever")), HP.class_ C.commentAuthor ] [ HH.text "Jacob Schmidt" ]
                        , HH.span [ HP.class_ C.datePosted ] [ HH.text "Dec 29th" ]
                        , HH.span [ HP.class_ C.modOptions ]
                            [ HH.i [ HP.class_ C.ionEdit ] []
                            , HH.i [ HP.class_ C.ionTrashA ] []
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
