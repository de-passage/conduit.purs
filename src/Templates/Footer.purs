module Templates.Footer where

import Classes as C
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Router (homeUrl)

render :: forall w i. HH.HTML w i
render =
  HH.footer_
    [ HH.div [ HP.class_ BS.container ]
        [ HH.a [ HP.href homeUrl, HP.class_ C.logoFont ]
            [ HH.text "conduit" ]
        , HH.span [ HP.class_ C.attribution ]
            [ HH.text "An interactive learning project from "
            , HH.a [ HP.href "https://thinkster.io" ] [ HH.text "Thinkster" ]
            , HH.text ". Code & design licensed under MIT."
            ]
        ]
    ]