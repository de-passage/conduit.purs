module Navbar where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 (navItem)
import Halogen.Themes.Bootstrap4 as BS

render :: forall w i. HH.HTML w i
render =
  HH.nav
    [ HP.classes [ BS.navbar, BS.navbarLight ] ]
    [ HH.div [ HP.class_ BS.container ]
        [ HH.a [ HP.class_ BS.navbarBrand ] [ HH.text "conduit" ]
        , HH.ul [ HP.classes [ BS.nav, BS.navbarNav, HH.ClassName "pull-xs-right" ] ]
            [ HH.li [ HP.class_ navItem ]
                [ HH.a
                    [ HP.classes [ BS.navLink, BS.active ], HP.href "" ]
                    [ HH.text "Home" ]
                ]
            , HH.li [ HP.class_ navItem ]
                [ HH.a [ HP.class_ BS.navLink ]
                    [ HH.i [ HP.class_ (HH.ClassName "ion-compose") ] []
                    , HH.text " NewPost"
                    ]
                ]
            , HH.li [ HP.class_ navItem ]
                [ HH.a [ HP.class_ BS.navLink, HP.href "" ]
                    [ HH.i [ HP.class_ (HH.ClassName "ion-gear-a") ] []
                    , HH.text " Settings"
                    ]
                ]
            , HH.li [ HP.class_ navItem ]
                [ HH.a
                    [ HP.classes [ BS.navLink ], HP.href "" ]
                    [ HH.text "Sign up" ]
                ]
            ]
        ]
    ]