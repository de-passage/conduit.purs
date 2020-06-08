module Templates.Navbar where

import Prelude

import Classes as C
import Data.Array (snoc)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.User (User)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Router (homeUrl, newArticleUrl, profileUrl, registerUrl, settingsUrl, loginUrl)

type Input
  = Maybe User

render :: forall w i. Input -> HH.HTML w i
render user =
  let
    navItem classes cont url =
      HH.li [ HP.class_ BS.navItem ]
        [ HH.a
            [ HP.classes (classes `snoc` BS.navLink), HP.href url ]
            cont
        ]

    home =
      navItem [ BS.active ]
        [ HH.text "Home" ]
        homeUrl

    newArticle =
      navItem []
        [ HH.i [ HP.class_ C.ionCompose ] []
        , HH.text " NewPost"
        ]
        newArticleUrl

    settings =
      navItem []
        [ HH.i [ HP.class_ C.ionGearA ] []
        , HH.text " Settings"
        ]
        settingsUrl

    signUp = navItem [] [ HH.text "Sign up" ] registerUrl

    signIn = navItem [] [ HH.text "Sign in" ] loginUrl

    profile name = navItem [] [ HH.text $ unwrap name ] $ profileUrl name

    content = case user of
      Nothing ->
        [ home
        , signUp
        , signIn
        ]
      Just u ->
        [ home
        , newArticle
        , settings
        , profile u.username
        ]
  in
    HH.nav
      [ HP.classes [ BS.navbar, BS.navbarLight ] ]
      [ HH.div [ HP.class_ BS.container ]
          [ HH.a [ HP.class_ BS.navbarBrand, HP.href homeUrl ] [ HH.text "conduit" ]
          , HH.ul [ HP.classes [ BS.nav, BS.navbarNav, C.pullXsRight ] ] content
          ]
      ]
