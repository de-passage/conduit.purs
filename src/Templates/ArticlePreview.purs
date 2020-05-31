module Templates.ArticlePreview where

import Prelude

import Classes as C
import Data.Article (Article)
import Data.Newtype (unwrap)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Router (profileUrl, showArticleUrl)

render :: forall w i. Article -> HH.HTML w i
render article =
  let
    username = unwrap article.author.username
    userUrl = profileUrl article.author.username
  in
    HH.div [ HP.class_ C.articlePreview ]
      [ HH.div [ HP.class_ C.articleMeta ]
          [ HH.a [ HP.href userUrl ] [ HH.img [ HP.src article.author.image ] ]
          , HH.div [ HP.class_ C.info ]
              [ HH.a [ HP.href userUrl, HP.class_ C.author ] [ HH.text username ]
              , HH.span [ HP.class_ C.date ] [ HH.text article.createdAt ]
              ]
          , HH.button [ HP.classes [ BS.btn, BS.btnOutlinePrimary, BS.btnSm, C.pullXsRight ] ]
              [ HH.i [ HP.class_ C.ionHeart ] [], HH.text (" " <> show article.favoritesCount)
              ]
          ]
      , HH.a [ HP.href (showArticleUrl article.slug), HP.class_ C.previewLink ]
          [ HH.text article.description
          ]
      ]
