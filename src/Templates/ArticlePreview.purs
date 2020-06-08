module Templates.ArticlePreview where

import Prelude
import Classes as C
import Data.Article (Article)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.User (fromImage)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Router (profileUrl, showArticleUrl)
import Web.UIEvent.MouseEvent (MouseEvent)

render :: forall w i. Article -> (MouseEvent -> Maybe i) -> HH.HTML w i
render article favorite =
  let
    username = unwrap article.author.username

    userUrl = profileUrl article.author.username

    btnStyle = if article.favorited then BS.btnPrimary else BS.btnOutlinePrimary
  in
    HH.div [ HP.class_ C.articlePreview ]
      [ HH.div [ HP.class_ C.articleMeta ]
          [ HH.a [ HP.href userUrl ] [ HH.img [ HP.src $ fromImage article.author.image ] ]
          , HH.div [ HP.class_ C.info ]
              [ HH.a [ HP.href userUrl, HP.class_ C.author ] [ HH.text username ]
              , HH.span [ HP.class_ C.date ] [ HH.text article.createdAt ]
              ]
          , HH.button [ HP.classes [ BS.btn, btnStyle, BS.btnSm, C.pullXsRight ], HE.onClick favorite ]
              [ HH.i [ HP.class_ C.ionHeart ] [], HH.text (" " <> show article.favoritesCount)
              ]
          , HH.a [ HP.href (showArticleUrl article.slug), HP.class_ C.previewLink ]
              [ HH.h1_ [ HH.text article.title ]
              , HH.p_ [ HH.text article.description ]
              , HH.span_ [ HH.text "Read more..." ]
              , HH.ul [ HP.class_ C.tagList ] (map mkTag article.tagList)
              ]
          ]
      ]
  where
  mkTag tag = HH.li [ HP.classes [ C.tagDefault, C.tagPill, C.tagOutline ] ] [ HH.text $ unwrap tag ]
