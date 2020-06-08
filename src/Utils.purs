module Utils where

import Prelude

import API as API
import API.Response (Error, fromError)
import Classes as C
import Data.Article (Article)
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.String as S
import Data.Token (Token)
import Data.User (Profile)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import LoadState (LoadState(..))
import Unsafe.Coerce as Unsafe
import Web.Event.Event (Event, preventDefault) as W
import Web.UIEvent.MouseEvent (MouseEvent)

preventDefault ::
  forall s a c o m.
  MonadEffect m =>
  W.Event -> Maybe a -> (a -> H.HalogenM s a c o m Unit) -> H.HalogenM s a c o m Unit
preventDefault event action handle = do
  H.liftEffect $ W.preventDefault event
  maybe (pure unit) handle action

log ::
  forall s a c o m.
  MonadEffect m =>
  String -> H.HalogenM s a c o m Unit
log = H.liftEffect <<< Console.log

unsafeLog ::
  forall s a c o m i.
  MonadEffect m =>
  i -> H.HalogenM s a c o m Unit
unsafeLog = H.liftEffect <<< Console.log <<< Unsafe.unsafeCoerce

follow ::
  forall a c o s m.
  MonadAff m =>
  Profile ->
  Token ->
  (LoadState Profile -> s -> s) ->
  H.HalogenM s a c o m Unit
follow profile token update = do
  request <-
    H.liftAff
      $ API.request
          ( ( if profile.following then
                API.unfollow
              else
                API.follow
            )
              profile.username
              token
          )
  case request of
    Left _ -> pure unit
    Right prof -> H.modify_ $ update $ Loaded prof.profile

favorite ::
  forall a c o s m f.
  MonadAff m =>
  Functor f =>
  Article ->
  Token ->
  (LoadState (f Article) -> s -> s) ->
  (s -> LoadState (f Article)) ->
  H.HalogenM s a c o m Unit
favorite article token update retrieve = do
  request <-
    H.liftAff
      $ API.request
          ( ( if article.favorited then
                API.unfavorite
              else
                API.favorite
            )
              article.slug
              token
          )
  case request of
    Left _ -> pure unit
    Right art -> H.modify_ $ replace art.article update retrieve

replace :: forall s f. Functor f => Article -> (LoadState (f Article) -> s -> s) -> (s -> LoadState (f Article)) -> s -> s
replace article update retrieve state = case retrieve state of
  Loaded arts ->
    update
      ( Loaded $ arts
          # map \a ->
              if a.slug == article.slug then article else a
      )
      state
  _ -> state

errorDisplay :: forall w i. Error -> HH.HTML w i
errorDisplay errors =
  HH.ul_
    ( fromError errors
        # map \msg ->
            HH.li [ HP.classes [ C.errorMessages ] ] [ HH.text msg ]
    )


followButtonC :: forall w i. Array H.ClassName -> (Profile -> MouseEvent -> Maybe w) -> Profile -> HH.HTML i w
followButtonC cs action profile =
  HH.button
    [ HP.classes
        ([ BS.btn
        , if profile.following then BS.btnOutlineSecondary else BS.btnOutlinePrimary
        , C.actionBtn
        ] <> cs)
    , HE.onClick $ action profile
    ]
    [ HH.i [ HP.class_ C.ionPlusRound ] []
    , HH.text $ (if profile.following then " Unfollow " else " Follow ") <> unwrap profile.username
    ]
    
followButton :: forall w i. (Profile -> MouseEvent -> Maybe w) -> Profile -> HH.HTML i w
followButton = followButtonC []

hackyFormatDate :: String -> String
hackyFormatDate date = 
  let year = S.take 4 date
      month = f $ S.take 2 $ S.drop 5 date 
      day = d $ S.take 2 $ S.drop 8 date

      f "01" = "January"
      f "02" = "February"
      f "03" = "March"
      f "04" = "April"
      f "05" = "May"
      f "06" = "June"
      f "07" = "July"
      f "08" = "August"
      f "09" = "September"
      f "10" = "October"
      f "11" = "November"
      f "12" = "December"
      f s = s

      d s = S.dropWhile (_ == S.codePointFromChar '0') s <> "."
  in 
    month <> " " <>  day <> " " <> year
