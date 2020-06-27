module Data.GlobalState where

import API.Url (UrlRepository)
import Data.Maybe (Maybe)
import Data.User (User)
import Router (Route)

type State
  = Record
      ( Paginated
          ( WithCommon
              ( currentRoute :: Route
              )
          )
      )

type WithUser r
  = ( currentUser :: Maybe User
    | r
    )

type WithUrls r
  = ( urls :: UrlRepository
    | r
    )

type WithCommon r
  = WithUser (WithUrls r)

type Paginated r
  = ( perPage :: Int | r )
