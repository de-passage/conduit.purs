module Data.GlobalState where

import API.Url (UrlRepository)
import Data.Article (PageNumber, PerPage)
import Data.Maybe (Maybe)
import Data.User (User)
import Router (Route)

type State
  = Record
      ( WithCommon
          ( currentRoute :: Route
          , perPage :: PerPage
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
  = ( perPage :: PerPage, pageNumber :: PageNumber | r )
