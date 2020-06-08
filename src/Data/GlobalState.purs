module Data.GlobalState where

import Data.Maybe (Maybe)
import Data.User (User)
import Router (Route)

type State = 
    { currentRoute :: Route
    , currentUser :: Maybe User
    }