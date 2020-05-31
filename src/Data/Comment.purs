module Data.Comment where

import Data.User (Profile)

type Comment
  = { id :: Int
    , createdAt :: String
    , updatedAt :: String
    , body :: String
    , author :: Profile
    }
