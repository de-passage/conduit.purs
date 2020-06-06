module API.Response where

import Data.Article (Article)
import Data.Comment (Comment)
import Data.Either (Either)
import Data.Tag (Tag)
import Data.User (Profile, User)
import Type.Proxy (Proxy(..))

type ProfileResponse
  = { profile :: Profile
    }

profile = Proxy :: Proxy ProfileResponse

type ArticleResponse
  = { article :: Article
    }

article = Proxy :: Proxy ArticleResponse

type ArticlesResponse
  = { articles :: Array Article
    }

articles = Proxy :: Proxy ArticlesResponse

type CommentResponse
  = { comment :: Comment
    }

comment = Proxy :: Proxy CommentResponse

type CommentsResponse
  = { comments :: Array Comment }

comments = Proxy :: Proxy CommentsResponse

type TagsResponse
  = { tags :: Array Tag }

tags = Proxy :: Proxy TagsResponse

type UserResponse
  = { user :: User }

user = Proxy :: Proxy UserResponse

type DecodedResponse a
  = Either String a
