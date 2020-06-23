module API.Response where

import Prelude
import Data.Article (Article)
import Data.Comment (Comment)
import Data.Either (Either)
import Data.Foldable (intercalate)
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

type StandardError
  = { name :: String, errors :: Array String }

type ValidationError
  = StandardError

type InternalError
  = StandardError

data Error
  = ValidationFailed (Array ValidationError)
  | Unauthorized
  | Forbidden
  | NotFound
  | AjaxFailed String
  | ParseError String
  | APIError Int
  | InternalServerError (Array InternalError)

type Response a
  = Either Error a

fromError :: Error -> Array String
fromError (ValidationFailed arr) = map (\{ name, errors } -> name <> " " <> intercalate " or " errors) arr

fromError (InternalServerError arr) = map (\{ name, errors } -> name <> " " <> intercalate " or " errors) arr

fromError Unauthorized = [ "401 Unauthorized: You need to authenticate to access this resource." ]

fromError Forbidden = [ "403 Forbidden: You are not allowed to access this resource." ]

fromError NotFound = [ "404 Not Found: The requested resource doesn't exist." ]

fromError (AjaxFailed error) = [ "Remote request failed: " <> error ]

fromError (ParseError error) = [ "API response had invalid format: " <> error ]

fromError (APIError errorCode) = [ "API responded with an undocumented error code: " <> show errorCode ]
