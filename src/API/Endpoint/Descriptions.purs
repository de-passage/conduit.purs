module API.Endpoint.Descriptions where

import Prelude
import API.Endpoint.Core as C
import API.Response as R
import Data.Maybe (Maybe)

type Empty auth r
  = C.Endpoint () auth r

make :: forall p a r. C.Endpoint p a r
make = C.EProxy

type LoginPayload
  = ( user :: { email :: String, password :: String } )

login = make :: C.Endpoint LoginPayload C.None R.UserResponse

type RegistrationPayload
  = ( user :: { email :: String, password :: String, username :: String } )

register = make :: C.Endpoint RegistrationPayload C.None R.UserResponse

currentUser = make :: Empty C.Required R.UserResponse

type UserUpdatePayload
  = ( user ::
        { email :: Maybe String
        , username :: Maybe String
        , password :: Maybe String
        , image :: Maybe String
        , bio :: Maybe String
        }
    )

updateUser = make :: C.Endpoint UserUpdatePayload C.Required R.UserResponse

profile = make :: Empty C.Optional R.ProfileResponse

follow = make :: Empty C.Required R.ProfileResponse

articles = make :: Empty C.Optional R.ArticlesResponse

feed = make :: Empty C.Required R.ArticlesResponse

article = make :: Empty C.Optional R.ArticleResponse

type ArticleCreationPayload
  = ( article ::
        { title :: String
        , description :: String
        , body :: String
        , tagList :: Maybe (Array String)
        }
    )

createArticle = make :: C.Endpoint ArticleCreationPayload C.Required R.ArticleResponse

type ArticleEditionPayload
  = ( article ::
        { title :: Maybe String
        , description :: Maybe String
        , body :: Maybe String
        -- , tagList :: Maybe (Array String) -- can't edit the tag list apparently
        }
    )

editArticle = make :: C.Endpoint ArticleEditionPayload C.Required R.ArticleResponse

deleteArticle = make :: Empty C.Required Unit

type CommentPayload
  = ( comment :: { body :: String } )

comment = make :: C.Endpoint CommentPayload C.Required R.CommentResponse

comments = make :: Empty C.Optional R.CommentsResponse

deleteComment = make :: Empty C.Required Unit

favorite = make :: Empty C.Required R.ArticleResponse

tags = make :: Empty C.None R.TagsResponse
