module API.Endpoint where

--import Prelude
import Prelude
import API.Response as R
import API.Endpoint.Descriptions as D
import API.Url as Url
import API.Endpoint.Core as C
import Affjax as AJ
import Affjax.RequestBody as AJRB
import Affjax.ResponseFormat as AJRF
import Data.Argonaut as A
import Data.Article (Slug(..))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Token (authorize, Token) as T
import Type.Data.Row (RProxy(..))
import Type.Proxy (Proxy(..))

type ResponseBody r
  = Either String r

type EndpointResponse a
  = AJ.Response A.Json

login :: Record D.LoginPayload -> C.Request
login = C.create D.login Url.login POST

registation :: Record D.RegistrationPayload -> C.Request
registation = C.create D.register Url.register POST

tags :: C.Request
tags = C.create_ D.tags Url.tags GET

article :: Slug -> Maybe T.Token -> C.Request
article s = C.create_ D.article (Url.article s) GET