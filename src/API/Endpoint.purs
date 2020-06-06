module API.Endpoint where

--import Prelude
import Prelude
import API.Response as R
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

type LoginPayload
  = ( email :: String, password :: String )

loginE = C.EProxy :: C.Endpoint LoginPayload C.None R.UserResponse

type RegistrationPayload
  = ( email :: String, password :: String, name :: String )

registerE = C.EProxy :: C.Endpoint RegistrationPayload C.None R.UserResponse

-- payload :: forall payload auth resp. Endpoint payload auth resp -> RProxy payload
-- payload _ = RProxy


login :: Record LoginPayload -> C.Request
login = C.create loginE Url.login POST

registation :: Record RegistrationPayload -> C.Request
registation = C.create registerE Url.register POST

tags :: C.Request
tags = C.create_ (C.describe C.noPayload C.noAuth R.tags) Url.tags GET

slug :: Slug -> Maybe T.Token -> C.Request
slug s = C.create_ (C.describe C.noPayload C.authOptional R.article) (Url.article s) GET
 {- 
class AddPayload (payload :: # Type) func | func -> payload

instance addNoPayload :: AddPayload () Request

instance addGenericPayload :: (A.EncodeJson (Record p)) => AddPayload p (Record p -> Request)

withPayload :: forall p m. Monad m => (A.EncodeJson (Record p)) => RProxy p -> m Request -> m (Record p -> Request)
withPayload _ r = do
  (Request req) <- r
  pure \record -> Request $ req { content = Just $ AJRB.Json $ A.encodeJson record }
-}