module API.Endpoint.Core where

import Prelude
import Affjax as AJ
import Affjax.RequestBody as AJRB
import Affjax.ResponseFormat as AJRF
import Data.Argonaut as A
import Data.Either (Either(..))
import Data.HTTP.Method (Method)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Token as T
import API.Url as Url
import Type.Data.Row (RProxy(..))
import Type.Proxy (Proxy)

newtype Request
  = Request (AJ.Request A.Json)

derive instance newtypeRequest :: Newtype Request _

foreign import kind AuthRequirement

foreign import data None :: AuthRequirement

foreign import data Optional :: AuthRequirement

foreign import data Required :: AuthRequirement

foreign import kind EndpointDescription

foreign import data Describe :: # Type -> AuthRequirement -> Type -> EndpointDescription

data AuthProxy (a :: AuthRequirement)
  = AuthProxy

noAuth = AuthProxy :: AuthProxy None

authOptional = AuthProxy :: AuthProxy Optional

authRequired = AuthProxy :: AuthProxy Required

data EProxy (p :: EndpointDescription)
  = EProxy

type Endpoint payload auth response
  = EProxy (Describe payload auth response)

noPayload = RProxy :: RProxy ()

describe ::
  forall auth payload response.
  RProxy payload -> AuthProxy auth -> Proxy response -> Endpoint payload auth response
describe _ _ _ = EProxy

auth :: forall payload auth resp. Endpoint payload auth resp -> AuthProxy auth
auth _ = AuthProxy

class AddAuthentication (auth :: AuthRequirement) func | auth -> func where
  addAuth :: AuthProxy auth -> Request -> func

instance addAuthNone :: AddAuthentication None Request where
  addAuth _ = identity

instance addAuthOptional :: AddAuthentication Optional (Maybe T.Token -> Request) where
  addAuth _ req@(Request r) token = case token of
    Nothing -> req
    Just t -> Request $ T.authorize r t

instance addAuthRequired :: AddAuthentication Required (T.Token -> Request) where
  addAuth _ (Request r) t = Request $ T.authorize r t

defaultRequest :: forall a. A.EncodeJson a => Url.Url -> Method -> a -> Request
defaultRequest url method payload =
  Request
    $ AJ.defaultRequest
        { method = Left method
        , url = show url
        , responseFormat = AJRF.json
        , content = Just $ AJRB.Json $ A.encodeJson payload
        }

create ::
  forall payload auth response result.
  AddAuthentication auth result =>
  A.EncodeJson (Record payload) =>
  Endpoint payload auth response -> Url.Url -> Method -> Record payload -> result
create description url method payload = defaultRequest url method payload # addAuth (auth description)

create_ ::
  forall auth response result.
  AddAuthentication auth result =>
  Endpoint () auth response -> Url.Url -> Method -> result
create_ d u m = create d u m {}
