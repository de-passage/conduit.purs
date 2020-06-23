module API.Utils where

import Prelude
import API.Response as R
import Control.Lazy (fix)
import Data.Argonaut as A
import Data.Array (concatMap)
import Data.Either (Either, either)
import Data.Tuple.Nested ((/\))
import Foreign.Object as F

type ErrorContainer
  = { errors :: A.Json }

parseAPIErrors :: A.Json -> Array R.ValidationError
parseAPIErrors json =
  (A.decodeJson json :: Either String ErrorContainer)
    # ( either
          (\s -> [ { name: "API", errors: [ "Unexpected error format received (" <> s <> "). Content: " <> A.stringify json ] } ])
          $ \j ->
              A.caseJson
                (\_ -> [])
                (\boolean -> [ { name: show boolean, errors: [] } ])
                (\number -> [ { name: show number, errors: [] } ])
                (\str -> [ { name: str, errors: [] } ])
                (\array -> concatMap parseAPIErrors array)
                (\object -> F.toUnfoldable object # map (\(n /\ o) -> { name: n, errors: parseObject o }))
                j.errors
      )
  where
  parseObject :: A.Json -> Array String
  parseObject =
    fix \self ->
      A.caseJson
        (\_ -> [])
        (\boolean -> [ show boolean ])
        (\number -> [ show number ])
        (\str -> [ str ])
        (concatMap self)
        (F.toUnfoldable >>> map (\(n /\ o) -> n <> A.stringify o))
