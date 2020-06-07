module API.Utils where

import Prelude
import API.Response as R
import Control.Lazy (fix)
import Data.Argonaut as A
import Data.Array (concat, concatMap)
import Data.Tuple.Nested ((/\))
import Foreign.Object (Object)
import Foreign.Object as F

parseValidationErrors :: A.Json -> Array R.ValidationError
parseValidationErrors =
  A.caseJson
    (\_ -> [])
    (\boolean -> [ { name: show boolean, errors: [] } ])
    (\number -> [ { name: show number, errors: [] } ])
    (\str -> [ { name: str, errors: [] } ])
    (\array -> concatMap parseValidationErrors array)
    (\object -> F.toUnfoldable object # map (\(n /\ o) -> { name: n, errors: parseObject o }))
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
