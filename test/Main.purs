module Test.Main where

import Prelude

import API.Response (ValidationError)
import Data.Argonaut as A
import Data.Either (Either, either)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import API.Utils as Utils

type FixtureFile = { test1 :: A.Json, test2 :: A.Json }

main :: Effect Unit
main = do
  file <- readTextFile UTF8 "./test/error-fixtures.json"
  let json = A.jsonParser file >>= A.decodeJson :: Either String FixtureFile
  json # either (\err -> log $ "Fixture file parsing failed: " <> err) \errors ->
    launchAff_ $ runSpec [ consoleReporter ] do
      describe "Error parsing" do
        it "should parse arbitrary errors" do
          let r = Utils.parseValidationErrors errors.test1 :: Array ValidationError
          r `shouldEqual` [ { name: "body", errors: [ "empty", "maybe" ] } ]