{-# LANGUAGE OverloadedStrings #-}

import Bark.FrontMatter (toMustacheValue)
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Yaml as Yml
import Test.HUnit (Test (..), assertFailure, runTestTTAndExit, (~:), (~=?))
import qualified Text.Mustache.Types as M

testToMustacheValue :: Test
testToMustacheValue = do
  "String" ~: do
    let (M.String s) = toMustacheValue (Aeson.String "Wario's woods & more")
    s ~=? "Wario's woods & more"

main :: IO ()
main =
  runTestTTAndExit $ TestList [testToMustacheValue]
