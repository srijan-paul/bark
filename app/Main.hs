module Main where

import Bark.Core (buildProject)
import Data.HashMap.Strict as HashMap
import Data.Text (pack)
import System.Directory (createDirectoryIfMissing)
import Text.Mustache (Template (..), ToMustache (toMustache), substitute)
import Text.Mustache.Compile (compileTemplate)

main :: IO ()
main = do
  let res = compileTemplate "xxx" $ pack "<h1> {{Hi}} {{Bye}} </h1>"
  let hashmap = HashMap.fromList [(pack "Hi", 1 :: Int), (pack "Bye", 2)]

  case res of
    Right template -> print $ substitute template (toMustache hashmap)
    _ -> print ":("

  createDirectoryIfMissing True "scratch"
  buildProject "scratch"
