module Main where

import Bark.Core (buildProject)
import Data.HashMap.Strict as HashMap
import Data.Text (intercalate, pack)
import System.Directory (createDirectoryIfMissing)
import Text.Mustache (Template (..), ToMustache (toMustache), substitute)
import Text.Mustache.Compile (compileTemplate)
import Bark.FrontMatter (tokenize, parse')

main :: IO ()
main = do
  let res = compileTemplate "xxx" $ pack "<h1> {{Hi}} {{Bye}} </h1>"
  let hashmap = HashMap.fromList [(pack "Hi", "some stuff"), (pack "Bye", "x")]

  case res of
    Right template -> print $ substitute template (toMustache hashmap)
    _ -> print ":("

  print $ parse' $ tokenize "[\"aa\" \"bb\" [\"11\" \"22\"] \"cc\"]"

  createDirectoryIfMissing True "scratch"
  buildProject "scratch"
