module Main where

import Bark.Core (buildProject)
import Data.HashMap.Strict as HashMap
import Data.Text (intercalate, pack)
import System.Directory (createDirectoryIfMissing)
import Text.Mustache (Template (..), ToMustache (toMustache), substitute)
import Text.Mustache.Compile (compileTemplate)
import Bark.FrontMatter (tokenize, parse)

main :: IO ()
main = do
  createDirectoryIfMissing True "scratch"
  buildProject "scratch"
