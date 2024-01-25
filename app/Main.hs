module Main where

import Bark.Core (parseMetaDataFromFile)
import Control.Monad.Except (liftEither, runExceptT)
import qualified Data.ByteString as BS
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  contents <- BS.readFile "todo.md"
  parseResult <- runExceptT (liftEither $ parseMetaDataFromFile "todo.md" contents)
  pPrint parseResult