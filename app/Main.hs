module Main where
import System.Directory (createDirectoryIfMissing)
import Bark.Core (buildProject)

main :: IO ()
main = do
  createDirectoryIfMissing True "scratch"
  buildProject "scratch"
