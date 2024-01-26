module Main where

import Bark.Core (Project (..), buildPost, getPostFromMdfile, postToHtml)
import Control.Monad.Except (runExceptT)
import System.Directory (makeAbsolute)
import System.FilePath ((</>))
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  projectPath <- makeAbsolute "./site"
  let project =
        Project
          { projectRoot = projectPath,
            projectSourceDir = projectPath </> "src",
            projectOutDir = projectPath </> "build",
            projectAssetsDir = projectPath </> "assets",
            projectTemplateDir = projectPath </> "template"
          }
  runExceptT $ do
    post <- getPostFromMdfile project (projectSourceDir project </> "index.md")
    buildPost project post

  return ()