module Main where

import Bark.Core (Project (..), getPostFromMdfile, postToHtml)
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
  post <- runExceptT $ getPostFromMdfile project (projectPath </> "src" </> "index.md")
  pPrint (post >>= postToHtml)
