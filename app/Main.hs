module Main where

import Bark.Core (Project (..), buildProjectWith)
import Bark.Preprocessors.SyntaxHighlight (highlightSnippets)
import Control.Monad.Except (runExceptT)
import System.Directory (makeAbsolute)
import System.FilePath ((</>))

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
  result <- runExceptT $ buildProjectWith [highlightSnippets] project
  return ()
