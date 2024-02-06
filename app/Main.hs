module Main where

import Bark.Core (Project (..), watchProjectWith)
import Bark.Processors.SyntaxHighlight (highlightSnippets)
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
  _ <- watchProjectWith [highlightSnippets] project
  return ()
