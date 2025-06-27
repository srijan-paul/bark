module Bark.Internal.IOUtil
  ( tryReadFile,
    tryReadFileT,
    tryReadFileTL,
    tryReadFileBS,
    ErrorMessage,
    copyDirectory,
  )
where

import Control.Monad (filterM, forM_)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Trans (lift)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.Directory.Recursive (getFilesRecursive)
import System.FilePath (makeRelative, takeDirectory, (</>))

type ErrorMessage = String

type ResultT m a = ExceptT ErrorMessage m a

tryReadFileWith :: (FilePath -> IO a) -> FilePath -> ExceptT ErrorMessage IO a
tryReadFileWith reader path = do
  exist <- (lift . doesFileExist) path
  if exist
    then lift $ reader path
    else throwError $ "File does not exist: '" ++ path ++ "'"

-- | Read a file from the filesystem and return it's contents
-- | as a lazy text object. If the file does not exist, return
-- | an error message instead.
tryReadFileTL :: FilePath -> ResultT IO TL.Text
tryReadFileTL = tryReadFileWith TLIO.readFile

-- | Read a file from the filesystem and return it's contents as a String.
-- | If the file does not exist, return an error message instead.
tryReadFile :: FilePath -> ResultT IO String
tryReadFile = tryReadFileWith readFile

-- | Read a file from the filesystem and return it's contents as a String.
-- | If the file does not exist, return an error message instead.
tryReadFileT :: FilePath -> ResultT IO T.Text
tryReadFileT = tryReadFileWith TIO.readFile

tryReadFileBS :: FilePath -> ResultT IO BS.ByteString
tryReadFileBS = tryReadFileWith BS.readFile

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory srcDir dstDir = do
  dirsAndFiles <- getFilesRecursive srcDir
  files <- filterM doesFileExist dirsAndFiles
  forM_ files $ \filePath -> do
    let dstPath = dstDir </> makeRelative srcDir filePath
    createDirectoryIfMissing True (takeDirectory dstPath)
    copyFile filePath dstPath
