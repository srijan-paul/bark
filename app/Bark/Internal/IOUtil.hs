module Bark.Internal.IOUtil
  ( tryReadFile,
    tryReadFileT,
    tryReadFileTL,
    tryReadFileBS,
    listFilesRecursive,
    ErrorMessage,
    ResultT,
    copyDirectory,
  )
where

import Control.Monad.Except (ExceptT, filterM, forM_, lift, throwError)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
  )
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

-- | Recursively list all files in a directory
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive = goDirectory []
  where
    go :: [FilePath] -> FilePath -> IO [FilePath]
    go acc fileOrDir = do
      isFile <- doesFileExist fileOrDir
      if isFile
        then return (fileOrDir : acc)
        else goDirectory acc fileOrDir

    goDirectory :: [FilePath] -> FilePath -> IO [FilePath]
    goDirectory acc dir = do
      isDir <- doesDirectoryExist dir
      if isDir
        then do
          files' <- listDirectory dir
          let files = map (dir </>) files'
          innerFiles <- concat <$> mapM (go []) files
          return $ acc ++ innerFiles
        else return acc

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory srcDir dstDir = do
  dirsAndFiles <- getFilesRecursive srcDir
  files <- filterM doesFileExist dirsAndFiles
  forM_ files $ \filePath -> do
    let dstPath = dstDir </> makeRelative srcDir filePath
    createDirectoryIfMissing True (takeDirectory dstPath)
    copyFile filePath dstPath
