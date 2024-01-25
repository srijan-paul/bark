module Bark.Internal.IOUtil
  ( tryReadFile,
    tryReadFileT,
    tryReadFileTL,
    tryReadFileBS,
    withFilesInDir,
    ErrorMessage,
    ResultT,
  )
where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, lift, throwError)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    listDirectory,
  )
import System.FilePath (combine)

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

-- | Recursively iterate over all files in a directory and
-- | call the provided function on every file path
withFilesInDir :: (FilePath -> IO ()) -> FilePath -> IO ()
withFilesInDir _ "" = return ()
withFilesInDir callback path = do
  isFile <- doesFileExist path
  isDirectory <- doesDirectoryExist path

  when isFile (callback path) -- if a file, then call callback
  when isDirectory recurse -- if a directory, then search inside it
  where
    recurse = do
      -- `path` is itself a directory, and needs to be recursed over
      contents <- listDirectory path
      mapM_
        (withFilesInDir callback . combine path)
        contents
