{-# LANGUAGE OverloadedStrings #-}

-- |The "main" gramophone module. This module essentially exposes all application
-- functionality as a library. The GUI module(s) then call this library.

module Gramophone.Gramophone
    (
     initFiles,
     scanDirectoryForAudioFiles,
     scanTreeForAudioFiles,

     module Gramophone.Database
    ) where

import qualified Gramophone.MediaController as MC
import Gramophone.Database

import qualified Data.Text as Text
import Data.Text (Text)
import System.FilePath((</>))
import qualified System.FilePath as FilePath
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents)
import qualified System.FilePath.Glob as Glob

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict


-- | Given a directory, initializes a database and any other files used
--   by Gramophone.
initFiles :: FilePath -> IO (Either CreateError DatabaseRef)
initFiles location = do
  createDirectoryIfMissing True location
  createDatabase $ location </> "database"

audioFileGlobs :: [Glob.Pattern]
audioFileGlobs = map Glob.compile ["*.flac", "*.mp3", "*.m4a"]

-- | Given a directory, (non-recursively) scans for audio files, based on file extensions.
scanDirectoryForAudioFiles :: FilePath -> IO [FilePath]
scanDirectoryForAudioFiles = (return . concat . fst) <=< (Glob.globDir audioFileGlobs)

data ScanState = ScanState {
              unscannedDirectories :: [FilePath],
              unscannedFiles       :: [FilePath]
            } deriving Show

scanTreeForAudioFiles :: FilePath -> StateT ScanState IO () -> IO ()
scanTreeForAudioFiles dir f = evalStateT f $ ScanState [dir] []

data ScanResult = FoundFile FilePath | ScanningDirectory FilePath | ScanDone
getNextFile :: StateT ScanState IO ScanResult
getNextFile = do
  scanState <- get
  case scanState of
    ScanState [] [] -> return ScanDone
    ScanState dirs (file:files) -> do
      put $ ScanState dirs files
      return $ FoundFile file
    ScanState (dir:dirs) [] -> do
      contents <- liftIO $ (filter (`notElem` [".",".."])) <$> getDirectoryContents dir
      subDirs <- liftIO $ filterM doesDirectoryExist $ map (FilePath.combine dir) contents
      files <- liftIO $ scanDirectoryForAudioFiles dir
      put $ ScanState (subDirs++dirs) files
      return $ScanningDirectory dir

