{-# LANGUAGE OverloadedStrings #-}

-- |The "main" gramophone module. This module essentially exposes all application
-- functionality as a library. The GUI module(s) then call this library.

module Gramophone.Core
    (
     initFiles,
     scanDirectoryForAudioFiles,
     scanTreeForAudioFiles,
     addFileToDatabase,
     addAudioFilesFromTree,

     module Gramophone.Core.Database
    ) where

import qualified Gramophone.Core.MediaController as MC
import Gramophone.Core.MediaController (MediaController)
import Gramophone.Core.Database

import System.FilePath((</>))
import qualified System.FilePath as FilePath
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents)
import qualified System.FilePath.Glob as Glob

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict

import Control.Lens


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


addAudioFilesFromTree :: MonadDB m => MediaController -> FilePath -> m ()
addAudioFilesFromTree mc dir = scanTreeForAudioFiles dir loop
    where loop :: (MonadDB m) => StateT ScanState m ()
          loop = do
            p <- getNextFile
            case p of
              FoundFile filename -> do
                     liftIO $ putStrLn ("File: " ++ filename)
                     addFileToDatabase mc filename
                     loop
              ScanningDirectory dirName -> do
                     liftIO $ putStr ("Scanning: " ++ dirName ++ "\r")
                     loop
              ScanDone -> return ()


addFileToDatabase :: MonadDB m => MediaController -> FilePath -> m ()
addFileToDatabase mc filename = do
    maybeTags <- liftIO $ MC.readTagsFromFile mc filename
    case maybeTags of
      Nothing -> return ()
      Just tags -> do
        liftIO $ putStrLn ("File: " ++ filename)
        recordingRelation <- addRecording (NewRecording (AudioFileName filename)
                                                        (getRecordingTitleTag tags)
                                                        Nothing
                                                        Nothing
                                                        (getTrackNumberTag tags))
        return ()
  where
    getRecordingTitleTag tags =  RecordingTitle <$> (view MC.tagTrackName tags)
    getTrackNumberTag tags = TrackNumber <$> (view MC.tagTrackNumber tags)

data ScanState = ScanState {
              unscannedDirectories :: [FilePath],
              unscannedFiles       :: [FilePath]
            } deriving Show

scanTreeForAudioFiles :: MonadIO m => FilePath -> StateT ScanState m () -> m ()
scanTreeForAudioFiles dir f = evalStateT f $ ScanState [dir] []

data ScanResult = FoundFile FilePath | ScanningDirectory FilePath | ScanDone
getNextFile :: MonadIO m => StateT ScanState m ScanResult
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

