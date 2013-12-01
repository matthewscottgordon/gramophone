{-  Copyright 2013 Matthew Gordon.

    This file is part of Gramophone.

    Gramophone is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Gramophone is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Gramophone.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE OverloadedStrings #-}

-- |The "main" gramophone module. This module essentially exposes all application
-- functionality as a library. The GUI module(s) then call this library.

module Gramophone.Core
    (
     initFiles,
     openFiles,
     scanDirectoryForAudioFiles,
     scanTreeForAudioFiles,
     addFileToDatabase,
     addAudioFilesFromTree,

     module Gramophone.Core.Database,
     module Gramophone.Core.MediaController
    ) where

import qualified Gramophone.Core.MediaController as MC
import Gramophone.Core.MediaController (MediaController, initMediaController)
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

import Data.Maybe (fromMaybe)


-- | Given a directory, initializes a database and any other files used
--   by Gramophone.
initFiles :: FilePath -> IO (Either CreateError DatabaseRef)
initFiles location = do
  createDirectoryIfMissing True location
  createDatabase $ location </> "database"
    

-- | Given the gramophone directory, returns a DatabaseRef pointing
--   to it.
openFiles :: FilePath -> IO (Either OpenError DatabaseRef)
openFiles location = openDatabase $ location </> "database"
  

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


addFileToDatabase :: MonadDB m => MediaController -> FilePath -> m (Maybe Recording)
addFileToDatabase mc filename = do
    maybeTags <- liftIO $ MC.readTagsFromFile mc filename
    newRecording <- case maybeTags of
      Nothing -> return $ NewRecording (AudioFileName filename) Nothing Nothing Nothing Nothing
      Just tags -> do
        liftIO $ putStrLn ("File: " ++ filename)
        artistRelation <- case (getTag ArtistName MC.tagArtistName tags) of
                            Nothing         -> return Nothing
                            Just name -> do
                                  artistRelations <- findArtists name
                                  case artistRelations of
                                    (a:_) -> return (Just a)
                                    []    -> addArtist (NewArtist name)
        albumRelation <- case (getTag AlbumTitle MC.tagAlbumName tags) of
                           Nothing        -> return Nothing
                           Just albumName -> do
                                  albumRelations <- findAlbums albumName
                                  case albumRelations of
                                    (a:_) -> return (Just a)
                                    []    -> addAlbum (NewAlbum albumName
                                                                    (artistId <$> artistRelation)
                                                                    (TrackCount $ fromMaybe 0 (view MC.tagNumTracks tags)))
        return $ NewRecording (AudioFileName filename)
                              (getTag RecordingTitle MC.tagTrackName tags)
                              (artistId <$> artistRelation)
                              (albumId <$> albumRelation)
                              (getTag TrackNumber MC.tagTrackNumber tags)
    addRecording newRecording
  where
    getTag c t ts = c <$> (view t ts)


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

