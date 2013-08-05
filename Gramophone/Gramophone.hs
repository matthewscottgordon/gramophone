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

scanTreeForAudioFiles :: FilePath -> IO [FilePath]
scanTreeForAudioFiles dir = do tree <- getTree dir
                               concat <$> mapM scanDirectoryForAudioFiles tree
                            where
                              getTree d = do
                                contents <- (filter (`notElem` [".", ".."])) <$> getDirectoryContents d
                                subDirs <- filterM doesDirectoryExist $ map (FilePath.combine d) contents
                                subSubDirs <- concat <$> mapM getTree subDirs
                                return $ d : subSubDirs
