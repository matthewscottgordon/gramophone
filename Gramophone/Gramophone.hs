{-# LANGUAGE OverloadedStrings #-}

-- |The "main" gramophone module. This module essentially exposes all application
-- functionality as a library. The GUI module(s) then call this library.

module Gramophone.Gramophone
    (
     initFiles,

     module Gramophone.Database
    ) where

import qualified Gramophone.MediaController as MC
import Gramophone.Database

import qualified Data.Text as Text
import Data.Text (Text)
import System.FilePath((</>))
import System.Directory (createDirectoryIfMissing)

-- | Given a directory, initializes a database and any other files used
--   by Gramophone.
initFiles :: FilePath -> IO (Either CreateError DatabaseRef)
initFiles location = do
  createDirectoryIfMissing True location
  createDatabase $ location </> "database"