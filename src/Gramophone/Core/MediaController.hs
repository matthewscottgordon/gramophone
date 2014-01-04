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

{-# LANGUAGE  OverloadedStrings, TemplateHaskell #-}

module Gramophone.Core.MediaController
    (
     MediaController,
     initMediaController,
     playFile,
     stopPlaying,
     getStatus,

     module Gramophone.Core.MediaController.ReadTagsFromFile
    ) where

import Gramophone.Core.MediaController.ReadTagsFromFile
import Gramophone.Core.MediaController.CommandMVar

import Control.Concurrent (forkIO)
import Control.Monad (liftM3)
import Control.Applicative ((<$>))
import Data.Time.Clock (DiffTime, UTCTime(..), getCurrentTime)

data Track = Track {
    trackFilename    :: FilePath,
    trackLength      :: DiffTime,
    trackCurrentTime :: DiffTime
  }


data PlayerState = Playing Track | Stopped


--data Result = StatusMessage PlayerState UTCTime
--                            | PlayMessage PlayResult


--data Command = Play FilePath | Stop | GetStatus

data GenericResult = Success | ErrorMessage String

data PlayCommand = Play FilePath

data StopCommand = Stop

data StatusCommand = GetStatus
data StatusResult = Status PlayerState UTCTime


data MediaController = MediaController {
  playCommand   :: CommandMVar PlayCommand   GenericResult,
  stopCommand   :: CommandMVar StopCommand   GenericResult,
  statusCommand :: CommandMVar StatusCommand StatusResult }

initMediaController :: IO MediaController
initMediaController = do
  mc <- liftM3 MediaController initCommand initCommand initCommand
  forkIO $ mcThread mc 
  return mc
  
mcThread = mcDispatch >> mcThread
  
mcDispatch :: MediaController -> IO ()
mcDispatch mc = do
  handleCommand' (playCommand mc) (playFile' mc)
  handleCommand' (stopCommand mc) (stopPlaying' mc)
  handleCommand' (statusCommand mc) (getStatus' mc)
    

playFile :: MediaController -> FilePath -> IO GenericResult
playFile mc = (invokeCommand (playCommand mc)) . Play 

playFile' :: MediaController -> PlayCommand -> IO GenericResult
playFile' _ (Play file) = do
  putStrLn ("Playing \"" ++ file ++ "\".")
  return Success


stopPlaying :: MediaController -> IO GenericResult
stopPlaying mc = (invokeCommand (stopCommand mc)) Stop

stopPlaying' :: MediaController -> StopCommand -> IO GenericResult
stopPlaying' _ _ = putStrLn "Stopped." >> return Success


getStatus :: MediaController -> IO StatusResult
getStatus mc = (invokeCommand (statusCommand mc)) GetStatus

getStatus' :: MediaController -> StatusCommand -> IO StatusResult
getStatus' _ _ = (Status Stopped) <$> getCurrentTime

