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
     readTagsFromFile,
     shutDown,
     
     Tags(Tags),
     tagTrackName,
     tagAlbumName,
     tagArtistName,
     tagTrackNumber,
     tagNumTracks,
     tagDiscNumber,
     tagNumDiscs,
     emptyTags,
     ReadTagsResult(..)
    ) where

import Gramophone.Core.MediaController.Types
import Gramophone.Core.MediaController.Tags
import Gramophone.Core.MediaController.ReadTags
import Gramophone.Core.MediaController.CommandMVar

import Control.Concurrent (forkIO, yield)
import Control.Monad (liftM5)
import Control.Applicative ((<$>))
import Data.Time.Clock (getCurrentTime)

import qualified Media.Streaming.GStreamer as GS
import qualified System.Glib as G
import qualified System.Glib.MainLoop as G


initMediaController :: IO MediaController
initMediaController = do
  mc <- liftM5 MediaController
                 initCommand initCommand initCommand
                 initCommand initCommand
  forkIO $ mcThread mc 
  return mc
  
mcThread :: MediaController -> IO ()
mcThread mcs = do
  putStrLn "mcThread"
  GS.init
  loop <- G.mainLoopNew Nothing False
  G.idleAdd (yield >> return True) G.priorityDefaultIdle
  G.timeoutAdd (mcDispatch loop mcs >> return True ) 100
  putStrLn "Entering main loop."
  G.mainLoopRun loop
  putStrLn "Exited main loop."
  
mcDispatch :: G.MainLoop -> MediaController -> IO ()
mcDispatch mainLoop mc = do
  handleCommand (shutDownCommand mc) (shutDown' mainLoop mc)
  handleCommand (playCommand mc) (playFile' mc)
  handleCommand (stopCommand mc) (stopPlaying' mc)
  handleCommand (statusCommand mc) (getStatus' mc)
  handleCommand (readTagsCommand mc) (readTagsFromFile' mc)


shutDown :: MediaController -> IO ()
shutDown mc = (invokeCommand (shutDownCommand mc)) ShutDown

shutDown' :: G.MainLoop -> MediaController -> ShutDownCommand -> IO ()
shutDown' loop _ _  = putStrLn "shutDown'" >> G.mainLoopQuit loop

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

readTagsFromFile :: MediaController ->
                    FilePath -> IO ReadTagsResult
readTagsFromFile mc = (invokeCommand (readTagsCommand mc)) . ReadTags

readTagsFromFile' :: MediaController -> 
                     ReadTagsCommand -> IO ReadTagsResult
readTagsFromFile' = readTags