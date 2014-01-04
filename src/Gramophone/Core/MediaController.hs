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
     --getCurrentState,

     module Gramophone.Core.MediaController.ReadTagsFromFile
    ) where

import Gramophone.Core.MediaController.ReadTagsFromFile

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar(..), newEmptyMVar, putMVar, takeMVar)
--import Control.Concurrent.Chan (Chan(..), newChan, readChan)
import Control.Monad (liftM, liftM2, (>>=), (>=>))
import Data.Time.Clock (DiffTime(..), secondsToDiffTime, UTCTime(..),
                        getCurrentTime)

data Track = Track {
    trackFilename    :: FilePath,
    trackLength      :: DiffTime,
    trackCurrentTime :: DiffTime
  }


data PlayerState = Playing Track | Stopped


data Result = StatusMessage PlayerState UTCTime
                            | PlayMessage PlayResult
    

data Command = Play FilePath | Stop | GetStatus


data MediaController =
  MediaController (MVar Result) (MVar Command)


data MCState = MCState {
    comms :: MediaController,
    state :: PlayerState
  }
               
putCommand :: MediaController -> Command -> IO ()
putCommand (MediaController _ c) = putMVar c

takeCommand :: MCState -> IO Command
takeCommand  (MCState (MediaController _ c) _) = takeMVar c

putResult :: MCState -> Result -> IO ()
putResult  (MCState (MediaController r _) _) = putMVar r

takeResult :: MediaController -> IO Result
takeResult (MediaController r _) = takeMVar r


updatePlayerState :: MCState -> PlayerState -> MCState
updatePlayerState (MCState mc _) s = MCState mc s

initMediaController :: IO MediaController
initMediaController = do
  mc <- liftM2 MediaController newEmptyMVar newEmptyMVar
  forkIO $ mcThread $ MCState mc Stopped
  return mc
  
mcThread :: MCState -> IO ()
mcThread mcs = do
  c <- takeCommand mcs
  (f c >=> mcThread) mcs
  where
    f :: Command -> MCState -> IO MCState
    f (Play file) = playFile' file
    f Stop        = stopPlaying'
    f GetStatus   = getStatus'

data PlayResult = PlaySuccess

playFile :: FilePath -> MediaController -> IO PlayResult
playFile file mc = do
  putCommand mc (Play file)
  PlayMessage r' <- takeResult mc
  return r'

playFile' :: FilePath -> MCState -> IO MCState
playFile' file mcs  = do
  putStrLn ("Playing \"" ++ file ++ "\".")
  putResult mcs (PlayMessage PlaySuccess)
  return $ updatePlayerState mcs (Playing (Track file 0 0))

stopPlaying' :: MCState -> IO MCState
stopPlaying' = (putStrLn "Stopped." >>) . return

getStatus' :: MCState -> IO MCState
getStatus' mcs@(MCState (MediaController r _) s) =
  liftM (StatusMessage s) getCurrentTime >>= putMVar r >> return mcs
