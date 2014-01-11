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

{-# LANGUAGE TemplateHaskell #-}

module Gramophone.Core.MediaController.Types
       (
         Track(..),
         PlayerState(..),
         GenericResult(..),
         PlayCommand(..),
         StopCommand(..),
         ShutDownCommand(..),
         StatusCommand(..),
         StatusResult(..),
         ReadTagsCommand(..),
         ReadTagsResult(..),
         MediaController(..)
       ) where
         
import Gramophone.Core.MediaController.CommandMVar
import Gramophone.Core.MediaController.Tags

import Data.Time.Clock (DiffTime, UTCTime(..))


data ReadTagsCommand = ReadTags FilePath
data ReadTagsResult = TagsSuccess Tags | TagsFail String

data Track = Track {
    trackFilename    :: FilePath,
    trackLength      :: DiffTime,
    trackCurrentTime :: DiffTime
  }

data PlayerState = Playing Track | Stopped

data GenericResult = Success | ErrorMessage String

data PlayCommand = Play FilePath

data StopCommand = Stop

data ShutDownCommand = ShutDown

data StatusCommand = GetStatus
data StatusResult = Status PlayerState UTCTime


data MediaController = MediaController {
  playCommand     :: CommandMVar PlayCommand      GenericResult,
  stopCommand     :: CommandMVar StopCommand      GenericResult,
  shutDownCommand :: CommandMVar ShutDownCommand  (),
  statusCommand   :: CommandMVar StatusCommand    StatusResult ,
  readTagsCommand :: CommandMVar ReadTagsCommand  ReadTagsResult }