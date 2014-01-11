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

module Gramophone.Core.MediaController.Tags
       (
         Tags(Tags),
         tagTrackName,
         tagAlbumName,
         tagArtistName,
         tagTrackNumber,
         tagNumTracks,
         tagDiscNumber,
         tagNumDiscs,
         emptyTags,
         modifyTag
       ) where

import Data.Text
import Control.Lens
import Control.Applicative ((<|>))

data Tags = Tags {
      _tagTrackName :: Maybe Text,
      _tagAlbumName :: Maybe Text,
      _tagArtistName :: Maybe Text,
      _tagTrackNumber :: Maybe Integer,
      _tagNumTracks :: Maybe Integer,
      _tagDiscNumber :: Maybe Integer,
      _tagNumDiscs :: Maybe Integer
}


$(makeLenses ''Tags)

emptyTags :: Tags
emptyTags = 
  Tags Nothing Nothing Nothing Nothing Nothing Nothing Nothing

modifyTag l v = l `over` (<|> v)

instance Show Tags where
    show (Tags maybeTrackName maybeAlbumName maybeArtistName
          maybeTrackNumber maybeNumTracks maybeDiscNumber
          maybeNumDiscs) =
                     (maybeLine "Title:       " maybeTrackName) ++ 
                     (maybeLine "Artist:      " maybeArtistName) ++
                     (maybeLine "Album:       " maybeAlbumName) ++
                     (maybeLine "Track #:     " maybeTrackNumber) ++
                     (maybeLine "Track Count: " maybeNumTracks) ++
                     (maybeLine "Disc #:      " maybeDiscNumber) ++
                     (maybeLine "Disc Count:  " maybeNumDiscs)
                   where
                     maybeLine label (Just value) =
                       label ++ (show value) ++ "\n"
                     maybeLine _     Nothing      = ""