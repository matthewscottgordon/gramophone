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
     {-Tags(),
     tagTrackName,
     tagAlbumName,
     tagArtistName,
     tagTrackNumber,
     tagNumTracks,
     tagDiscNumber,
     tagNumDiscs,
     emptyTags,

     MediaController,
     initMediaController,
     readTagsFromFile-}
     module Gramophone.Core.MediaController.ReadTagsFromFile
    ) where

import Gramophone.Core.MediaController.ReadTagsFromFile

