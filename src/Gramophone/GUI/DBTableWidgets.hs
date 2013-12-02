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

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Gramophone.GUI.DBTableWidgets
       (
         recordingTableWidget
       ) where

import Gramophone.Core
import Gramophone.GUI.Foundation

import Data.Text (Text)
import Yesod (newIdent, whamlet)
import Text.Blaze (ToMarkup(..))

instance ToMarkup TrackNumber where
  toMarkup (TrackNumber n) = toMarkup n

instance ToMarkup RecordingTitle where
  toMarkup (RecordingTitle t) = toMarkup t

instance ToMarkup AudioFileName where
  toMarkup (AudioFileName s) = toMarkup s
  
maybeField :: ToMarkup a => Maybe a -> Widget
maybeField v = [whamlet|
                  $maybe n <- v
                    <td>#{n}
                  $nothing
                    <td>|]


recordingTableWidget :: [Recording] -> Widget
recordingTableWidget recordings = do
  recordingListID <- newIdent
  [whamlet|
    <table id="#{recordingListID}">
      <tr>
        <th>Title
        <th>Track&nbsp;# 
        <th>File
      $forall recording <- recordings
        <tr>
          ^{maybeField (recordingTitle recording)}
          ^{maybeField (recordingTrackNumber recording)}
          <td>#{recordingFile recording}|]