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

{-# LANGUAGE QuasiQuotes #-}

module Gramophone.GUI.DBTableWidgets
       (
         recordingTableWidget,
         titleColumn,
         trackNumberColumn,
         fileColumn
       ) where

import Gramophone.Core
import Gramophone.GUI.Foundation

import Data.Text (Text)
import Yesod (newIdent, whamlet)
import Text.Blaze (ToMarkup(..))
import Text.Blaze.Html(Html(..))

instance ToMarkup TrackNumber where
  toMarkup (TrackNumber n) = toMarkup n

instance ToMarkup RecordingTitle where
  toMarkup (RecordingTitle t) = toMarkup t

instance ToMarkup AudioFileName where
  toMarkup (AudioFileName s) = toMarkup s
  
maybeToHtml :: ToMarkup a => Maybe a -> Html
maybeToHtml (Just v) = toMarkup v
maybeToHtml Nothing = toMarkup ""


data RecordingColumn = RecordingColumn {
  columnTitle :: Html,
  valueGetter :: Recording -> Html }

makeRecordingColumn :: (ToMarkup a, ToMarkup b) => a -> (Recording -> b) -> RecordingColumn
makeRecordingColumn a f = RecordingColumn (toMarkup a) (toMarkup . f)

titleColumn = makeRecordingColumn "Title" (maybeToHtml . recordingTitle)
trackNumberColumn = makeRecordingColumn (preEscapedToMarkup "Track&nbsp;#") (maybeToHtml . recordingTrackNumber)
fileColumn = makeRecordingColumn "File" recordingFile


recordingTableWidget :: [RecordingColumn] -> [Recording] -> Widget
recordingTableWidget cs rs = do
  recordingListID <- newIdent
  [whamlet|
    <table id="#{recordingListID}">
      <tr>
        $forall c <- cs
           <th>#{columnTitle c}      
        $forall r <- rs
          <tr>
            $forall c <- cs
              <td>^{valueGetter c r}|]
