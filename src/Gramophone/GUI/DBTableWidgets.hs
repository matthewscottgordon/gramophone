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
         dbTableWidget,
         
         recordingTitleColumn,
         recordingTrackNumberColumn,
         recordingFileColumn,
         recordingArtistNameColumn,
         recordingAlbumTitleColumn,
         recordingAlbumArtistNameColumn,
         recordingAlbumTrackCountColumn,
         
         albumTitleColumn,
         albumArtistNameColumn,
         albumTrackCountColumn,
         
         artistNameColumn
       ) where

import Gramophone.Core
import Gramophone.GUI.Foundation

import Data.Text (Text)
import Yesod (newIdent, whamlet)
import Text.Blaze (ToMarkup(..))
import Text.Blaze.Html(Html(..))

import Control.Monad ((<=<))

  
maybeToHtml :: ToMarkup a => Maybe a -> Html
maybeToHtml (Just v) = toMarkup v
maybeToHtml Nothing = toMarkup ""

data Column a = Column {
  columnTitle :: Html,
  valueGetter :: a -> Html }

makeColumn :: (ToMarkup a, ToMarkup b) => a -> (c -> b) -> Column c
makeColumn a f = Column (toMarkup a) (toMarkup .f)

dbTableWidget :: [Column a] -> [a] -> Widget
dbTableWidget cs rs = do
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



instance ToMarkup TrackNumber where
  toMarkup (TrackNumber n) = toMarkup n

instance ToMarkup RecordingTitle where
  toMarkup (RecordingTitle t) = toMarkup t

instance ToMarkup AudioFileName where
  toMarkup (AudioFileName s) = toMarkup s
                
recordingTitleColumn = makeColumn "Title" (maybeToHtml . recordingTitle)
recordingTrackNumberColumn = makeColumn (preEscapedToMarkup "Track&nbsp;#") (maybeToHtml . recordingTrackNumber)
recordingFileColumn = makeColumn "File" recordingFile
recordingArtistNameColumn = makeColumn "Artist" (maybeToHtml . (fmap artistName) . recordingArtist)
recordingAlbumTitleColumn = makeColumn "Album" (maybeToHtml . (fmap albumTitle) . recordingAlbum)
recordingAlbumArtistNameColumn = makeColumn "Album Artist" (maybeToHtml . (fmap artistName) . (albumArtist <=< recordingAlbum))
recordingAlbumTrackCountColumn = makeColumn "Album # Tracks" (maybeToHtml . (fmap albumTrackCount) . recordingAlbum)


instance ToMarkup AlbumTitle where
  toMarkup (AlbumTitle t) = toMarkup t
  
instance ToMarkup TrackCount where
  toMarkup (TrackCount c) = toMarkup c
  
albumTitleColumn = makeColumn "Title" albumTitle
albumArtistNameColumn = makeColumn "Artist" (maybeToHtml . (fmap artistName) . albumArtist)
albumTrackCountColumn = makeColumn "Track Count" albumTrackCount

    
instance ToMarkup ArtistName where
  toMarkup (ArtistName n) = toMarkup n

artistNameColumn = makeColumn "Name" artistName