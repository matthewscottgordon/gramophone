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

{-# LANGUAGE QuasiQuotes, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Gramophone.GUI.DBTableWidgets
       (
         dbTableWidget,
         
         recordingTitleColumn,
         recordingTrackNumberColumn,
         recordingFileColumn,
         recordingArtistNameColumn,
         recordingAlbumTitleColumn,
         recordingAlbumTitleColumnWithLink,
         recordingAlbumArtistNameColumn,
         recordingAlbumTrackCountColumn,
         recordingTrackNumberWithCountColumn,
         
         albumTitleColumn,
         albumArtistNameColumn,
         albumTrackCountColumn,
         
         artistNameColumn
       ) where

import Gramophone.Core
import Gramophone.GUI.Foundation

import Data.Text (Text)
import Yesod (newIdent, whamlet, Route())
import Text.Blaze (ToMarkup(..))
import Text.Blaze.Html(Html(..))

import Control.Monad ((<=<))


maybeAp :: Maybe (a -> Maybe b) -> a -> Maybe b
maybeAp f' v = case f' of
  Just f -> f v
  Nothing -> Nothing
  
maybeToHtml :: ToMarkup a => Maybe a -> Html
maybeToHtml (Just v) = toMarkup v
maybeToHtml Nothing = toMarkup ""

data Column a = Column {
  columnTitle :: Html,
  valueGetter :: a -> Html, 
  linkGetter  :: Maybe (a -> Maybe (Route Website)) }

makeColumn :: (ToMarkup a, ToMarkup b) => a -> (c -> b) -> Column c
makeColumn a f = Column (toMarkup a) (toMarkup .f) Nothing

addLink :: Column a -> Maybe (a -> Maybe (Route Website)) -> Column a
addLink (Column t v _) l = Column t v l

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
              $with text <-  valueGetter c r
                $maybe link <- maybeAp (linkGetter c) r
                  <td>
                    <a href=@{link}>^{text}
                $nothing
                  <td>^{text}|]



deriving instance ToMarkup TrackNumber
deriving instance ToMarkup RecordingTitle
deriving instance ToMarkup AudioFileName
deriving instance ToMarkup AlbumTitle
deriving instance ToMarkup TrackCount
deriving instance ToMarkup ArtistName
                
  
recordingTitleColumn = makeColumn "Title" (maybeToHtml . recordingTitle)
recordingTrackNumberColumn = makeColumn (preEscapedToMarkup "Track&nbsp;#") (maybeToHtml . recordingTrackNumber)
recordingFileColumn = makeColumn "File" recordingFile
recordingArtistNameColumn = makeColumn "Artist" (maybeToHtml . (fmap artistName) . recordingArtist)
recordingAlbumTitleColumn = makeColumn "Album" (maybeToHtml . (fmap albumTitle) . recordingAlbum)
recordingAlbumTitleColumnWithLink =
  recordingAlbumTitleColumn `addLink` (Just $ (fmap (AlbumInfoR . albumId)) . recordingAlbum)
recordingAlbumArtistNameColumn = makeColumn "Album Artist"
                                            (maybeToHtml . (fmap artistName) . (albumArtist <=< recordingAlbum))
recordingAlbumTrackCountColumn = makeColumn "Album # Tracks" (maybeToHtml . (fmap albumTrackCount) . recordingAlbum)

recordingTrackNumberWithCountColumn = makeColumn (preEscapedToMarkup "Track&nbsp;#") format
  where
    format r = format' (recordingTrackNumber r) (((fmap albumTrackCount) .recordingAlbum) r)
    format' (Just t) (Just c) | c > (TrackCount 0) = (toMarkup t) >> (toMarkup " of ") >> (toMarkup c)
                              | otherwise          = toMarkup t
    format' (Just t) Nothing                       = toMarkup t
    format' Nothing Nothing                        = toMarkup ""


  
albumTitleColumn = makeColumn "Title" albumTitle
albumArtistNameColumn = makeColumn "Artist" (maybeToHtml . (fmap artistName) . albumArtist)
albumTrackCountColumn = makeColumn "Track Count" albumTrackCount


artistNameColumn = makeColumn "Name" artistName
