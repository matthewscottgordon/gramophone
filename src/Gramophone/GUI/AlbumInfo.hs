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

module Gramophone.GUI.AlbumInfo
       (
         getAlbumInfoR
       ) where

import Gramophone.GUI.Foundation

import Yesod (Html, defaultLayout, whamlet, notFound)

import Gramophone.GUI.DBTableWidgets

albumInfoWidget :: Album -> Widget
albumInfoWidget album = 
  [whamlet|
   <table>
     <tr>
       <th>Title:
       <td>#{albumTitle album}
     $maybe artist <- albumArtist album
       <tr>
         <th>Artist:
         <td>#{artistName artist}
     $if (albumTrackCount album) > (TrackCount 0)
       <tr>
         <th>Number of Tracks:
         <td>#{albumTrackCount album}|]
     
getAlbumInfoR :: AlbumID -> Handler Html
getAlbumInfoR albumID= defaultLayout $ do
  maybeAlbum <- withDatabase $ getAlbum albumID
  case maybeAlbum of
    Just album -> albumInfoWidget album
    Nothing    -> notFound
  [whamlet|<div><a href=@{TestR}>Back to testing functions</a>|]