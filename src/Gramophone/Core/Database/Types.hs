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

{-# LANGUAGE  OverloadedStrings, MultiParamTypeClasses #-}

module Gramophone.Core.Database.Types 
       (
         Id(..),
         
         AudioFileName(..),
         RecordingTitle(..),
         AlbumTitle(..),
         TrackNumber(..),
         TrackCount(..),
         ArtistName(..),

         Artist(..),
         ArtistID(),

         Album(..),
         AlbumID(),

         Recording(..),
         RecordingID()
       ) where


import Database.HDBC (SqlValue)
import Data.Convertible (Convertible(..))
import Data.Text (Text)


data Id a = Id Integer
    deriving Show

-- |Opaque type containing a unique identifier for a Recording
type RecordingID = Id Recording

-- |The name of an audio file
newtype AudioFileName = AudioFileName FilePath
    deriving (Show, Eq)
instance Convertible SqlValue AudioFileName where
    safeConvert = (fmap AudioFileName) . safeConvert
instance Convertible AudioFileName SqlValue where
     safeConvert (AudioFileName a) = safeConvert a

-- |The title of a recording
newtype RecordingTitle = RecordingTitle Text
    deriving (Show, Eq)
instance Convertible SqlValue RecordingTitle where
    safeConvert = (fmap RecordingTitle) . safeConvert
instance Convertible RecordingTitle SqlValue where
     safeConvert (RecordingTitle a) = safeConvert a

-- |The title of an album
newtype AlbumTitle = AlbumTitle Text
    deriving (Show, Eq)
instance Convertible SqlValue AlbumTitle where
    safeConvert = (fmap AlbumTitle) . safeConvert
instance Convertible AlbumTitle SqlValue where
     safeConvert (AlbumTitle a) = safeConvert a

-- |The name of an artist
newtype ArtistName = ArtistName Text
    deriving (Show, Eq)
instance Convertible SqlValue ArtistName where
    safeConvert = (fmap ArtistName) . safeConvert
instance Convertible ArtistName SqlValue where
     safeConvert (ArtistName a) = safeConvert a

-- |The track number of a recording within it's album
newtype TrackNumber = TrackNumber Integer
    deriving (Show, Eq)
instance Convertible SqlValue TrackNumber where
    safeConvert = (fmap TrackNumber) . safeConvert
instance Convertible TrackNumber SqlValue where
     safeConvert (TrackNumber a) = safeConvert a

-- |The number of recordings in a album
newtype TrackCount = TrackCount Integer
    deriving (Show, Eq)
instance Convertible SqlValue TrackCount where
    safeConvert = (fmap TrackCount) . safeConvert
instance Convertible TrackCount SqlValue where
     safeConvert (TrackCount a) = safeConvert a

-- |Record describing an audio file
data Recording = Recording {
     recordingId          :: RecordingID,
     recordingFile        :: AudioFileName,
     recordingTitle       :: Maybe RecordingTitle,
     recordingArtist      :: Maybe Artist,
     recordingAlbum       :: Maybe Album,
     recordingTrackNumber :: Maybe TrackNumber
} deriving Show

-- |Opaque type containing a unique identifier for an Album
type AlbumID = Id Album

-- |Record describing an album
data Album = Album {
     albumId         :: AlbumID,
     albumTitle      :: AlbumTitle,
     albumArtist     :: Maybe Artist,
     albumTrackCount :: TrackCount
} deriving Show

-- |Opaque type containing a unique identifier for an Artist
type ArtistID = Id Artist

-- |Record describing a recording artist
data Artist = Artist {
     artistId   :: ArtistID,
     artistName :: ArtistName
} deriving Show

instance Convertible SqlValue (Id a) where
     safeConvert = (fmap Id) . safeConvert

instance Convertible (Id a) SqlValue where
     safeConvert (Id val) = safeConvert val