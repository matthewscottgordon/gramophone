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

{-# LANGUAGE  OverloadedStrings, DefaultSignatures, GeneralizedNewtypeDeriving, 
              FlexibleContexts, FlexibleInstances #-}

module Gramophone.Core.Database.Types 
       (
         Id(..),
         Column(..),
         
         AudioFileName(..),
         RecordingTitle(..),
         AlbumTitle(..),
         TrackNumber(..),
         TrackCount(..),
         ArtistName(..),

         Artist(..),
         ArtistID(),
         artistIdColumn,

         Album(..),
         AlbumID(..),
         albumIdColumn,
         albumTitleColumn,

         Recording(..),
         RecordingID(..),
         recordingIdColumn,
         recordingTitleColumn,
         recordingAlbumColumn,
         
         rowIdToText,
         textToRowId,
         
         ConvertSqlValue(..)
       ) where


import Database.HDBC (SqlValue(SqlNull))
import Data.Convertible (Convertible(..), convert, ConvertError(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric (showHex, readHex)


class ConvertSqlValue a where
  toSqlValue :: a -> SqlValue
  fromSqlValue :: SqlValue -> a
  maybeToSqlValue :: Maybe a -> SqlValue
  maybeFromSqlValue :: SqlValue -> Maybe a
  
  default toSqlValue :: Convertible a SqlValue => a -> SqlValue
  toSqlValue = convert
  
  default fromSqlValue :: Convertible SqlValue a => SqlValue -> a
  fromSqlValue = convert
  
  default maybeToSqlValue :: Convertible a SqlValue => Maybe a -> SqlValue
  maybeToSqlValue = convert
  
  default maybeFromSqlValue :: Convertible SqlValue a => SqlValue -> Maybe a
  maybeFromSqlValue = convert

instance ConvertSqlValue Integer
instance ConvertSqlValue Text
instance ConvertSqlValue String

data Id a = Id Integer
    deriving (Read, Show, Eq)
             
unwrapMaybeId :: Maybe (Id a) -> Maybe Integer
unwrapMaybeId (Just (Id i)) = Just i
unwrapMaybeId Nothing       = Nothing

instance ConvertSqlValue (Id a) where
  toSqlValue (Id i) = convert i
  fromSqlValue = Id . convert
  maybeToSqlValue = convert . unwrapMaybeId
  maybeFromSqlValue = f . convert
    where
      f :: Maybe Integer -> Maybe (Id a)
      f (Just i) = Just $ Id i
      f Nothing = Nothing
            
rowIdToText :: Id a -> Text
rowIdToText (Id i) = Text.pack $ showHex i ""

textToRowId :: Text -> Maybe (Id a)
textToRowId t = case readHex $ Text.unpack t of
  (i,""):_  -> Just $ Id i
  otherwise -> Nothing


-- |Opaque type containing a unique identifier for a Recording
type RecordingID = Id Recording

-- |The name of an audio file
newtype AudioFileName = AudioFileName FilePath
    deriving (Show, Eq, ConvertSqlValue)

-- |The title of a recording
newtype RecordingTitle = RecordingTitle Text
    deriving (Show, Eq, ConvertSqlValue)

-- |The title of an album
newtype AlbumTitle = AlbumTitle Text
    deriving (Show, Eq, ConvertSqlValue)

-- |The name of an artist
newtype ArtistName = ArtistName Text
    deriving (Show, Eq, ConvertSqlValue)

-- |The track number of a recording within it's album
newtype TrackNumber = TrackNumber Integer
    deriving (Show, Eq, Ord, ConvertSqlValue)

-- |The number of recordings in a album
newtype TrackCount = TrackCount Integer
    deriving (Show, Eq, Ord, ConvertSqlValue)

-- |Record describing an audio file
data Recording = Recording {
     recordingId          :: Id Recording,
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
     albumId         :: Id Album,
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
     


data Column t v = Column {
    columnSqlName :: String,
    columnGetter :: t -> v,
    columnSqlValue :: v -> SqlValue
    }
                  

recordingTitleColumn :: Column Recording (Maybe RecordingTitle)
recordingTitleColumn = Column "title" recordingTitle maybeToSqlValue

recordingIdColumn :: Column Recording (Id Recording)
recordingIdColumn = Column "id" recordingId toSqlValue

recordingAlbumColumn :: Column Recording (Maybe (Id Album))
recordingAlbumColumn = Column "album" ((fmap albumId) . recordingAlbum) maybeToSqlValue

albumTitleColumn :: Column Album AlbumTitle
albumTitleColumn = Column "title" albumTitle toSqlValue

albumIdColumn :: Column Album (Id Album)
albumIdColumn = Column "id" albumId toSqlValue

artistIdColumn :: Column Artist (Id Artist)
artistIdColumn = Column "id" artistId toSqlValue