{-# LANGUAGE  OverloadedStrings, MultiParamTypeClasses #-}

-- |Create and manage the main Gramophone database.
module Gramophone.Database 
    (
     DatabaseRef(),
     getDatabaseRef,

     FileName,
     Title,
     TrackNumber,
     TrackCount,
     Name,

     Artist(..),
     ArtistID(),
     findArtists,
     getArtist,
     NewArtist(..),
     addArtist,

     Album(..),
     AlbumID(),
     findAlbums,
     getAlbum,
     NewAlbum(..),
     addAlbum,

     Recording(..),
     RecordingID(),
     getRecording,
     NewRecording(..),
     addRecording
    ) where

import qualified Data.Text as T
import Data.Text (Text)

import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC.Sqlite3 (Connection)
import Database.HDBC

import Control.Monad
import Data.Functor

import Data.Convertible

import System.Directory (doesFileExist)


-- |Opaque type containing a unique identifier for a Recording
data RecordingID = RecordingID Integer deriving Show

-- |The name of an audio file
newtype FileName = FileName Text
    deriving Show
instance Convertible SqlValue FileName where
    safeConvert = (fmap FileName) . safeConvert
instance Convertible FileName SqlValue where
     safeConvert (FileName a) = safeConvert a

-- |The title of a recording or album
newtype Title = Title Text
    deriving Show
instance Convertible SqlValue Title where
    safeConvert = (fmap Title) . safeConvert
instance Convertible Title SqlValue where
     safeConvert (Title a) = safeConvert a

-- |The name of an artist
newtype Name = Name Text
    deriving Show
instance Convertible SqlValue Name where
    safeConvert = (fmap Name) . safeConvert
instance Convertible Name SqlValue where
     safeConvert (Name a) = safeConvert a

-- |The track number of a recording within it's album
newtype TrackNumber = TrackNumber Integer
    deriving Show
instance Convertible SqlValue TrackNumber where
    safeConvert = (fmap TrackNumber) . safeConvert
instance Convertible TrackNumber SqlValue where
     safeConvert (TrackNumber a) = safeConvert a

-- |The number of recordings in a album
newtype TrackCount = TrackCount Integer
    deriving Show
instance Convertible SqlValue TrackCount where
    safeConvert = (fmap TrackCount) . safeConvert
instance Convertible TrackCount SqlValue where
     safeConvert (TrackCount a) = safeConvert a

-- |Record describing an audio file
data Recording = Recording {
     recordingId          :: RecordingID,
     recordingFile        :: FileName,
     recordingTitle       :: Title,
     recordingArtist      :: Artist,
     recordingAlbum       :: Album,
     recordingTrackNumber :: TrackNumber
} deriving Show

-- |Opaque type containing a unique identifier for an Album
data AlbumID = AlbumID Integer deriving Show

-- |Record describing an album
data Album = Album {
     albumId        :: AlbumID,
     albumTitle     :: Title,
     albumArtist    :: Artist,
     albumNumTracks :: TrackCount
} deriving Show

-- |Opaque type containing a unique identifier for an Artist
data ArtistID = ArtistID Integer deriving Show

-- |Record describing a recording artist
data Artist = Artist {
     artistId   :: ArtistID,
     artistName :: Name
} deriving Show

instance Convertible SqlValue ArtistID where
     safeConvert = (fmap ArtistID) . safeConvert

instance Convertible SqlValue AlbumID where
     safeConvert = (fmap AlbumID) . safeConvert

instance Convertible SqlValue RecordingID where
     safeConvert = (fmap RecordingID) . safeConvert

instance Convertible ArtistID SqlValue where
     safeConvert (ArtistID a) = safeConvert a

instance Convertible AlbumID SqlValue where
     safeConvert (AlbumID a) = safeConvert a

instance Convertible RecordingID SqlValue where
     safeConvert (RecordingID a) = safeConvert a

-- Convenience functions for unpacking a [SqlValue]
convert1 :: Convertible a b => [a] -> b
convert1 (a:_) = convert a

convert2 :: Convertible a b => Convertible a c => [a] -> (b, c)
convert2 (a:b:_) = (convert a, convert b)

convert3 :: Convertible a b => Convertible a c => Convertible a d => [a] -> (b, c, d)
convert3 (a:b:c:_) = (convert a, convert b, convert c)

convert4 :: Convertible a b => Convertible a c => Convertible a d => Convertible a e => [a] -> (b, c, d, e)
convert4 (a:b:c:d:_) = (convert a, convert b, convert c, convert d)

convert5 :: Convertible a b => Convertible a c => Convertible a d => Convertible a e => Convertible a f => [a] -> (b, c, d, e, f)
convert5 (a:b:c:d:e:_) = (convert a, convert b, convert c, convert d, convert e)

convert6 :: Convertible a b => Convertible a c => Convertible a d => Convertible a e => Convertible a f => Convertible a g =>
            [a] -> (b, c, d, e, f, g)
convert6 (a:b:c:d:e:f:_) = (convert a, convert b, convert c, convert d, convert e, convert f)


-- |Opaque type refering to a database.
-- Some safety is provided by the fact that the type constructor is not exported—for a caller to get
-- a DatabaseRef, they must call getDatabaseRef which first checks that the database exists and creates
-- it if necessary.
data DatabaseRef = DatabaseRef String


printSqlError :: SqlError -> IO ()
printSqlError e = putStrLn $ show e


-- |Checks that a database file exists, creates it if it doesn't, and returns a DatabaseRef to the database
getDatabaseRef :: String                         -- ^The name of the database file
               -> IO (Either String DatabaseRef)
getDatabaseRef filename = do
  fileExists <- doesFileExist filename
  maybeDB <- if fileExists
    then
      catchSql (Just <$> connectSqlite3 filename) $ \e -> printSqlError e >> return Nothing
    else
      catchSql (Just <$> createNewDatabase filename) $ \e -> printSqlError e >> return Nothing
  case maybeDB of
    Just db -> disconnect db >> (return $ Right $ DatabaseRef filename)
    Nothing -> return $ Left "Error"


-- Given a filename, create a new Sqlite database and set up the schema
createNewDatabase :: String -> IO Connection
createNewDatabase filename = do
  conn <- connectSqlite3 filename
  run conn
      "CREATE TABLE recordings (\n\
       \        id                  INTEGER PRIMARY KEY,\n\
       \        file                VARCHAR(1024),\n\
       \        title               VARCHAR(256),\n\
       \        artist              INTEGER,\n\
       \        album               INTEGER,\n\
       \        track_number        INTEGER,\n\
       \        FOREIGN KEY(album)  REFERENCES albums(id),\n\
       \        FOREIGN KEY(artist) REFERENCES artists(id)\n\
       \    );\n"
       []
  run conn
      "CREATE TABLE albums (\n\
       \        id                  INTEGER PRIMARY KEY,\n\
       \        title               VARCHAR(1024),\n\
       \        artist              INTEGER,\n\
       \        num_tracks          INTEGER,\n\
       \        FOREIGN KEY(artist) REFERENCES artists(id)\n\
       \    );\n"
       []
  run conn
      "CREATE TABLE artists (\n\
      \         id                  INTEGER,\n\
      \         name                VARCHAR(1024)\n\
      \    );\n"
      []
  run conn
      "CREATE TABLE last_ids (\n\
      \         recording_id  INTEGER,\n\
      \         album_id      INTEGER,\n\
      \         artist_id     INTEGER\n\
      \    );\n"
      []
  run conn "INSERT INTO last_ids (recording_id, album_id, artist_id) VALUES (0, 0, 0);" []
  commit conn
  return conn


-- Opens the database, calls a function which takes a database Connection, closes the
-- database, and returns the result of the called function.
withDatabase :: DatabaseRef -> ( Connection -> IO b ) -> IO b
withDatabase (DatabaseRef filename) action = do
  conn <- connectSqlite3 filename
  r <- action conn
  disconnect conn
  return r


-- |Given the name of an artist, returns a list of all Artist records that match that name exactly.
findArtists :: Text -> DatabaseRef -> IO [Artist]
findArtists name db = withDatabase db $ findArtists' name

-- |Like findArtists, but expects a Connection rather than a DatabaseRef.
findArtists' :: Text -> Connection -> IO [Artist]
findArtists' name conn = do
    r <- quickQuery' conn "SELECT id, name FROM artists WHERE name = ?;" [convert name]
    return $ map artistFromSql r
  where artistFromSql (idValue:nameValue:[]) = Artist (ArtistID (convert idValue)) (convert nameValue)


-- |Given an ArtistID, retrieves the Artist record from the database.
getArtist :: ArtistID -> DatabaseRef -> IO Artist
getArtist a db = withDatabase db $ getArtist' a

-- |Like getArtist, but expects a Connection rather than a DatabaseRef,
getArtist' :: ArtistID -> Connection -> IO Artist
getArtist' (ArtistID i) conn = do
    r <- quickQuery' conn "SELECT name FROM artists WHERE id = ?;" [convert i]
    case r of
      [[name]] -> return $ Artist (ArtistID i) (convert name)

-- |An artist which may not yet have been added to the database.
data NewArtist = NewArtist Name

-- |Add a new Artist to the Database. If successful, returns the new Artist record.
addArtist :: NewArtist -> DatabaseRef -> IO (Maybe Artist)
addArtist a db = withDatabase db $ addArtist' a

-- Like addArtist, but expects a Connection rather than a DatabaseRef
addArtist' :: NewArtist -> Connection -> IO (Maybe Artist)
addArtist' (NewArtist name) conn = do
    newID <- getNewArtistID $ conn
    run conn "INSERT INTO artists (id, name) VALUES (?, ?);" [convert newID, convert name]
    commit conn
    Just <$> getArtist' (ArtistID newID) conn

-- Returns an Integer that is not currently used as an ArtistID
getNewArtistID :: Connection -> IO Integer
getNewArtistID conn = do
    r <- quickQuery' conn "SELECT artist_id FROM last_ids" []
    let [[oldID]] = r
    let newID = (convert oldID) + 1;
    run conn "UPDATE last_ids SET artist_id=?" [convert newID]
    return newID

-- |Given the name of an Album, returns a list of all Album records that have that name.
findAlbums :: Text -> DatabaseRef -> IO [Album]
findAlbums title db = withDatabase db $ findAlbum' title

-- Like findAlbums, but expects a Connection rather than a DatabaseRef
findAlbum' :: Text -> Connection -> IO [Album]
findAlbum' title conn = do
    r <- quickQuery' conn "SELECT id FROM albums WHERE title = ?;" [convert title]
    forM r $ \x ->
      getAlbum' (convert1 x) conn

-- |Given an AlbumID, retrieve the corresponding Album record from the database.
getAlbum :: AlbumID -> DatabaseRef -> IO Album
getAlbum a db = withDatabase db $ getAlbum' a

-- Like getAlbum, but expects a Connection rather than a DatabaseRef
getAlbum' :: AlbumID -> Connection -> IO Album
getAlbum' albumID conn = do
    r <- quickQuery' conn "SELECT title, artist, num_tracks FROM albums WHERE id = ?;" [convert albumID]
    let (title, artistID, numTracks) = convert3 $ head r
    artist <- getArtist' artistID conn
    return $ Album albumID title artist numTracks

-- |An album which may not yet have been added to the database
data NewAlbum = NewAlbum Title ArtistID TrackCount

-- |Add a new Album to the database. If successful, returns the new Album record.
addAlbum :: NewAlbum -> DatabaseRef -> IO (Maybe Album)
addAlbum a db = withDatabase db $ addAlbum' a

getNewAlbumID :: Connection -> IO AlbumID
getNewAlbumID conn = do
    r <- quickQuery' conn "SELECT album_id FROM last_ids" []
    let [[oldID]] = r
    let newID = (convert oldID) + 1
    run conn "UPDATE last_ids SET album_id=?;" [convert newID]
    return $ AlbumID newID

addAlbum' :: NewAlbum -> Connection -> IO (Maybe Album)
addAlbum' (NewAlbum title artistID trackCount) conn = do
    newID <- getNewAlbumID conn
    run conn "INSERT INTO albums (id, title, artist, num_tracks) VALUES (?, ?, ?, ?);"
        [convert newID, convert title, convert artistID, convert trackCount]
    commit conn
    Just <$> getAlbum' newID conn

-- |Given a RecordingID, retrieve the corresponding Recording from the database.
getRecording :: RecordingID -> DatabaseRef -> IO Recording
getRecording r db = withDatabase db $ getRecording' r

-- Like getRecording, but expects a Connection rather than a DatabaseRef
getRecording' :: RecordingID -> Connection -> IO Recording
getRecording' recordingID conn = do
    r <- quickQuery' conn "SELECT file, title, artist, album, track_number FROM recordings WHERE id = ?;" [convert recordingID]
    let (file, title, artistID, albumID, trackNumber) = convert5 (head r)
    artist <- getArtist' artistID conn
    album <- getAlbum' albumID conn
    return $ Recording recordingID file title artist album trackNumber

-- |A recording which may not yet have been added to the database
data NewRecording = NewRecording FileName Title ArtistID AlbumID TrackNumber

-- |Add a new recording to the database. If successful, returns the new Recording record.
addRecording :: NewRecording -> DatabaseRef -> IO (Maybe Recording)
addRecording r db = withDatabase db $ addRecording' r

getNewRecordingID conn = do
    r <- quickQuery' conn "SELECT recording_id FROM last_ids" []
    let [[oldID]] = r
    let newID = (convert oldID) + 1
    run conn "UPDATE last_ids SET artist_id=?;" [convert newID]
    return $ RecordingID newID

addRecording' :: NewRecording -> Connection -> IO (Maybe Recording)
addRecording' (NewRecording filename title artistID albumID trackNumber) conn = do
    newID <- getNewRecordingID conn
    run conn "INSERT INTO recordings (id, title, artist, album, track_number) VALUES (?, ?, ?, ?, ?);"
        [convert newID, convert filename, convert title, convert artistID, convert albumID, convert trackNumber]
    commit conn
    Just <$> getRecording' newID conn




