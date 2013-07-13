{-# LANGUAGE  OverloadedStrings, MultiParamTypeClasses #-}

module Gramophone.Database 
    (
     DatabaseRef(),
     getDatabaseRef,

     Artist(..),
     ArtistID(),
     findArtist,
     getArtist,
     NewArtist(..),
     addArtist,
     Album(..),
     AlbumID(),
     findAlbums,
     getAlbum,
     Recording(..),
     RecordingID(),
     getRecording
    ) where

import qualified Data.Text as T
import Data.Text(Text)

import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified Database.HDBC.Sqlite3 (Connection)
import Database.HDBC

import Control.Monad
import Data.Functor

import Data.Convertible

import System.Directory (doesFileExist)



data RecordingID = RecordingID Integer deriving Show

data Recording = Recording {
     recordingId          :: RecordingID,
     recordingFile        :: Text,
     recordingTitle       :: Text,
     recordingArtist      :: Artist,
     recordingAlbum       :: Album,
     recordingTrackNumber :: Integer
} deriving Show

data AlbumID = AlbumID Integer deriving Show

data Album = Album {
     albumId        :: AlbumID,
     albumTitle     :: Text,
     albumArtist    :: Artist,
     albumNumTracks :: Integer
} deriving Show

data ArtistID = ArtistID Integer deriving Show

data Artist = Artist {
     artistId   :: ArtistID,
     artistName :: Text
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

data Connection = Connection Database.HDBC.Sqlite3.Connection

data DatabaseRef = DatabaseRef String


printSqlError :: SqlError -> IO ()
printSqlError e = putStrLn $ show e


getDatabaseRef :: String -> IO (Either String DatabaseRef)
getDatabaseRef filename = do
  fileExists <- doesFileExist filename
  maybeDB <- if fileExists
    then
      catchSql (Just <$> openDatabase filename) $ \e -> printSqlError e >> return Nothing
    else
      catchSql (Just <$> createNewDatabase filename) $ \e -> printSqlError e >> return Nothing
  case maybeDB of
    Just db -> closeDatabase db >> (return $ Right $ DatabaseRef filename)
    Nothing -> return $ Left "Error"

openDatabase :: String -> IO Connection
openDatabase = return . Connection <=< connectSqlite3


closeDatabase :: Connection -> IO ()
closeDatabase (Connection conn) = disconnect conn



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
  return (Connection conn)

withDatabase :: DatabaseRef -> ( Connection -> IO b ) -> IO b
withDatabase (DatabaseRef filename) action = do
  Connection conn <- openDatabase filename
  r <- action (Connection conn)
  disconnect conn
  return r


findArtist :: Text -> DatabaseRef -> IO [Artist]
findArtist name db = withDatabase db $ findArtist' name

findArtist' :: Text -> Connection -> IO [Artist]
findArtist' name (Connection conn) = do
    r <- quickQuery' conn "SELECT id, name FROM artists WHERE name = ?;" [convert name]
    return $ map artistFromSql r
  where artistFromSql (idValue:nameValue:[]) = Artist (ArtistID (convert idValue)) (convert nameValue)


getArtist :: ArtistID -> DatabaseRef -> IO Artist
getArtist a db = withDatabase db $ getArtist' a

getArtist' :: ArtistID -> Connection -> IO Artist
getArtist' (ArtistID i) (Connection conn) = do
    r <- quickQuery' conn "SELECT name FROM artists WHERE id = ?;" [convert i]
    case r of
      [[name]] -> return $ Artist (ArtistID i) (convert name)

data NewArtist = NewArtist {
     newArtistName :: Text
}

addArtist :: NewArtist -> DatabaseRef -> IO (Maybe Artist)
addArtist a db = withDatabase db $ addArtist' a

addArtist' :: NewArtist -> Connection -> IO (Maybe Artist)
addArtist' newArtist (Connection conn) = do
    --newID <- fromIntegral . length <$> findArtist' (newArtistName newArtist) (Connection conn)
    newID <- getNewArtistID' $ Connection conn
    run conn "INSERT INTO artists (id, name) VALUES (?, ?);" [(convert newID), (convert $ newArtistName newArtist)]
    commit conn
    Just <$> getArtist' (ArtistID newID) (Connection conn)

getNewArtistID' :: Connection -> IO Integer
getNewArtistID' (Connection conn) = do
    r <- quickQuery' conn "SELECT artist_id FROM last_ids" []
    let [[oldID]] = r
    let newID = (convert oldID) + 1;
    run conn "UPDATE last_ids SET artist_id=?" [convert newID]
    return newID


findAlbums :: Text -> DatabaseRef -> IO [Album]
findAlbums title db = withDatabase db $ findAlbum' title

findAlbum' :: Text -> Connection -> IO [Album]
findAlbum' title (Connection conn) = do
    r <- quickQuery' conn "SELECT id FROM albums WHERE title = ?;" [convert title]
    forM r $ \x ->
      getAlbum' (convert1 x) (Connection conn)

getAlbum :: AlbumID -> DatabaseRef -> IO Album
getAlbum a db = withDatabase db $ getAlbum' a

getAlbum' :: AlbumID -> Connection -> IO Album
getAlbum' albumID (Connection conn) = do
    r <- quickQuery' conn "SELECT title, artist, num_tracks FROM albums WHERE id = ?;" [convert albumID]
    let (title, artistID, numTracks) = convert3 $ head r
    artist <- getArtist' artistID (Connection conn)
    return $ Album albumID title artist numTracks

getRecording :: RecordingID -> DatabaseRef -> IO Recording
getRecording r db = withDatabase db $ getRecording' r

getRecording' :: RecordingID -> Connection -> IO Recording
getRecording' recordingID (Connection conn) = do
    r <- quickQuery' conn "SELECT file, title, artist, album, track_number FROM recordings WHERE id = ?;" [convert recordingID]
    let (file, title, artistID, albumID, trackNumber) = convert5 (head r)
    artist <- getArtist' artistID (Connection conn)
    album <- getAlbum' albumID (Connection conn)
    return $ Recording recordingID file title artist album trackNumber