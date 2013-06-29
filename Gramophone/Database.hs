{-# LANGUAGE  OverloadedStrings #-}

module Gramophone.Database 
    (
     Connection(),
     openDatabase,
     closeDatabase,
     openOrCreateDatabase,
     DatabaseRef(),
     getDatabaseRef,

     Artist,
     ArtistID(),
     findArtist,
     Album,
     AlbumID(),
     findAlbum,
     Recording,
     RecordingID()
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


openOrCreateDatabase :: String -> IO Connection
openOrCreateDatabase filename = do
  fileExists <- doesFileExist filename
  if fileExists
    then
      openDatabase filename
    else
      createNewDatabase filename


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

findAlbum :: Text -> DatabaseRef -> IO [Album]
findAlbum name db = do
    let DatabaseRef filename = db
    Connection conn <- openDatabase filename
    r <- quickQuery conn "SELECT id, title, artist, num_tracks FROM artists WHERE name = ?;" [convert name]
    r' <- mapM (albumFromSql db) r
    disconnect conn
    return r'
  where albumFromSql db (idV:titleV:artistV:numTracksV:[]) = do
                                            artist <- getArtist (ArtistID (convert artistV)) db
                                            return $ Album (AlbumID (convert idV)) (convert titleV) artist (convert numTracksV)