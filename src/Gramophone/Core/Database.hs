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

-- |Create and manage the main Gramophone database.
module Gramophone.Core.Database 
    (
     DatabaseRef(),
     openDatabase,
     OpenError(..),
     createDatabase,
     CreateError(..),

     AudioFileName(..),
     RecordingTitle(..),
     AlbumTitle(..),
     TrackNumber(..),
     TrackCount(..),
     ArtistName(..),

     MonadDB,
     DBT,
     withDatabase,

     Artist(..),
     ArtistID(),
     getAllArtists,
     findArtists,
     getArtist,
     NewArtist(..),
     addArtist,

     Album(..),
     AlbumID(),
     findAlbums,
     getAlbum,
     getAllAlbums,
     NewAlbum(..),
     addAlbum,

     Recording(..),
     RecordingID(),
     getAllRecordings,
     findRecordings,
     getRecording,
     NewRecording(..),
     addRecording
    ) where

import Prelude hiding (mapM)

import qualified Data.Text as T
import Data.Text (Text)

import qualified Database.HDBC.Sqlite3 as Sqlite
import Database.HDBC

import Control.Monad hiding (mapM, forM)
import Control.Monad.Trans
import Control.Error
import Control.Applicative
import Data.Traversable

import Data.Convertible

import System.Directory (doesFileExist)

import Gramophone.Core.Database.Monad
import Gramophone.Core.Database.Types
import Gramophone.Core.Database.Query



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


databaseMagic :: Text
databaseMagic = "Gramophone"
databaseCurrentVersion :: Integer
databaseCurrentVersion = 0


-- |Opaque type refering to a database.
-- Some safety is provided by the fact that the type constructor is not exported—for a caller to get
-- a DatabaseRef, they must call createDatabase of openDatabase.
-- it if necessary.
data DatabaseRef = DatabaseRef String


-- |Error values returned by openDatabase and createDatabase
data OpenError
    = OpenDoesNotExistError        -- ^ The given filename does not exist
    | OpenBadFormatError String    -- ^ The database schema is not recognized
    | OpenOldFormatError Integer   -- ^ The database is in an old format and must be migrated before use
    | OpenNewerFormatError Integer -- ^ The database format is newer than this version of Gramophone
    | OpenFileError String         -- ^ File exists but could not be opened. String is HDBC error message.
    deriving (Show, Eq)

-- |Opens a database and checks that it is a valid gramophone database.
openDatabase :: FilePath -> IO (Either OpenError DatabaseRef)
openDatabase filename = do
  fileExists <- doesFileExist filename
  if fileExists 
    then
      checkDatabase filename
    else
      return $ Left OpenDoesNotExistError

catchSql' :: (IO a) -> (SqlError -> IO b) -> (EitherT b IO a)
catchSql' f handler = do
    r <- liftIO $ catchSql (Right <$> f) (wrapLeft handler)
    case r of
      Right v -> return v
      Left e  -> left e
  where
    wrapLeft h e = do
      r <- h e
      return $ Left r
  
checkDatabase :: FilePath -> IO (Either OpenError DatabaseRef)
checkDatabase filepath = runEitherT $ do
  conn <- catchSql' (Sqlite.connectSqlite3 filepath) (\e -> return $ OpenFileError $ show e)
  magic <- catchSql' (getMagic conn) (\e -> return $ OpenBadFormatError $ show e)
  unless (magic == databaseMagic) $ left (OpenBadFormatError "Unrecognized Magic")
  version <- catchSql' (getVersion conn) (\e -> return $ OpenBadFormatError $ show e)
  when (version < databaseCurrentVersion) $ left (OpenOldFormatError version)
  when (version > databaseCurrentVersion) $ left (OpenNewerFormatError version)
  liftIO $ disconnect conn
  return $ DatabaseRef filepath

getMagic :: Sqlite.Connection -> IO Text
getMagic conn = do
  r <- quickQuery' conn "SELECT magic FROM database_info;" []
  if (length r /= 1)
    then
      return ""
    else
      return $ convert $ head $ head r

getVersion :: Sqlite.Connection -> IO Integer
getVersion conn = (convert . head . head) <$> quickQuery' conn "SELECT version FROM database_info;" []


-- | Error values returned by createDatabase
data CreateError
    = CreateFileError String   -- ^ File could not be created. String is HDBC error message.
    | CreateAlreadyExistsError -- ^ File already exists.
    deriving (Show, Eq)

-- |Creates and initializes a new database. Will not overwrite an existing file
createDatabase :: FilePath -> IO (Either CreateError DatabaseRef)
createDatabase filename = do
  fileExists <- doesFileExist filename
  if fileExists
    then
      return $ Left CreateAlreadyExistsError
    else
      catchSql ( Right <$> createDatabase' filename) $ \e -> return $ Left $ CreateFileError $ show e


createDatabase' :: FilePath -> IO DatabaseRef
createDatabase' filename = do
  conn <- Sqlite.connectSqlite3 filename
  initSchema conn
  disconnect conn
  return $ DatabaseRef filename

initSchema :: Sqlite.Connection -> IO ()
initSchema conn = do
  run conn
      "CREATE TABLE recordings (\n\
       \        id                  INTEGER PRIMARY KEY,\n\
       \        file                VARCHAR(1024) NOT NULL UNIQUE,\n\
       \        title               VARCHAR(256),\n\
       \        artist              INTEGER,\n\
       \        album               INTEGER,\n\
       \        track_number        INTEGER,\n\
       \        FOREIGN KEY(album)  REFERENCES albums(id),\n\
       \        FOREIGN KEY(artist) REFERENCES artists(id),\n\
       \        UNIQUE(album,track_number)\n\
       \    );\n"
       []
  run conn
      "CREATE TABLE albums (\n\
       \        id                  INTEGER PRIMARY KEY,\n\
       \        title               VARCHAR(1024) NOT NULL,\n\
       \        artist              INTEGER,\n\
       \        num_tracks          INTEGER NOT NULL,\n\
       \        FOREIGN KEY(artist) REFERENCES artists(id)\n\
       \    );\n"
       []
  run conn
      "CREATE TABLE artists (\n\
      \         id                  INTEGER PRIMARY KEY,\n\
      \         name                VARCHAR(1024) NOT NULL\n\
      \    );\n"
      []
  run conn
      "CREATE TABLE last_ids (\n\
      \         recording_id  INTEGER PRIMARY KEY,\n\
      \         album_id      INTEGER,\n\
      \         artist_id     INTEGER\n\
      \    );\n"
      []
  run conn "INSERT INTO last_ids (recording_id, album_id, artist_id) VALUES (0, 0, 0);" []
  run conn "CREATE TABLE database_info ( magic CHARACTER(10) NOT NULL, version INTEGER NOT NULL );" []
  run conn "INSERT INTO database_info ( magic, version ) VALUES (?,?);"
          [convert databaseMagic, convert databaseCurrentVersion]
  commit conn



-- |Opens the database, performs the MonadDB action, closes the
--  database, and returns the result of the action.
withDatabase :: (MonadIO m, Functor m) => DatabaseRef -> DBT m b -> m b
withDatabase (DatabaseRef filename) action = do
  conn <- liftIO $ Sqlite.connectSqlite3 filename
  r <- runDBT action conn
  liftIO $ disconnect conn
  return r


queryDB :: (MonadDB m) => String -> [SqlValue] -> m [[SqlValue]]
queryDB sql values = do
  conn <- getConn
  liftIO $ quickQuery' conn sql values


runDB :: MonadDB m => String -> [SqlValue] -> m Integer
runDB sql values = do
  conn <- getConn
  liftIO $ run conn sql values

commitDB :: MonadDB m => m ()
commitDB = do
  conn <- getConn
  liftIO $ commit conn

-- |Given the name of an artist, returns a list of all Artist records that match that name exactly.
findArtists :: MonadDB m => ArtistName -> m [Artist]
findArtists name = (map artistFromSql) <$> queryDB "SELECT id, name FROM artists WHERE name = ?;" [convert name]
  where artistFromSql (idValue:nameValue:[]) = Artist (Id (convert idValue)) (convert nameValue)

-- |Given an ArtistID, retrieves the Artist record from the database.
getArtist :: MonadDB m => ArtistID -> m Artist
getArtist = getRow
  
-- |Returns a list containing the ArtistID of every artist in the database
getAllArtists :: MonadDB m => m [ArtistID]
getAllArtists = queryId NoConstraint


-- |An artist which may not yet have been added to the database.
data NewArtist = NewArtist ArtistName

-- |Add a new Artist to the Database. If successful, returns the new Artist record.
addArtist :: MonadDB m => NewArtist -> m (Maybe Artist)
addArtist (NewArtist name) = do
    newID <- getNewArtistID
    runDB "INSERT INTO artists (id, name) VALUES (?, ?);" [convert newID, convert name]
    commitDB
    Just <$> getArtist (Id newID)

-- Returns an Integer that is not currently used as an ArtistID
getNewArtistID :: MonadDB m => m Integer
getNewArtistID = do
  [[oldID]] <- queryDB "SELECT artist_id FROM last_ids" []
  let newID = (convert oldID) + 1;
  runDB "UPDATE last_ids SET artist_id=?" [convert newID]
  return newID
  
-- |Given the name of an Album, returns a list of all Album records that have that name.
findAlbums :: MonadDB m => AlbumTitle -> m [Album]
findAlbums title = mapM getAlbum =<< queryId (EqualsConstraint albumTitleColumn title)


-- |Given an AlbumID, retrieve the corresponding Album record from the database.
getAlbum :: MonadDB m => AlbumID -> m Album
getAlbum = getRow
  
    
-- |Returns a list containing the AlbumID of every album in the database
getAllAlbums :: MonadDB m => m [AlbumID]
getAllAlbums = queryId NoConstraint


-- |An album which may not yet have been added to the database
data NewAlbum = NewAlbum AlbumTitle (Maybe ArtistID) TrackCount

getNewAlbumID :: MonadDB m => m AlbumID
getNewAlbumID = do
    [[oldID]] <- queryDB "SELECT album_id FROM last_ids" []
    let newID = (convert oldID) + 1
    runDB "UPDATE last_ids SET album_id=?;" [convert newID]
    return $ Id newID

-- |Add a new Album to the database. If successful, returns the new Album record.
addAlbum :: MonadDB m => NewAlbum -> m (Maybe Album)
addAlbum (NewAlbum title artistID trackCount) = do
    newID <- getNewAlbumID
    runDB "INSERT INTO albums (id, title, artist, num_tracks) VALUES (?, ?, ?, ?);"
          [convert newID, convert title, convert artistID, convert trackCount]
    commitDB
    Just <$> getAlbum newID 

-- |Given a RecordingID, retrieve the corresponding Recording from the database.
getRecording :: MonadDB m => (Id Recording) -> m Recording
getRecording = getRow
    
-- |Returns a list containing the ArtistID of every artist in the database
getAllRecordings :: MonadDB m => m [Id Recording]
getAllRecordings = queryId NoConstraint


-- |A recording which may not yet have been added to the database
data NewRecording = NewRecording AudioFileName (Maybe RecordingTitle) (Maybe ArtistID) (Maybe AlbumID) (Maybe TrackNumber)

getNewRecordingID :: MonadDB m => m Integer
getNewRecordingID = do
    [[oldID]] <- queryDB "SELECT recording_id FROM last_ids" []
    let newID = (convert oldID) + 1
    runDB "UPDATE last_ids SET recording_id=?;" [convert newID]
    return newID

-- |Add a new recording to the database. If successful, returns the new Recording record.
addRecording :: MonadDB m => NewRecording -> m (Maybe Recording)
addRecording (NewRecording filename title artistID albumID trackNumber) = do
    newID <- Id <$> getNewRecordingID
    runDB "INSERT INTO recordings (id, file, title, artist, album, track_number) VALUES (?, ?, ?, ?, ?, ?);"
        [convert newID, convert filename, convert title, convert artistID, convert albumID, convert trackNumber]
    commitDB
    Just <$> getRecording newID


findRecordings :: MonadDB m => RecordingTitle -> m [Recording]
findRecordings title = mapM getRecording =<< queryId (EqualsConstraint recordingTitleColumn (Just title))


