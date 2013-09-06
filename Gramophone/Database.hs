{-# LANGUAGE  OverloadedStrings, MultiParamTypeClasses #-}

-- |Create and manage the main Gramophone database.
module Gramophone.Database 
    (
     DatabaseRef(),
     openDatabase,
     OpenError(..),
     createDatabase,
     CreateError(..),

     AudioFileName(..),
     Title,
     TrackNumber,
     TrackCount,
     Name,

     DBIO(),
     withDatabase,

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
     addRecording,
    ) where

import qualified Data.Text as T
import Data.Text (Text)

import qualified Database.HDBC.Sqlite3 as Sqlite
import Database.HDBC

import Control.Monad
import Control.Monad.Reader
import Control.Error
import Data.Functor
import qualified Data.Traversable

import Data.Convertible

import System.Directory (doesFileExist)


data Id a = Id Integer
    deriving Show


-- |Opaque type containing a unique identifier for a Recording
type RecordingID = Id Recording

-- |The name of an audio file
newtype AudioFileName = AudioFileName FilePath
    deriving Show
instance Convertible SqlValue AudioFileName where
    safeConvert = (fmap AudioFileName) . safeConvert
instance Convertible AudioFileName SqlValue where
     safeConvert (AudioFileName a) = safeConvert a

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
     recordingFile        :: AudioFileName,
     recordingTitle       :: Maybe Title,
     recordingArtist      :: Maybe Artist,
     recordingAlbum       :: Maybe Album,
     recordingTrackNumber :: Maybe TrackNumber
} deriving Show

-- |Opaque type containing a unique identifier for an Album
type AlbumID = Id Album

-- |Record describing an album
data Album = Album {
     albumId        :: AlbumID,
     albumTitle     :: Title,
     albumArtist    :: Maybe Artist,
     albumNumTracks :: TrackCount
} deriving Show

-- |Opaque type containing a unique identifier for an Artist
type ArtistID = Id Album

-- |Record describing a recording artist
data Artist = Artist {
     artistId   :: ArtistID,
     artistName :: Name
} deriving Show

instance Convertible SqlValue (Id a) where
     safeConvert = (fmap Id) . safeConvert

instance Convertible (Id a) SqlValue where
     safeConvert (Id val) = safeConvert val

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
-- a DatabaseRef, they must call getDatabaseRef which first checks that the database exists and creates
-- it if necessary.
data DatabaseRef = DatabaseRef String


printSqlError :: SqlError -> IO ()
printSqlError e = putStrLn $ show e

-- |Error values returned by openDatabase
data OpenError
    = OpenDoesNotExistError        -- ^ The given filename does not exist
    | OpenBadFormatError String    -- ^ The database schema is not recognized
    | OpenOldFormatError Integer   -- ^ The database is in an old format and must be migrated before use
    | OpenNewerFormatError Integer -- ^ The database format is newer than this version of Gramophone
    | OpenFileError String         -- ^ File exists but could not be opened. String is HDBC error message.
    deriving (Show, Eq)

-- |Opens a database.
openDatabase :: FilePath -> IO (Either OpenError DatabaseRef)
openDatabase filename = do
  fileExists <- doesFileExist filename
  if fileExists 
    then
      checkDatabase filename
    else
      return $ Left OpenDoesNotExistError
--  where
--    checkDatabase = do
--        conn <- Sqlite.connectSqlite3 filename
--        
--        disconnect conn
--        return $ Right $ DatabaseRef filename

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
getVersion conn = do
  (convert . head . head) <$> quickQuery' conn "SELECT version FROM database_info;" []


-- | Error values returned by createDatabase
data CreateError
    = CreateFileError String   -- ^ File could not be created. String is HDBC error message.
    | CreateAlreadyExistsError -- ^ File already exists.
    deriving (Show, Eq)

-- |Creates a new database. Will not overwrite an existing file
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


-- | Database actions
type DBIO a = ReaderT Sqlite.Connection IO a

-- Opens the database, performs the action DBIO, closes the
-- database, and returns the result of the action.
withDatabase :: DatabaseRef -> DBIO b -> IO b
withDatabase (DatabaseRef filename) action = do
  conn <- Sqlite.connectSqlite3 filename
  r <- runReaderT action conn
  disconnect conn
  return r


queryDB :: String -> [SqlValue] -> DBIO [[SqlValue]]
queryDB sql values = do
  conn <- ask
  liftIO $ quickQuery' conn sql values


runDB :: String -> [SqlValue] -> DBIO Integer
runDB sql values = do
  conn <- ask
  liftIO $ run conn sql values

commitDB :: DBIO ()
commitDB = do
  conn <- ask
  liftIO $ commit conn

wrapDB :: ( a -> DBIO b ) -> a -> DatabaseRef -> IO b
wrapDB f = \v -> \db -> withDatabase db $ f v

overMaybe :: Monad m => Functor m =>  (a -> m b) -> Maybe a -> m (Maybe b)
overMaybe = Data.Traversable.mapM
--overMaybe _ Nothing  = return Nothing
--overMaybe f (Just v) = Just <$> f v

-- |Given the name of an artist, returns a list of all Artist records that match that name exactly.
findArtists' :: Text -> DatabaseRef -> IO [Artist]
findArtists' = wrapDB findArtists

findArtists :: Text -> DBIO [Artist]
findArtists name = do
    r <- queryDB "SELECT id, name FROM artists WHERE name = ?;" [convert name]
    return $ map artistFromSql r
  where artistFromSql (idValue:nameValue:[]) = Artist (Id (convert idValue)) (convert nameValue)

-- |Given an ArtistID, retrieves the Artist record from the database.
getArtist' :: ArtistID -> DatabaseRef -> IO Artist
getArtist' = wrapDB getArtist

getArtist :: ArtistID -> DBIO Artist
getArtist (Id i) = do
  [[name]] <- queryDB "SELECT name FROM artists WHERE id = ?;" [convert i]
  return $ Artist (Id i) (convert name)

-- |An artist which may not yet have been added to the database.
data NewArtist = NewArtist Name

-- |Add a new Artist to the Database. If successful, returns the new Artist record.
addArtist' :: NewArtist -> DatabaseRef -> IO (Maybe Artist)
addArtist' = wrapDB addArtist

addArtist :: NewArtist -> DBIO (Maybe Artist)
addArtist (NewArtist name) = do
    newID <- getNewArtistID
    runDB "INSERT INTO artists (id, name) VALUES (?, ?);" [convert newID, convert name]
    commitDB
    Just <$> getArtist (Id newID)

-- Returns an Integer that is not currently used as an ArtistID
getNewArtistID :: DBIO Integer
getNewArtistID = do
  [[oldID]] <- queryDB "SELECT artist_id FROM last_ids" []
  let newID = (convert oldID) + 1;
  runDB "UPDATE last_ids SET artist_id=?" [convert newID]
  return newID

-- |Given the name of an Album, returns a list of all Album records that have that name.
findAlbums' :: Text -> DatabaseRef -> IO [Album]
findAlbums' = wrapDB findAlbums

findAlbums :: Text -> DBIO [Album]
findAlbums title = do
    r <- queryDB "SELECT id FROM albums WHERE title = ?;" [convert title]
    forM r $ \x ->
      getAlbum (convert1 x)

-- |Given an AlbumID, retrieve the corresponding Album record from the database.
getAlbum' :: AlbumID -> DatabaseRef -> IO Album
getAlbum' = wrapDB getAlbum

getAlbum :: AlbumID -> DBIO Album
getAlbum albumID = do
    r <- queryDB "SELECT title, artist, num_tracks FROM albums WHERE id = ?;" [convert albumID]
    let (title, artistID, numTracks) = convert3 $ head r
    artist <- overMaybe getArtist artistID
    return $ Album albumID title artist numTracks

-- |An album which may not yet have been added to the database
data NewAlbum = NewAlbum Title ArtistID TrackCount

-- |Add a new Album to the database. If successful, returns the new Album record.
addAlbum' :: NewAlbum -> DatabaseRef -> IO (Maybe Album)
addAlbum' = wrapDB addAlbum

getNewAlbumID :: DBIO AlbumID
getNewAlbumID = do
    [[oldID]] <- queryDB "SELECT album_id FROM last_ids" []
    let newID = (convert oldID) + 1
    runDB "UPDATE last_ids SET album_id=?;" [convert newID]
    return $ Id newID

addAlbum :: NewAlbum -> DBIO (Maybe Album)
addAlbum (NewAlbum title artistID trackCount) = do
    newID <- getNewAlbumID
    runDB "INSERT INTO albums (id, title, artist, num_tracks) VALUES (?, ?, ?, ?);"
          [convert newID, convert title, convert artistID, convert trackCount]
    commitDB
    Just <$> getAlbum newID 

-- |Given a RecordingID, retrieve the corresponding Recording from the database.
getRecording' :: RecordingID -> DatabaseRef -> IO Recording
getRecording' = wrapDB getRecording

getRecording :: RecordingID -> DBIO Recording
getRecording recordingID = do
    r <- queryDB "SELECT file, title, artist, album, track_number FROM recordings WHERE id = ?;" [convert recordingID]
    let (file, title, artistID, albumID, trackNumber) = convert5 (head r)
    artist <- overMaybe getArtist artistID
    album <- overMaybe getAlbum albumID
    return $ Recording recordingID file title artist album trackNumber

-- |A recording which may not yet have been added to the database
data NewRecording = NewRecording AudioFileName (Maybe Title) (Maybe ArtistID) (Maybe AlbumID) (Maybe TrackNumber)

-- |Add a new recording to the database. If successful, returns the new Recording record.
addRecording' :: NewRecording -> DatabaseRef -> IO (Maybe Recording)
addRecording' = wrapDB addRecording

getNewRecordingID :: DBIO Integer
getNewRecordingID = do
    [[oldID]] <- queryDB "SELECT recording_id FROM last_ids" []
    let newID = (convert oldID) + 1
    runDB "UPDATE last_ids SET artist_id=?;" [convert newID]
    return newID

addRecording :: NewRecording -> DBIO (Maybe Recording)
addRecording (NewRecording filename title artistID albumID trackNumber) = do
    newID <- Id <$> getNewRecordingID
    runDB "INSERT INTO recordings (id, title, artist, album, track_number) VALUES (?, ?, ?, ?, ?);"
        [convert newID, convert filename, convert title, convert artistID, convert albumID, convert trackNumber]
    commitDB
    Just <$> getRecording newID




