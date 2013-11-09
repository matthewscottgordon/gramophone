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
import Control.Monad.Trans
import Control.Error
import Control.Applicative
import qualified Data.Traversable

import Data.Convertible

import System.Directory (doesFileExist)

import Gramophone.Core.Database.Monad


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
getVersion conn = do
  (convert . head . head) <$> quickQuery' conn "SELECT version FROM database_info;" []


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

wrapDB :: (MonadIO m, Functor m) => ( a -> DBT m b ) -> a -> DatabaseRef -> m b
wrapDB f = \v -> \db -> withDatabase db $ f v

overMaybe :: Monad m => Functor m =>  (a -> m b) -> Maybe a -> m (Maybe b)
overMaybe = Data.Traversable.mapM

findArtists' :: (MonadIO m, Functor m) => ArtistName -> DatabaseRef -> m [Artist]
findArtists' = wrapDB findArtists

-- |Given the name of an artist, returns a list of all Artist records that match that name exactly.
findArtists :: MonadDB m => ArtistName -> m [Artist]
findArtists name = do
    r <- queryDB "SELECT id, name FROM artists WHERE name = ?;" [convert name]
    return $ map artistFromSql r
  where artistFromSql (idValue:nameValue:[]) = Artist (Id (convert idValue)) (convert nameValue)

getArtist' :: ArtistID -> DatabaseRef -> IO Artist
getArtist' = wrapDB getArtist

-- |Given an ArtistID, retrieves the Artist record from the database.
getArtist :: MonadDB m => ArtistID -> m Artist
getArtist (Id i) = do
  [[name]] <- queryDB "SELECT name FROM artists WHERE id = ?;" [convert i]
  return $ Artist (Id i) (convert name)

-- |An artist which may not yet have been added to the database.
data NewArtist = NewArtist ArtistName

addArtist' :: NewArtist -> DatabaseRef -> IO (Maybe Artist)
addArtist' = wrapDB addArtist

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

findAlbums' :: AlbumTitle -> DatabaseRef -> IO [Album]
findAlbums' = wrapDB findAlbums

-- |Given the name of an Album, returns a list of all Album records that have that name.
findAlbums :: MonadDB m => AlbumTitle -> m [Album]
findAlbums title = do
    r <- queryDB "SELECT id FROM albums WHERE title = ?;" [convert title]
    forM r $ \x ->
      getAlbum (convert1 x)

getAlbum' :: AlbumID -> DatabaseRef -> IO Album
getAlbum' = wrapDB getAlbum

-- |Given an AlbumID, retrieve the corresponding Album record from the database.
getAlbum :: MonadDB m => AlbumID -> m Album
getAlbum albumID = do
    r <- queryDB "SELECT title, artist, num_tracks FROM albums WHERE id = ?;" [convert albumID]
    let (title, artistID, numTracks) = convert3 $ head r
    artist <- overMaybe getArtist artistID
    return $ Album albumID title artist numTracks

-- |An album which may not yet have been added to the database
data NewAlbum = NewAlbum AlbumTitle (Maybe ArtistID) TrackCount

addAlbum' :: NewAlbum -> DatabaseRef -> IO (Maybe Album)
addAlbum' = wrapDB addAlbum

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

getRecording' :: RecordingID -> DatabaseRef -> IO Recording
getRecording' = wrapDB getRecording

-- |Given a RecordingID, retrieve the corresponding Recording from the database.
getRecording :: MonadDB m => RecordingID -> m Recording
getRecording recordingID = do
    r <- queryDB "SELECT file, title, artist, album, track_number FROM recordings WHERE id = ?;" [convert recordingID]
    let (file, title, artistID, albumID, trackNumber) = convert5 (head r)
    artist <- overMaybe getArtist artistID
    album <- overMaybe getAlbum albumID
    return $ Recording recordingID file title artist album trackNumber

-- |A recording which may not yet have been added to the database
data NewRecording = NewRecording AudioFileName (Maybe RecordingTitle) (Maybe ArtistID) (Maybe AlbumID) (Maybe TrackNumber)

addRecording' :: NewRecording -> DatabaseRef -> IO (Maybe Recording)
addRecording' = wrapDB addRecording

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




