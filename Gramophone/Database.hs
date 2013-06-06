module Gramophone.Database 
    (
     Connection(),
     openDatabase,
     closeDatabase,
     openOrCreateDatabase,
     DatabaseRef(),
     getDatabaseRef
    ) where

import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified Database.HDBC.Sqlite3 (Connection)
import Database.HDBC

import Control.Monad
import Data.Functor

import System.Directory (doesFileExist)


data Connection = Connection Database.HDBC.Sqlite3.Connection

data DatabaseRef = DatabaseRef String

getDatabaseRef :: String -> IO (Either String DatabaseRef)
getDatabaseRef filename = do
  fileExists <- doesFileExist filename
  maybeDB <- if fileExists
    then
      catchSql (Just <$> openDatabase filename) $ \e -> return Nothing
    else
      Just <$> createNewDatabase filename
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
  r <- quickQuery' conn
                   "CREATE TABLE tracks (\n\
                   \        track_id            INTEGER UNIQUE,\n\
                   \        file                VARCHAR(1024),\n\
                   \        title               VARCHAR(256),\n\
                   \        album               VARCHAR(256),\n\
                   \        track_number        INTEGER,\n\
                   \        num_tracks_in_album INTEGER,\n\
                   \        disc_number         INTEGER,\n\
                   \        num_discs_in_album  INTEGER,\n\
                   \        artist              VARCHAR(256)\n\
                   \    );\n"
                   []
  commit conn
  return (Connection conn)

withDatabase :: DatabaseRef -> ( Connection -> IO b ) -> IO b
withDatabase (DatabaseRef filename) action = do
  db <- openDatabase filename
  r <- action db
  closeDatabase db
  return r