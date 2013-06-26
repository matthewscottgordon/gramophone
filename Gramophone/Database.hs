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
      \         id                  INTEGER PRIMARY KEY,\n\
      \         name                VARCHAR(1024)\n\
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