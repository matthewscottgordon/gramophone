module Gramophone.Database 
    (
     Connection(),
     openDatabase,
     closeDatabase,
     openOrCreateDatabase
    ) where

import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified Database.HDBC.Sqlite3 (Connection)
import Database.HDBC

import Control.Monad

import System.Directory (doesFileExist)


data Connection = Connection Database.HDBC.Sqlite3.Connection

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
                   "CREATE TABLE recordings (\n\
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