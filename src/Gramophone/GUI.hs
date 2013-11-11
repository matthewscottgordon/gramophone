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

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

module Gramophone.GUI (startGUI) where

import qualified Gramophone.Core.Database as DB
import qualified Gramophone.Core.MediaController as MC

import Control.Monad (forM_,filterM,liftM)
import Control.Applicative((<$>))
import System.Directory (createDirectoryIfMissing,getHomeDirectory)
import Yesod
import Text.Julius(rawJS)

import qualified Data.Text as T
import qualified System.FilePath as FilePath
import System.Directory(getDirectoryContents,doesDirectoryExist,doesFileExist)
import System.IO.Error(isDoesNotExistError,isPermissionError)
import Control.Exception(try)
import Data.List(sort)

--import qualified Network.Wai.Handler.Webkit as Webkit


data RawFilePath = RawFilePath FilePath.FilePath
                   deriving(Show, Eq, Read)


instance PathMultiPiece RawFilePath where
    toPathMultiPiece (RawFilePath p) = map T.pack $ FilePath.splitDirectories p
    fromPathMultiPiece ts = let filePath = FilePath.joinPath $ map T.unpack ts
                            in if FilePath.isValid filePath
                              then Just $ RawFilePath filePath
                              else Nothing



data Website = Website DB.DatabaseRef

mkYesod "Website" [parseRoutes|
/                        TestR GET
/FileSystem/*RawFilePath BrowseForFilesR GET
|]

instance Yesod Website

getTestR :: Handler Html
getTestR = defaultLayout $ do
    setTitle "Gramophone - Testing Functions"
    browseFilesStartDir <- liftIO $ RawFilePath <$> getHomeDirectory
    [whamlet|
              <body>
                  <h1>Testing Functions
                  <a href=@{BrowseForFilesR browseFilesStartDir}>Browse for Files|]

fileListWidget :: [FilePath.FilePath] -> Widget
fileListWidget files = do
  fileListID <- newIdent
  toWidget [hamlet|
            <div>
              <h2>Files:
              <ol id="#{fileListID}">
                $forall file <- sort files
                  <li>#{FilePath.takeFileName file}|]
  toWidget [cassius|
            ol##{fileListID}
               list-style-type: none|]
               
              

dirListWidget :: [FilePath.FilePath] -> Widget
dirListWidget dirs = do
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
  divID <- newIdent
  dirListID <- newIdent
  toWidget [hamlet|
            <div id="#{divID}">
              <h2>Subfolders:
              <ol id="#{dirListID}">
                $forall dir <- sort dirs
                  <li><a href=@{BrowseForFilesR (RawFilePath dir)}>#{FilePath.takeFileName dir}</a>
              <br>|]
  toWidget [cassius|
            ol##{dirListID}
              list-style-type:none
            ol##{dirListID} li
              float: left
              min-width: 5em
              white-space: nowrap
            div##{divID} br
              clear: left
            div##{divID}
              margin-bottom: 1em|]
  -- This javascript finds the largest width of any list item (directory name) and sets the width
  -- of all the list items to that width. Together with the above CSS, this creates a multi-column
  -- list where the columns are always wide enough to accomodate their contents. Each column has
  -- a minumum width of 5 ems.
  toWidget [julius|
            $('ol##{rawJS dirListID}').ready(function() {
              elementWidth = Math.max.apply( null, $('ol##{rawJS dirListID} > li').map( function() {
                return $(this).innerWidth(true);
              }).get() );
              $('ol##{rawJS dirListID} > li').width(elementWidth);
            });|]

getBrowseForFilesR :: RawFilePath -> Handler Html
getBrowseForFilesR (RawFilePath path) = defaultLayout $ do
  dirContentsOrError <- liftIO $ try $ (liftM $ map $ FilePath.combine path) $ getDirectoryContents path
  case dirContentsOrError of
    Left e | isDoesNotExistError e -> notFound
           | isPermissionError e   -> permissionDenied $ T.concat ["You are not allowed access to \"", (T.pack path), "\""]
           | otherwise             -> notFound
    Right dirContents -> do
      setTitle "Gramophone - Browse Filesystem"
      toWidget [hamlet|<h1>#{path}|]
      
      files <- liftIO $ filterM doesFileExist $ map (FilePath.combine path) dirContents
      fileListWidget files
      
      subDirs <- liftIO $ filterM doesDirectoryExist $ map (FilePath.combine path) dirContents
      dirListWidget subDirs

      [whamlet|<div><a href=@{TestR}>Back to testing functions</a>|]


startGUI :: DB.DatabaseRef -> IO ()
--startGUI dbRef = do
--  app <- toWaiApp (Website dbRef)
--  Webkit.run "Gramophone" app
startGUI dbRef = warp 3000 $ Website dbRef