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

module Gramophone.GUI.BrowseForFiles (getBrowseForFilesR) where

import Prelude hiding (concat)
import Gramophone.GUI.Foundation

import Yesod (addScriptRemote, newIdent, toWidget, hamlet, cassius, julius, Html, defaultLayout,
              liftIO, notFound, permissionDenied, setTitle, whamlet)
import Text.Julius (rawJS)

import System.FilePath (takeFileName, combine)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import Data.List (sort)
import Control.Exception (try)
import Control.Monad(liftM, filterM)
import Data.Text (pack, concat)

fileListWidget :: [FilePath] -> Widget
fileListWidget files = do
  fileListID <- newIdent
  toWidget [hamlet|
            <div>
              <h2>Files:
              <ol id="#{fileListID}">
                $forall file <- sort files
                  <li>#{takeFileName file}|]
  toWidget [cassius|
            ol##{fileListID}
               list-style-type: none|]
               
              

dirListWidget :: [FilePath] -> Widget
dirListWidget dirs = do
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
  divID <- newIdent
  dirListID <- newIdent
  toWidget [hamlet|
            <div id="#{divID}">
              <h2>Subfolders:
              <ol id="#{dirListID}">
                $forall dir <- sort dirs
                  <li><a href=@{BrowseForFilesR (RawFilePath dir)}>#{takeFileName dir}</a>
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
  dirContentsOrError <- liftIO $ try $ (liftM $ map $ combine path) $ getDirectoryContents path
  case dirContentsOrError of
    Left e | isDoesNotExistError e -> notFound
           | isPermissionError e   -> permissionDenied $ concat ["You are not allowed access to \"", (pack path), "\""]
           | otherwise             -> notFound
    Right dirContents -> do
      setTitle "Gramophone - Browse Filesystem"
      toWidget [hamlet|<h1>#{path}|]
      
      files <- liftIO $ filterM doesFileExist $ map (combine path) dirContents
      fileListWidget files
      
      subDirs <- liftIO $ filterM doesDirectoryExist $ map (combine path) dirContents
      dirListWidget subDirs

      [whamlet|<div><a href=@{TestR}>Back to testing functions</a>|]