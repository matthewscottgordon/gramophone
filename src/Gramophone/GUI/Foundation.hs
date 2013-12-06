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

module Gramophone.GUI.Foundation 
       (
         module Gramophone.GUI.Foundation,
         module Gramophone.Core
       ) where

import Yesod

import qualified Gramophone.Core.Database as DB
import Gramophone.Core hiding (withDatabase)

import System.FilePath as FilePath
import qualified Data.Text as T


data RawFilePath = RawFilePath FilePath.FilePath
                   deriving(Show, Eq, Read)


instance PathMultiPiece RawFilePath where
    toPathMultiPiece (RawFilePath p) = map T.pack $ FilePath.splitDirectories p
    fromPathMultiPiece ts = let filePath = FilePath.joinPath $ map T.unpack ts
                            in if FilePath.isValid filePath
                              then Just $ RawFilePath filePath
                              else Nothing

data Website = Website DatabaseRef

withDatabase :: (MonadHandler m, HandlerSite m ~ Website) => DBT m b -> m b
withDatabase f = do
  Website dbr <- getYesod
  DB.withDatabase dbr f

mkYesodData "Website" [parseRoutes|
/                        TestR GET
/FileSystem/*RawFilePath BrowseForFilesR GET
/Recordings              ListRecordingsR GET
/Albums                  ListAlbumsR GET
/Artists                 ListArtistsR GET
|]

instance Yesod Website