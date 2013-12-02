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

import Control.Applicative((<$>))
import System.Directory (getHomeDirectory)
import Yesod

import Gramophone.GUI.Foundation
import Gramophone.GUI.BrowseForFiles
import Gramophone.GUI.ListRecordings


mkYesodDispatch "Website" resourcesWebsite

getTestR :: Handler Html
getTestR = defaultLayout $ do
    setTitle "Gramophone - Testing Functions"
    browseFilesStartDir <- liftIO $ RawFilePath <$> getHomeDirectory
    [whamlet|
              <body>
                  <h1>Testing Functions
                  <a href=@{BrowseForFilesR browseFilesStartDir}>Browse for Files
                  <a href=@{ListRecordingsR}>List Recordings|]


startGUI :: DB.DatabaseRef -> IO ()
startGUI dbRef = warp 3000 $ Website dbRef