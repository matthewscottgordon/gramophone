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

import qualified Gramophone.GUI as GUI
import Gramophone.Core

import Control.Monad (forM_)
import System.Directory (createDirectoryIfMissing,getHomeDirectory)


getDataDirectory :: IO String
getDataDirectory = do
  homeDir <- getHomeDirectory
  let dataDir = homeDir ++ "/.gramophone"
  createDirectoryIfMissing False dataDir
  return dataDir



main :: IO ()
main = return ()
         --do
     --MC.initMediaController

     --dataDir <- getDataDirectory
     --dbRefOrError <- DB.openDatabase (dataDir ++ "/database")
     --case dbRefOrError of
     --  Right dbRef   -> GUI.startGUI dbRef
     --  Left a -> putStrLn "Could not open Database."