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

import System.Directory
import System.FilePath
import System.Environment


getDataDirectory :: IO FilePath
getDataDirectory = do
  homeDir <- getHomeDirectory
  let dataDir = homeDir </> ".gramophone"
  createDirectoryIfMissing False dataDir
  return dataDir


printHelp :: IO ()
printHelp = putStrLn "Usage: gramophone [dataDirectory]"

main :: IO ()
main = do
    args <- getArgs
    case args of
      (opt:dir:[]) | opt == "--create" -> runWith initFiles dir
      (arg:[])     | arg == "--create" -> getDataDirectory >>= runWith initFiles
                   | otherwise         -> runWith openFiles arg
      []                               -> getDataDirectory >>= runWith openFiles
      otherwise                        -> printHelp
  where
    runWith f dir = do
      r <- f dir
      case r of
        Left e   -> putStrLn (show e)
        Right db -> putStrLn ("Running Web GUI with \"" ++ dir ++ "\"") >> GUI.startGUI db
