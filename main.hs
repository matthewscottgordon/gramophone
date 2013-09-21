{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

import qualified Gramophone.Database as DB
import qualified Gramophone.MediaController as MC
import qualified Gramophone.GUI as GUI
import Gramophone.Gramophone

import Control.Monad (forM_)
import System.Directory (createDirectoryIfMissing,getHomeDirectory)


getDataDirectory :: IO String
getDataDirectory = do
  homeDir <- getHomeDirectory
  let dataDir = homeDir ++ "/.gramophone"
  createDirectoryIfMissing False dataDir
  return dataDir



main :: IO ()
main = do
     MC.initMediaController

     dataDir <- getDataDirectory
     dbRefOrError <- DB.openDatabase (dataDir ++ "/database")
     case dbRefOrError of
       Right dbRef   -> GUI.startGUI dbRef
       Left a -> putStrLn "Could not open Database."