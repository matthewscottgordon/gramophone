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