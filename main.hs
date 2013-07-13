{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

import qualified Gramophone.Database as DB
import qualified Gramophone.MediaController as MC
import qualified Gramophone.GUI as GUI

import Control.Monad (forM_)
import System.Directory (createDirectoryIfMissing,getHomeDirectory)


printTags :: [String] -> IO ()
printTags filenames =
    forM_ filenames $ \filename -> do
        maybeTags <- MC.readTagsFromFile filename
        case maybeTags of
          Just tags -> putStrLn ( filename ++ ":\n" ++ (show tags))
          Nothing -> putStrLn ( "No tags for \"" ++ filename ++ "\"" )

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
     dbRefOrError <- DB.getDatabaseRef (dataDir ++ "/database")
     case dbRefOrError of
       Right dbRef   -> GUI.startGUI dbRef
       Left errorMsg -> putStrLn ("Could not open Database: " ++ errorMsg)