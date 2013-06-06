{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

import qualified Gramophone.Database as DB
import qualified Gramophone.MediaController as MC

import Control.Monad (forM_)
import System.Directory (createDirectoryIfMissing,getHomeDirectory)
import Yesod


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

openDatabase :: IO DB.Connection
openDatabase = do
  dataDir <- getDataDirectory
  DB.openOrCreateDatabase ( dataDir ++ "/database" )



data Website = Website DB.DatabaseRef

mkYesod "Website" [parseRoutes|
/ TestR GET
|]

instance Yesod Website

getTestR :: Handler RepHtml
getTestR = defaultLayout $ do
    setTitle "Gramophone - Testing Functions"
    [whamlet|
              <body>
                  <h1>Testing Functions|]



main :: IO ()
main = do
     MC.initMediaController

     dataDir <- getDataDirectory
     dbRefOrError <- DB.getDatabaseRef (dataDir ++ "/database")
     case dbRefOrError of
       Right dbRef   -> warp 3000 $ Website dbRef
       Left errorMsg -> putStrLn ("Could not open Database: " ++ errorMsg)