{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

import qualified Gramophone.Database as DB
import qualified Gramophone.MediaController as MC

import Control.Monad (forM_,filterM,liftM)
import System.Directory (createDirectoryIfMissing,getHomeDirectory)
import Yesod
import Text.Julius(rawJS)

import qualified Data.Text as T
import qualified System.FilePath as FilePath
import System.Directory(getDirectoryContents,doesDirectoryExist,doesFileExist)
import System.IO.Error(isDoesNotExistError,isPermissionError)
import Control.Exception(try)
import Data.List(sort)


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

getTestR :: Handler RepHtml
getTestR = defaultLayout $ do
    setTitle "Gramophone - Testing Functions"
    [whamlet|
              <body>
                  <h1>Testing Functions
                  <a href=@{BrowseForFilesR (RawFilePath "/home/gordon")}>Browse for Files|]

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

getBrowseForFilesR :: RawFilePath -> Handler RepHtml
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

main :: IO ()
main = do
     MC.initMediaController

     dataDir <- getDataDirectory
     dbRefOrError <- DB.getDatabaseRef (dataDir ++ "/database")
     case dbRefOrError of
       Right dbRef   -> warp 3000 $ Website dbRef
       Left errorMsg -> putStrLn ("Could not open Database: " ++ errorMsg)