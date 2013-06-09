{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

import qualified Gramophone.Database as DB
import qualified Gramophone.MediaController as MC

import Control.Monad (forM_,filterM)
import System.Directory (createDirectoryIfMissing,getHomeDirectory)
import Yesod

import qualified Data.Text as T
import qualified System.FilePath as FilePath
import System.Directory(getDirectoryContents,doesDirectoryExist)
import System.IO.Error(isDoesNotExistError,isPermissionError)
import Control.Exception(try)


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

getBrowseForFilesR :: RawFilePath -> Handler RepHtml
getBrowseForFilesR (RawFilePath path) = defaultLayout $ do
  dirContentsOrError <- liftIO $ try $ getDirectoryContents path
  case dirContentsOrError of
    Left e | isDoesNotExistError e -> notFound
           | isPermissionError e   -> permissionDenied $ T.concat ["You are not allowed access to \"", (T.pack path), "\""]
           | otherwise             -> notFound
    Right dirContents -> do
      subDirs <- liftIO $ filterM doesDirectoryExist $ map (FilePath.combine path) dirContents
      setTitle "Gramophone - Browse Filesystem"
      [whamlet|
                <body>
                  <h1>#{path}
                  $forall dir <- subDirs
                    <li><a href=@{BrowseForFilesR (RawFilePath dir)}>#{FilePath.takeFileName dir}
                  <a href=@{TestR}>Back to testing functions|]

main :: IO ()
main = do
     MC.initMediaController

     dataDir <- getDataDirectory
     dbRefOrError <- DB.getDatabaseRef (dataDir ++ "/database")
     case dbRefOrError of
       Right dbRef   -> warp 3000 $ Website dbRef
       Left errorMsg -> putStrLn ("Could not open Database: " ++ errorMsg)