import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import qualified Gramophone.Database as DB
import qualified Gramophone.MediaController as MC

import Control.Monad (forM_)

import System.Directory (createDirectoryIfMissing,getHomeDirectory)

data GUI = GUI {
      mainWindow :: Window,
      importFilesButton :: Button }

initializeWidgets :: IO GUI
initializeWidgets = do
  builder <-  builderNew
  builderAddFromFile builder "MainWindow.glade"
     
  guiMainWindow <- builderGetObject builder castToWindow "mainWindow"
  onDestroy guiMainWindow mainQuit
     
  guiStatusLabel <- builderGetObject builder castToLabel "statusLabel"

  guiImportFilesButton <- builderGetObject builder castToButton "importFilesButton"

  return (GUI guiMainWindow guiImportFilesButton)


onImportFilesButtonClicked :: GUI -> DB.Connection -> IO ()
onImportFilesButtonClicked gui db = do
  dl <- fileChooserDialogNew (Just "Files to import...")
                             (Just (mainWindow gui))
                             FileChooserActionOpen
                             [("Import", ResponseOk), ("Cancel", ResponseCancel)]
  fileChooserSetSelectMultiple dl True
  r <- dialogRun dl
  widgetHideAll dl
  case r of
    ResponseOk -> do filePaths <- fileChooserGetFilenames dl
                     printTags filePaths
    otherwise  -> return ()
  widgetDestroy dl

printTags :: [String] -> IO ()
printTags filenames =
    forM_ filenames $ \filename -> do
        maybeTags <- MC.readTagsFromFile filename
        case maybeTags of
          Just tags -> putStrLn ( filename ++ ":\n" ++ (show tags))
          Nothing -> putStrLn ( "No tags for \"" ++ filename ++ "\"" )

setDatabase :: GUI -> DB.Connection -> IO ()
setDatabase gui db = do
  onClicked (importFilesButton gui) (onImportFilesButtonClicked gui db) 
  return ()

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

main = do
     initGUI
     MC.initMediaController
     
     gui <- initializeWidgets
     db <- openDatabase
     setDatabase gui db

     widgetShowAll (mainWindow gui)
     mainGUI