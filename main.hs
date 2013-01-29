import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import qualified Gramophone.Database as DB
import qualified Gramophone.MediaController as MC

import Control.Monad (forM_)

data GUI = GUI {
      mainWindow :: Window,
      createDatabaseButton :: Button,
      openDatabaseButton :: Button,
      importFilesButton :: Button }

initializeWidgets :: IO GUI
initializeWidgets = do
  builder <-  builderNew
  builderAddFromFile builder "MainWindow.glade"
     
  guiMainWindow <- builderGetObject builder castToWindow "mainWindow"
  onDestroy guiMainWindow mainQuit
     
  guiStatusLabel <- builderGetObject builder castToLabel "statusLabel"

  guiCreateDatabaseButton <- builderGetObject builder castToButton "createDatabaseButton"

  guiOpenDatabaseButton <- builderGetObject builder castToButton "openDatabaseButton"
  guiImportFilesButton <- builderGetObject builder castToButton "importFilesButton"

  return (GUI guiMainWindow guiCreateDatabaseButton guiOpenDatabaseButton guiImportFilesButton)


onCreateDatabaseButtonClicked :: GUI -> IO ()
onCreateDatabaseButtonClicked gui = do
  dl <- fileChooserDialogNew (Just "Save Database As...")
                             (Just (mainWindow gui))
                             FileChooserActionSave
                             [("Save", ResponseOk), ("Cancel", ResponseCancel)]
  r <- dialogRun dl
  widgetHideAll dl
  case r of
    ResponseOk -> do Just filePath <- fileChooserGetFilename dl
                     db <- DB.createNewDatabase filePath
                     setDatabase gui db
    otherwise  -> return ()
  widgetDestroy dl


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
          Just tags -> do
            putStrLn ( filename ++ ":" )
            MC.printTags tags
            putStrLn ""
          Nothing -> putStrLn ( "No tags for \"" ++ filename ++ "\"" )

setDatabase :: GUI -> DB.Connection -> IO ()
setDatabase gui db = do
  onClicked (importFilesButton gui) (onImportFilesButtonClicked gui db) 
  return ()

main = do
     initGUI
     
     gui <- initializeWidgets

     onClicked (createDatabaseButton gui) (onCreateDatabaseButtonClicked gui) 

     widgetShowAll (mainWindow gui)
     mainGUI