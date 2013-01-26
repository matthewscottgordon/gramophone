import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Gramophone.Database as DB
import Gramophone.MediaController as MC

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
  guiImportFilesButton <- builderGetObject builder castToButton "createDatabaseButton"

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
onImportFilesButtonClicked gui db = return ()

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