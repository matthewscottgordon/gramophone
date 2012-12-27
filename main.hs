import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Gramophone.Database as DB

data Widgets = Widgets {
      mainWindow :: Window,
      createDatabaseButton :: Button,
      openDatabaseButton :: Button,
      importFilesButton :: Button }

initializeWidgets :: IO Widgets
initializeWidgets = do
  builder <-  builderNew
  builderAddFromFile builder "MainWindow.glade"
     
  wMainWindow <- builderGetObject builder castToWindow "mainWindow"
  onDestroy wMainWindow mainQuit
     
  wStatusLabel <- builderGetObject builder castToLabel "statusLabel"

  wCreateDatabaseButton <- builderGetObject builder castToButton "createDatabaseButton"

  wOpenDatabaseButton <- builderGetObject builder castToButton "openDatabaseButton"
  wImportFilesButton <- builderGetObject builder castToButton "createDatabaseButton"

  return (Widgets wMainWindow wCreateDatabaseButton wOpenDatabaseButton wImportFilesButton)


onCreateDatabaseButtonClicked :: Widgets -> IO ()
onCreateDatabaseButtonClicked widgets = do
  dl <- fileChooserDialogNew (Just "Save Database As...")
                             (Just (mainWindow widgets))
                             FileChooserActionSave
                             [("Save", ResponseOk), ("Cancel", ResponseCancel)]
  r <- dialogRun dl
  widgetHideAll dl
  case r of
    ResponseOk -> do Just filePath <- fileChooserGetFilename dl
                     db <- DB.createNewDatabase filePath
                     setDatabase widgets db
    otherwise  -> return ()
  widgetDestroy dl


onImportFilesButtonClicked :: Widgets -> DB.Connection -> IO ()
onImportFilesButtonClicked widgets db = return ()

setDatabase :: Widgets -> DB.Connection -> IO ()
setDatabase widgets db = do
  onClicked (importFilesButton widgets) (onImportFilesButtonClicked widgets db)
  return ()

main = do
     initGUI
     
     widgets <- initializeWidgets

     onClicked (createDatabaseButton widgets) (onCreateDatabaseButtonClicked widgets) 

     widgetShowAll (mainWindow widgets)
     mainGUI