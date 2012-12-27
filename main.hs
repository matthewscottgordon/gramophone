import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Gramophone.Database as DB

main = do
     initGUI
     
     builder <- builderNew
     builderAddFromFile builder "MainWindow.glade"
     
     mainWindow <- builderGetObject builder castToWindow "mainWindow"
     onDestroy mainWindow mainQuit
     
     statusLabel <- builderGetObject builder castToLabel "statusLabel"

     createDatabaseButton <- builderGetObject builder castToButton "createDatabaseButton"
     onClicked createDatabaseButton $ do
         dl <- fileChooserDialogNew (Just "Save Database As...")
                                    (Just mainWindow)
                                    FileChooserActionSave
                                    [("Save", ResponseOk), ("Cancel", ResponseCancel)]
         r <- dialogRun dl
         widgetHideAll dl
         case r of
           ResponseOk -> do maybeFilePath <- fileChooserGetFilename dl
                            case maybeFilePath of
                              Just filePath -> do DB.createNewDatabase filePath
                                                  return ()
                              Nothing       -> return ()
           otherwise  -> return ()
         widgetDestroy dl

     --onClicked displayMessageButton $ do
     --    set statusLabel [ labelText := "Button Clicked" ]

     widgetShowAll mainWindow
     mainGUI