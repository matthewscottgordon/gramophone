import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

main = do
     initGUI
     
     builder <- builderNew
     builderAddFromFile builder "MainWindow.glade"
     
     mainWindow <- builderGetObject builder castToWindow "mainWindow"
     onDestroy mainWindow mainQuit
     
     statusLabel <- builderGetObject builder castToLabel "statusLabel"
     displayMessageButton <- builderGetObject builder castToButton "displayMessageButton"
     onClicked displayMessageButton $ do
         set statusLabel [ labelText := "Button Clicked" ]

     widgetShowAll mainWindow
     mainGUI