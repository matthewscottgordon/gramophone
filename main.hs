import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

main = do
     initGUI
     
     builder <- builderNew
     builderAddFromFile builder "MainWindow.glade"
     mainWindow <- builderGetObject builder castToWindow "mainWindow"
     onDestroy mainWindow mainQuit
     widgetShowAll mainWindow
     mainGUI