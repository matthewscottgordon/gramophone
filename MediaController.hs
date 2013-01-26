module Gramophone.MediaController
    (
     MediaController(),
     Tags(..),
     initMediaController,
     readTagsFromFile
    ) where

import qualified Media.Streaming.GStreamer as GS
import qualified System.Glib.Properties as GLib
import qualified System.Glib.Signals as GLib

data MediaController = MediaController

data Tags = Tags {
      tagTrackName :: Maybe String,
      tagArtistName :: Maybe String,
      tagTrackNumber :: Maybe Integer,
      tagNumTracks :: Maybe Integer }

initMediaController :: IO ()
initMediaController = do
  GS.init
  return ()

onNewPadConnectToSink :: GS.Element -> GS.Pad -> IO ()
onNewPadConnectToSink sink pad = do
  maybeSinkpad <- GS.elementGetStaticPad sink "sink"
  case maybeSinkpad of
    Just sinkpad -> do
      q <- GS.padIsLinked pad
      if not q 
      then do
        GS.padLink pad sinkpad
        return ()
      else return ()
    Nothing -> return ()

printTags tagList = do
  case (GS.tagListGetString tagList (GS.standardTagToString GS.StandardTagTitle)) of
    Just title -> putStrLn title
    Nothing    -> return ()

whileLoop pipe =
    loop 
    where
      loop = do
        bus <- GS.elementGetBus pipe
        maybeMessage <- GS.busTimedPop bus Nothing
        case maybeMessage of
          Just message -> case (GS.messageType message) of
                            GS.MessageAsyncDone -> putStrLn "Done"
                            GS.MessageError     -> putStrLn "Error"
                            GS.MessageTag       -> do
                              case (GS.messageParseTag message) of
                                Just tagList -> printTags tagList
                                Nothing      -> return ()
                              loop
                            otherwise -> loop
          Nothing -> return ()
  

readTagsFromFile :: FilePath -> IO (Maybe Tags)
readTagsFromFile filePath = do
  pipe <- GS.pipelineNew filePath

  Just dec <- GS.elementFactoryMake "uridecodebin" Nothing
  GLib.objectSetPropertyString "uri" dec filePath
  GS.binAdd (GS.castToBin pipe) dec

  Just sink <- GS.elementFactoryMake "fakesink" Nothing
  GS.binAdd (GS.castToBin pipe) sink

  GLib.on dec GS.elementPadAdded $ onNewPadConnectToSink sink

  GS.elementSetState pipe GS.StatePaused

  whileLoop pipe

  GS.elementSetState pipe GS.StateNull

  return Nothing
  
      