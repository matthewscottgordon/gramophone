module Gramophone.MediaController
    (
     Tags(..),
     initMediaController,
     readTagsFromFile
    ) where

import qualified Media.Streaming.GStreamer as GS
import qualified System.Glib.GError as GLib
import qualified System.Glib.Properties as GLib
import qualified System.Glib.Signals as GLib

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
        padLinkResult <- GS.padLink pad sinkpad
        case padLinkResult of
          GS.PadLinkWrongHierarchy -> putStrLn "Error: PadLinkWrongHierarchy"
          GS.PadLinkWasLinked      -> putStrLn "Error: PadLinkWasLinked"
          GS.PadLinkWrongDirection -> putStrLn "Error: PadLinkWrongDirection"
          GS.PadLinkNoformat       -> putStrLn "Error: PadLinkNoformat"
          GS.PadLinkNosched        -> putStrLn "Error: PadLinkNosched"
          GS.PadLinkRefused        -> putStrLn "Error: PadLinkRefused"
          GS.PadLinkOk             -> return ()
        return ()
      else return ()
    Nothing -> return ()


readTagsFromFile :: FilePath -> IO (Maybe Tags)
readTagsFromFile filePath = do
  pipe <- GS.pipelineNew filePath

  Just dec <- GS.elementFactoryMake "uridecodebin" Nothing
  GLib.objectSetPropertyString "uri" dec filePath
  _ <- GS.binAdd (GS.castToBin pipe) dec

  Just sink <- GS.elementFactoryMake "fakesink" Nothing
  _ <- GS.binAdd (GS.castToBin pipe) sink

  _ <- GLib.on dec GS.elementPadAdded $ onNewPadConnectToSink sink

  _ <- GS.elementSetState pipe GS.StatePaused

  whileLoop pipe

  _ <- GS.elementSetState pipe GS.StateNull

  return Nothing
  
  where
    whileLoop pipe =
        loop 
        where
          loop = do
            bus <- GS.elementGetBus pipe
            maybeMessage <- GS.busTimedPop bus Nothing
            case maybeMessage of
              Just message -> case (GS.messageType message) of
                                GS.MessageAsyncDone -> return ()
                                GS.MessageError     -> case GS.messageParseError message of
                                                         Just ( (GLib.GError _ _ errorMessage), errorString) ->
                                                                putStrLn ("Error: " ++ errorMessage ++ ": " ++ errorString)
                                                         Nothing ->
                                                                putStrLn "Uknown Error"
                                GS.MessageTag       -> do
                                  case (GS.messageParseTag message) of
                                    Just tagList -> printTags tagList
                                    Nothing      -> return ()
                                  loop
                                _                   -> loop
              Nothing -> return ()

          printTags tagList = do
                              case (GS.tagListGetString tagList (GS.standardTagToString GS.StandardTagTitle)) of
                                Just title -> putStrLn title
                                Nothing    -> return ()
      