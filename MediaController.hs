module Gramophone.MediaController
    (
     Tags(..),
     initMediaController,
     readTagsFromFile,
     printTags
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
  let fileUri = "file://" ++ filePath
  pipe <- GS.pipelineNew fileUri

  Just dec <- GS.elementFactoryMake "uridecodebin" Nothing
  GLib.objectSetPropertyString "uri" dec fileUri
  _ <- GS.binAdd (GS.castToBin pipe) dec

  Just sink <- GS.elementFactoryMake "fakesink" Nothing
  _ <- GS.binAdd (GS.castToBin pipe) sink

  _ <- GLib.on dec GS.elementPadAdded $ onNewPadConnectToSink sink

  _ <- GS.elementSetState pipe GS.StatePaused

  bus<- GS.elementGetBus pipe
  eitherTags <- getTags bus

  _ <- GS.elementSetState pipe GS.StateNull

  case eitherTags of
    Right tags   -> return (Just tags)
    Left message -> do
                    putStrLn ("Error: " ++ message)
                    return Nothing


getTags :: GS.Bus -> IO (Either String Tags)
getTags bus = loop (Tags Nothing Nothing Nothing Nothing)
  where
    loop :: Tags -> IO (Either String Tags)
    loop tags = do
        maybeMessage <- GS.busTimedPop bus Nothing
        case maybeMessage of
          Just message -> case (GS.messageType message) of
                            GS.MessageAsyncDone -> return (Right tags)
                            GS.MessageError     -> case GS.messageParseError message of
                                                     Just ( (GLib.GError _ _ errorMessage), errorString) ->
                                                         return (Left ("Error: " ++ errorMessage ++ ": " ++ errorString) )
                                                     Nothing ->
                                                         return (Left "Uknown Error")
                            GS.MessageTag       -> do
                              case (GS.messageParseTag message) of
                                Just tagList -> loop (parseTags tags tagList)
                                Nothing      -> return (Right tags)
                            _                   -> loop tags
          Nothing -> return (Right tags)

    parseTags tags tagList = do
                case (GS.tagListGetString tagList (GS.standardTagToString GS.StandardTagTitle)) of
                  Just title -> tags { tagTrackName = Just title }
                  Nothing    -> tags


printTags :: Tags -> IO ()
printTags (Tags maybeTrackName maybeArtistName maybeTrackNumber maybeNumberOfTracks) = do
  printMaybe "Title:       " maybeTrackName
  printMaybe "Artist:      " maybeArtistName
  printMaybe "Track #:     " maybeTrackNumber
  printMaybe "Track Count: " maybeNumberOfTracks
  where
    printMaybe label (Just value) = putStrLn ( label ++ (show value) )
    printMaybe label Nothing      = return ()
