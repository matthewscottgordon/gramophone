{-  Copyright 2013 Matthew Gordon.

    This file is part of Gramophone.

    Gramophone is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Gramophone is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Gramophone.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE  OverloadedStrings, TemplateHaskell #-}

module Gramophone.Core.MediaController
    (
     Tags(),
     tagTrackName,
     tagAlbumName,
     tagArtistName,
     tagTrackNumber,
     tagNumTracks,
     tagDiscNumber,
     tagNumDiscs,
     emptyTags,

     MediaController,
     initMediaController,
     readTagsFromFile,
    ) where

import Control.Applicative ((<$>),(<|>))
import Control.Lens
import Data.Text

import qualified Media.Streaming.GStreamer as GS
import qualified System.Glib.GError as GLib
import qualified System.Glib.Properties as GLib
import qualified System.Glib.Signals as GLib

data Tags = Tags {
      _tagTrackName :: Maybe Text,
      _tagAlbumName :: Maybe Text,
      _tagArtistName :: Maybe Text,
      _tagTrackNumber :: Maybe Integer,
      _tagNumTracks :: Maybe Integer,
      _tagDiscNumber :: Maybe Integer,
      _tagNumDiscs :: Maybe Integer
}

$(makeLenses ''Tags)

modifyTag l v = l `over` (<|> v)

emptyTags :: Tags
emptyTags = Tags Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Show Tags where
    show (Tags maybeTrackName maybeAlbumName maybeArtistName maybeTrackNumber maybeNumTracks maybeDiscNumber maybeNumDiscs) =
                     (maybeLine "Title:       " maybeTrackName) ++ 
                     (maybeLine "Artist:      " maybeArtistName) ++
                     (maybeLine "Album:       " maybeAlbumName) ++
                     (maybeLine "Track #:     " maybeTrackNumber) ++
                     (maybeLine "Track Count: " maybeNumTracks) ++
                     (maybeLine "Disc #:      " maybeDiscNumber) ++
                     (maybeLine "Disc Count:  " maybeNumDiscs)
                   where
                     maybeLine label (Just value) = label ++ (show value) ++ "\n"
                     maybeLine _     Nothing      = ""

-- | Reference needed to call most functions in this module.
--   Currently, this is just used to ensure the GStreamer library has been initialized.
data MediaController = MediaController

initMediaController :: IO MediaController
initMediaController = do
  GS.init
  return MediaController

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
        else return ()
    Nothing -> return ()


readTagsFromFile :: MediaController -> FilePath -> IO (Maybe Tags)
readTagsFromFile _ filePath = do
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
getTags bus = loop emptyTags
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
                                Just tagList -> loop (parseTags tagList tags)
                                Nothing      -> return (Right tags)
                            _                   -> loop tags
          Nothing -> return (Right tags)

    parseTags tagList = (checkTag tagTrackName (parseString GS.StandardTagTitle))
             . (checkTag tagAlbumName (parseString GS.StandardTagAlbum))
             . (checkTag tagArtistName (parseString GS.StandardTagArtist))
             . (checkTag tagTrackNumber (parseUInt GS.StandardTagTrackNumber))
             . (checkTag tagNumTracks (parseUInt GS.StandardTagTrackCount))
             . (checkTag tagDiscNumber (parseUInt GS.StandardTagAlbumVolumeNumber))
             . (checkTag tagNumDiscs (parseUInt GS.StandardTagVolumeCount))
         where
           checkTag field parseFunc = modifyTag field (parseFunc tagList)
           parseString t l = pack <$> GS.tagListGetString l (GS.standardTagToString t)
           parseUInt t l = fromIntegral <$> GS.tagListGetUInt l (GS.standardTagToString t)


