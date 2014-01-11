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

{-# LANGUAGE  OverloadedStrings #-}

module Gramophone.Core.MediaController.ReadTags
    (
      readTags
    ) where

import Gramophone.Core.MediaController.Types
import Gramophone.Core.MediaController.Tags

import Prelude hiding (mapM)

import Data.Traversable (mapM)
import Control.Applicative ((<$>))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans (lift)
import Control.Monad (guard, void)
import Data.Text

import qualified Media.Streaming.GStreamer as GS

import qualified System.Glib.Properties as G
import qualified System.Glib.Signals as G
import qualified System.Glib as G



onNewPadConnectToSink :: GS.Element -> GS.Pad -> IO ()
onNewPadConnectToSink sink pad = void $ runMaybeT $ do
  sinkpad <- MaybeT $ GS.elementGetStaticPad sink "sink"
  linkedAlready <- lift $ GS.padIsLinked pad
  guard (not linkedAlready)
  padLinkResult <- lift $ GS.padLink pad sinkpad
  lift $ case padLinkResult of
    GS.PadLinkWrongHierarchy ->
      putStrLn "Error: PadLinkWrongHierarchy"
    GS.PadLinkWasLinked      ->
      putStrLn "Error: PadLinkWasLinked"
    GS.PadLinkWrongDirection ->
      putStrLn "Error: PadLinkWrongDirection"
    GS.PadLinkNoformat       ->
      putStrLn "Error: PadLinkNoformat"
    GS.PadLinkNosched        ->
      putStrLn "Error: PadLinkNosched"
    GS.PadLinkRefused        ->
      putStrLn "Error: PadLinkRefused"
    GS.PadLinkOk             -> return ()


readTags :: MediaController -> ReadTagsCommand -> IO ReadTagsResult
readTags _ (ReadTags filePath) = do
  let fileUri = "file://" ++ filePath
  pipe <- GS.pipelineNew fileUri

  maybeBus <- runMaybeT $ do 
    dec <- MaybeT $ GS.elementFactoryMake "uridecodebin" Nothing
    lift $ G.objectSetPropertyString "uri" dec fileUri
    lift $ GS.binAdd (GS.castToBin pipe) dec
    sink <- MaybeT $ GS.elementFactoryMake "fakesink" Nothing
    lift $ GS.binAdd (GS.castToBin pipe) sink
    lift $ G.on dec GS.elementPadAdded $ onNewPadConnectToSink sink
    lift $ GS.elementSetState pipe GS.StatePaused
    lift $ GS.elementGetBus pipe

  eitherTags <- case maybeBus of
    Just bus -> getTags bus
    Nothing  -> return (TagsFail "Error constructing bus.")

  GS.elementSetState pipe GS.StateNull

  return eitherTags


getTags :: GS.Bus -> IO ReadTagsResult
getTags bus = loop emptyTags
  where
    loop :: Tags -> IO ReadTagsResult
    loop tags = do
        maybeMessage <- GS.busTimedPop bus Nothing
        case maybeMessage of
          Just message ->
            case (GS.messageType message) of
              GS.MessageAsyncDone -> return (TagsSuccess tags)
              GS.MessageError ->
                case GS.messageParseError message of
                  Just ( (G.GError _ _ errorMessage), errorString) ->
                    return (TagsFail ("Error: " ++ errorMessage ++ ": "
                                  ++ errorString) )
                  Nothing ->
                    return (TagsFail "Uknown Error")
              GS.MessageTag -> do
                case (GS.messageParseTag message) of
                  Just tagList -> loop (parseTags tagList tags)
                  Nothing      -> return (TagsSuccess tags)
              otherwise -> loop tags
          Nothing -> return (TagsSuccess tags)

    parseTags tagList =
      (checkTag tagTrackName (parseString GS.StandardTagTitle))
      . (checkTag tagAlbumName (parseString GS.StandardTagAlbum))
      . (checkTag tagArtistName (parseString GS.StandardTagArtist))
      . (checkTag tagTrackNumber (parseUInt GS.StandardTagTrackNumber))
      . (checkTag tagNumTracks (parseUInt GS.StandardTagTrackCount))
      . (checkTag tagDiscNumber
         (parseUInt GS.StandardTagAlbumVolumeNumber))
      . (checkTag tagNumDiscs (parseUInt GS.StandardTagVolumeCount))
         where
           checkTag field parseFunc =
             modifyTag field (parseFunc tagList)
           parseString t l = 
             pack <$> GS.tagListGetString l (GS.standardTagToString t)
           parseUInt t l = 
             fromIntegral <$> 
             GS.tagListGetUInt l (GS.standardTagToString t)


