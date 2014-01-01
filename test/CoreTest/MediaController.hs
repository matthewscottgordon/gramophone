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

{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies #-}

module CoreTest.MediaController
    (
     tests
    ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import System.FilePath

import Control.Lens

import Gramophone.Core.MediaController

import Paths_Gramophone

tests :: Test
tests = testGroup "Core.MediaController Tests" [
  testGroup "Tag Reading Tests" [
     testCase "Read tags from FLAC" (testReadTags flacTestFile), 
     testCase "Read tags from MP3" (testReadTags mp3TestFile),
     testCase "Read tags from AAC" (testReadTags aacTestFile) ]]

data TestFile = TestFile FilePath Tags

flacTestFile :: IO TestFile
flacTestFile = do
  dataDir <- getDataDir 
  return $ TestFile (dataDir </> "test.flac")
    (((set tagTrackName (Just "FLAC Title")) . (set tagArtistName (Just "FLAC Artist"))
     . (set tagAlbumName (Just "FLAC Album")) . (set tagTrackNumber (Just 5))) emptyTags)

mp3TestFile :: IO TestFile
mp3TestFile = do
  dataDir <- getDataDir 
  return $ TestFile (dataDir </> "test.mp3")
    (((set tagTrackName (Just "MP3 Title")) . (set tagArtistName (Just "MP3 Artist")) 
     . (set tagAlbumName (Just "MP3 Album")) . (set tagTrackNumber (Just 3))) emptyTags)

aacTestFile :: IO TestFile
aacTestFile = do
  dataDir <- getDataDir 
  return $ TestFile (dataDir </> "test.m4a")
    (((set tagTrackName (Just "AAC Title")) {-. (set tagArtistName (Just "AAC Artist"))) -}
     . (set tagAlbumName (Just "AAC Album")) . (set tagTrackNumber (Just 16))) emptyTags)

testReadTags :: IO TestFile -> Assertion
testReadTags testFile = do
    mc <- initTagReader
    (TestFile name expectedTags) <- testFile
    maybeFoundTags <- readTagsFromFile mc name
    case maybeFoundTags of
      Nothing -> assertFailure "Could not read file."
      Just foundTags -> do
        assertTagsEqual [("Artist",view tagArtistName),("Track Name",view tagTrackName)] expectedTags foundTags
  where
    assertTagsEqual ((msg, field):rest) expected found = do
      assertTagsEqual' msg field expected found
      assertTagsEqual rest expected found
    assertTagsEqual [] _ _ = return ()
    assertTagsEqual' msg field expected found = assertEqual msg' (v expected) (v found)
      where v = field
            msg' = msg ++ " tag doesn't have expected value."