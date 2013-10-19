{-# LANGUAGE OverloadedStrings #-}

module CoreTest.Database
    (
     tests
    ) where

import Gramophone.Core.Database

import Test.HUnit

import System.Directory
import System.IO.Temp
import System.FilePath

import Control.Monad.Trans

withEmptyDatabase f = withSystemTempDirectory "gramophone" $ \tmpDir -> do
                        dbEither <- createDatabase (tmpDir </> "database")
                        case dbEither of
                          Left e   -> assertFailure $ "Creating Database:" ++ (show e)
                          Right db -> f db


testCreateDB = TestCase $ withEmptyDatabase $ \_ -> return ()

checkRecordingRecord (Recording _ filename title artist album trackNum) filename' title' _ _ trackNum' = do
  assertEqual "Filename wrong in track record." filename filename'
  assertEqual "Title wrong in track record." title title'
  assertEqual "Track number wrong in track record." trackNum trackNum'

testAddRecording = TestCase $ withEmptyDatabase $ \db -> withDatabase db $ do
                     maybeRec1 <- addRecording new1
                     maybeRec2 <- addRecording new2
                     liftIO $ do
                       case maybeRec1 of
                         Nothing -> assertFailure "Could not create recording record."
                         Just r  -> checkRecordingRecord r filename1 Nothing Nothing Nothing Nothing
                       case maybeRec2 of
                         Nothing -> assertFailure "Could not create recording record."
                         Just r  -> checkRecordingRecord r filename2 (Just title2) Nothing Nothing (Just trackNum2)
                   where
                     filename1 = AudioFileName "/home/foo/Music/file1"
                     new1 = NewRecording filename1 Nothing Nothing Nothing Nothing
                     filename2 = AudioFileName "/home/foo/Music/file2"
                     title2 = RecordingTitle "A Song"
                     trackNum2 = TrackNumber 5
                     new2 = NewRecording filename2 (Just title2) Nothing Nothing (Just trackNum2)

tests = TestList [testCreateDB, testAddRecording]


