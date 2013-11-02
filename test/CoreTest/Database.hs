{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies #-}

module CoreTest.Database
    (
     tests
    ) where

import Gramophone.Core.Database

import Test.HUnit
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit

import System.Directory
import System.IO.Temp
import System.FilePath

import Control.Monad
import Control.Monad.Trans

tests = testGroup "Database Tests"  [
         testCase "Create Database" testCreateDB,
         testCase "Add Recordings" testAddRecordings,
         testCase "Add Artists" testAddArtists,
         testCase "Add Albums" testAddAlbums,
         testCase "Add Album with Artist and Recordingss" testAddFullAlbum,
         testCase "Find Artist by name" testFindArtists,
         testCase "Find Album by name" testFindAlbums
        ]


withEmptyDatabase f = withSystemTempDirectory "gramophone" $ \tmpDir -> do
                        dbEither <- createDatabase (tmpDir </> "database")
                        case dbEither of
                          Left e   -> assertFailure $ "Creating Database:" ++ (show e)
                          Right db -> withDatabase db f


testCreateDB = withEmptyDatabase $ return ()


class TestableRecord r n | n -> r where
  addRecord :: MonadDB m => n -> m (Maybe r)
  checkRecord ::  r -> n -> IO ()
  recordName :: n -> String


testAddRecord :: (TestableRecord r n) => (MonadDB m) => n -> m ()
testAddRecord n = do
  maybeRec <- addRecord n
  liftIO $ do
    case maybeRec of
      Nothing -> assertFailure ("Could not create " ++ (recordName n) ++ " record.")
      Just r  -> checkRecord r n
      
testAddRecords :: TestableRecord r n => [n] -> Assertion
testAddRecords ns = withEmptyDatabase $ mapM_ testAddRecord ns

addAndUseRecord :: TestableRecord r n => MonadDB m => n -> (r -> m ()) -> m ()
addAndUseRecord n f = do
  maybeRec <- addRecord n
  do
    case maybeRec of
      Nothing -> liftIO $ assertFailure ("Could not create " ++ (recordName n) ++ " record.")
      Just r  -> (liftIO $ checkRecord r n) >> (f r)


instance TestableRecord Recording NewRecording where
  addRecord = addRecording
  checkRecord r (NewRecording filename' title' _ _ trackNum') = do
    assertEqual "Filename wrong in track record." (recordingFile r) filename'
    assertEqual "Title wrong in track record." (recordingTitle r) title'
    assertEqual "Track number wrong in track record." (recordingTrackNumber r) trackNum'
  recordName _ = "Recording"

testAddRecordings = testAddRecords [new1, new2]
  where
    filename1 = AudioFileName "/home/foo/Music/file1"
    new1 = NewRecording filename1 Nothing Nothing Nothing Nothing
    filename2 = AudioFileName "/home/foo/Music/file2"
    title2 = RecordingTitle "A Song"
    trackNum2 = TrackNumber 5
    new2 = NewRecording filename2 (Just title2) Nothing Nothing (Just trackNum2)



instance TestableRecord Album NewAlbum where
  addRecord = addAlbum
  checkRecord a (NewAlbum title _ trackCount) = do
    assertEqual "Title wrong on Album record." (albumTitle a) title
    assertEqual "TrackCount wrong in Album record." (albumTrackCount a) trackCount
  recordName _ = "Album"

testAddAlbums = testAddRecords [new1, new2]
               where
                 new1 = NewAlbum (AlbumTitle "An Album") Nothing (TrackCount 0)
                 new2 = NewAlbum (AlbumTitle "Another Album") Nothing (TrackCount 16)



instance TestableRecord Artist NewArtist where
  addRecord = addArtist
  checkRecord a (NewArtist name) = do
    assertEqual "Name wrong on Artisrt record." (artistName a) name
  recordName _ = "Artist"

testAddArtists = testAddRecords [new1, new2]
               where
                 new1 = NewArtist (ArtistName "artist")
                 new2 = NewArtist (ArtistName "Johnny Q. Artist")


setUpTestDatabase = do
    addAndUseRecord (NewArtist (ArtistName "Disaster Area")) $ \artist ->
        addAndUseRecord (NewAlbum (AlbumTitle "An Album") (Just (artistId artist)) (TrackCount 5)) $ \album -> do
            testAddRecord (NewRecording (AudioFileName "/Music/Disaster_Area/An_Album/Song.flac")
                                          (Just $ RecordingTitle "Song")
                                          (Just $ artistId artist)
                                          (Just $ albumId album)
                                          (Just $ (TrackNumber 1)))
            testAddRecord (NewRecording (AudioFileName "/Music/Disaster_Area/An_Album/Song_2.flac")
                                          (Just $ RecordingTitle "Song 2")
                                          (Just $ artistId artist)
                                          (Just $ albumId album)
                                          (Just $ (TrackNumber 2)))
            testAddRecord (NewRecording (AudioFileName "/Music/Disaster_Area/An_Album/Song_B.flac")
                                          (Just $ RecordingTitle "Song C")
                                          (Just $ artistId artist)
                                          (Just $ albumId album)
                                          (Just $ (TrackNumber 3)))
            testAddRecord (NewRecording (AudioFileName "/Music/Disaster_Area/An_Album/The_End.flac")
                                          (Just $ RecordingTitle "The End")
                                          (Just $ artistId artist)
                                          (Just $ albumId album)
                                          (Just $ (TrackNumber 4)))
    addAndUseRecord (NewArtist (ArtistName "Artist")) $ \artist ->
        addAndUseRecord (NewAlbum (AlbumTitle "An Album by an Artist") (Just (artistId artist)) (TrackCount 4)) $ \album -> do
            testAddRecord (NewRecording (AudioFileName "/Music/Artist/An_Album/Song.flac")
                                          (Just $ RecordingTitle "Song")
                                          (Just $ artistId artist)
                                          (Just $ albumId album)
                                          (Just $ (TrackNumber 1)))
            testAddRecord (NewRecording (AudioFileName "/Music/Artist/An_Album/Song_2.flac")
                                          (Just $ RecordingTitle "Song 2")
                                          (Just $ artistId artist)
                                          (Just $ albumId album)
                                          (Just $ (TrackNumber 2)))
            testAddRecord (NewRecording (AudioFileName "/Music/Artist/An_Album/Song_B.flac")
                                          (Just $ RecordingTitle "Song C")
                                          (Just $ artistId artist)
                                          (Just $ albumId album)
                                          (Just $ (TrackNumber 3)))

withTestDatabase f = withEmptyDatabase $ setUpTestDatabase >> f

testAddFullAlbum = withTestDatabase $ return ()


testFindArtists = withTestDatabase $ do
    artists <- findArtists (ArtistName "Disaster Area")
    liftIO $ do
      (1 @=? (length artists))
      let artist = head artists
      (ArtistName "Disaster Area") @=? artistName artist
    artists <- findArtists (ArtistName "Kevin")
    liftIO $ assertBool "Found artist where none expected." $ null artists


testFindAlbums = withTestDatabase $ do
    albums <- findAlbums (AlbumTitle "An Album by an Artist")
    liftIO $ do
      1 @=? length albums
      let album = head albums
      (AlbumTitle "An Album by an Artist") @=? albumTitle album
      (TrackCount 4) @=? albumTrackCount album
      case (albumArtist album) of
        Nothing     -> assertFailure "No Artist for Album where expected."
        Just artist -> (ArtistName "Artist") @=? artistName artist


--testFindRecordings = withEmptyData