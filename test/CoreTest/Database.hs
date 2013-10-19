module CoreTest.Database
    (
     tests
    ) where

import Gramophone.Core.Database

import Test.HUnit

import System.Directory
import System.FilePath
import System.IO.Error
import Control.Exception


createTempDir :: String -> IO FilePath
createTempDir template = do
  sysTmpDir <- getTemporaryDirectory
  createTempDir' 0 template sysTmpDir
    where
      createTempDir' n base sysTmpDir = do
                           let tmpDir = sysTmpDir </> base <.> (show n)
                           r <- tryIOError (createDirectory tmpDir)
                           case r of
                             Left e -> if (isAlreadyExistsError e)
                                       then createTempDir' (n+1) base sysTmpDir
                                       else ioError e
                             Right () -> return tmpDir

data Fixture a b = Fixture (IO a) (a -> IO b)

withFixture (Fixture setUp tearDown) = TestCase . (bracket setUp tearDown)

emptyTempDirFixture template = Fixture (createTempDir template) (\dirName -> removeDirectoryRecursive dirName)


tests = withFixture (emptyTempDirFixture "gramophone") $ \tmpDir -> do
  dbEither <- createDatabase (tmpDir </> "database")
  case dbEither of
    Left e   -> assertFailure $ show e
    Right db -> return ()


