module CoreTest.Database
    (
     tests
    ) where

import Gramophone.Core.Database

import Test.HUnit

import System.Directory
import System.IO.Temp
import System.FilePath

tests = TestCase $ withSystemTempDirectory "gramophone" $ \tmpDir -> do
  dbEither <- createDatabase (tmpDir </> "database")
  case dbEither of
    Left e   -> assertFailure $ show e
    Right db -> return ()


