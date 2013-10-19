module CoreTest.Database
    (
     tests
    ) where

import Gramophone.Core.Database

import Test.HUnit

import System.Directory
import System.IO.Temp
import System.FilePath

withEmptyDatabase f = withSystemTempDirectory "gramophone" $ \tmpDir -> do
                        dbEither <- createDatabase (tmpDir </> "database")
                        case dbEither of
                          Left e   -> assertFailure $ "Creating Database:" ++ (show e)
                          Right db -> f db

tests = TestCase $ withEmptyDatabase $ \_ -> return ()


