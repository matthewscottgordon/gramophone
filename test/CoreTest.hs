import Test.HUnit

import qualified CoreTest.Database

tests = TestList [CoreTest.Database.tests]

main = runTestTT tests
