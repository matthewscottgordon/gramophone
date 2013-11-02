import Test.Framework (defaultMain)
import Test.HUnit

import qualified CoreTest.Database

tests = [CoreTest.Database.tests]

main = defaultMain tests
