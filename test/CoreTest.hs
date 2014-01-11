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

import Test.Framework (defaultMain, Test)

import qualified CoreTest.Database
import qualified CoreTest.MediaController

import Gramophone.Core.MediaController (initMediaController, shutDown)

--tests :: [Test]
--tests = [CoreTest.Database.tests, CoreTest.MediaController.tests]

main :: IO ()
main = do
  mc <- initMediaController
  defaultMain [CoreTest.Database.tests, CoreTest.MediaController.tests mc]
  shutDown mc
