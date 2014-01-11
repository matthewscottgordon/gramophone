{-  Copyright 2014 Matthew Gordon.

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

module Gramophone.Core.MediaController.CommandMVar
       (
         CommandMVar,
         initCommand,
         invokeCommand,
         handleCommand',
         handleCommand
       ) where

import Prelude hiding (mapM_)

import Control.Monad (liftM2)
import Control.Applicative ((<*))
import Data.Foldable (mapM_)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar,
                                tryTakeMVar)

data CommandMVar c r = CommandMVar (MVar c) (MVar r)

log' :: String -> (a -> IO b) -> a -> IO b
log' m f a = do
  putStrLn (m ++ " start")
  r <- f a
  putStrLn (m ++ " end")
  return r

initCommand :: IO (CommandMVar c r)
initCommand = liftM2 CommandMVar newEmptyMVar newEmptyMVar

invokeCommand :: CommandMVar c r -> c -> IO r
invokeCommand (CommandMVar c r) a =
  putMVar c a >> putStr "." >> takeMVar r

handleCommand' :: (CommandMVar c r) -> (c -> IO r) -> IO ()
handleCommand' (CommandMVar c r) f = takeMVar c >>= f >>= putMVar r

handleCommand :: (CommandMVar c r) -> (c -> IO r) -> IO ()
handleCommand (CommandMVar c r) f =
  tryTakeMVar c >>= mapM_ ((putMVar r =<<) . f)
