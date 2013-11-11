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

{-# LANGUAGE GADTs #-}

module Gramophone.Core.Database.Monad
    (
     MonadDB(..),
     DBT(..),
     runDBT
    ) where

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.Functor
import Control.Applicative

import qualified Database.HDBC.Sqlite3 as Sqlite


class (Monad m, MonadIO m, Functor m) => MonadDB m where
    getConn :: m Sqlite.Connection


data DBT m a where
    DBT :: (MonadIO m) => (Sqlite.Connection -> m a) -> DBT m a

runDBT :: MonadIO m => DBT m a -> Sqlite.Connection -> m a
runDBT (DBT f) = f

mapDBT :: (MonadIO m, MonadIO n) => (m a -> n b) -> DBT m a -> DBT n b
mapDBT f m = DBT $ f . runDBT m

liftDBT :: MonadIO m => m a -> DBT m a
liftDBT m = DBT (const m)


instance (MonadIO m, Functor m) => Functor (DBT m) where
    fmap f = mapDBT (fmap f)

instance (MonadIO m, Applicative m) => Applicative (DBT m) where
    pure    = liftDBT . pure
    f <*> v = DBT $ \db -> runDBT f db <*> runDBT v db

instance (Monad m, MonadIO m) => Monad (DBT m) where
    return a = DBT $ \_ -> return a
    m >>= k  = DBT $ \db -> do
        a <- runDBT m db
        runDBT (k a) db
    fail msg = DBT $ \_ -> fail msg

instance MonadIO m => MonadIO (DBT m) where
    liftIO = liftDBT . liftIO

instance (Monad m, MonadIO m, Functor m) => MonadDB (DBT m) where
    getConn = DBT return

instance (MonadDB m) => MonadDB (StateT s m) where
    getConn = lift getConn