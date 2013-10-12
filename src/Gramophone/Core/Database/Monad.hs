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