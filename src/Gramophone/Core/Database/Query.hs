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

{-# LANGUAGE  OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

module Gramophone.Core.Database.Query
       (
         Constraint(..),
         Table(..),
         queryId
       )
       where

import Prelude hiding (mapM)

import Gramophone.Core.Database.Types
import Gramophone.Core.Database.Monad

import Database.HDBC (SqlValue(..), quickQuery', toSql)

import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)
import Data.Traversable (mapM)
import Data.Text (Text(..))


queryDB :: (MonadDB m) => String -> [SqlValue] -> m [[SqlValue]]
queryDB sql values = do
  conn <- getConn
  liftIO $ quickQuery' conn sql values


data Constraint t v = EqualsConstraint (Column t v) v
                    | NoConstraint


class Table t where
  queryTable :: MonadDB m => String -> Constraint t v -> m [[SqlValue]]
  getRow :: MonadDB m => Id t -> m t
  
instance Table Recording where
  queryTable = queryTable' "recordings"
  getRow i = fromRow =<< (head <$> queryTable "file, title, artist, album, track_number" (EqualsConstraint recordingIdColumn i))
    where
      fromRow (file:title:artistId:albumId:trackNum:[]) = do
        artist <- mapM getRow (convert artistId)
        album <- mapM getRow (convert albumId)
        return $ Recording i (convert file) (convert title) artist album (convert trackNum)
      fromRow _ = undefined
  
instance Table Artist where
  queryTable = queryTable' "artists"
  getRow i = (fromRow . head) <$> queryTable "name" (EqualsConstraint artistIdColumn i)
    where
      fromRow (name:[]) = Artist i (convert name)
      from _ = undefined
  
instance Table Album where
  queryTable = queryTable' "albums"
  getRow i = fromRow =<< (head <$> queryTable "title, artist, num_tracks" (EqualsConstraint albumIdColumn i))
    where
      fromRow (title:artistId:numTracks:[]) = do
        artist <- mapM getRow (convert artistId)
        return $ Album i (convert title) artist (convert numTracks)
      fromRow _ = undefined

queryId :: (MonadDB m, Table t) => Constraint t v -> m [Id t]
queryId constraint = map (convert . head) <$> queryTable "id" constraint
    
queryTable' ::  (MonadDB m, Table t) => String -> String -> Constraint t v -> m [[SqlValue]]
queryTable' table columns constraint = q (select columns table) constraint 
  where
    select columns table = "SELECT " ++ columns ++ " FROM " ++ table
    q s (EqualsConstraint column value) = queryDB (s ++ " WHERE " ++ (columnSqlName column) ++ " = ?;") [columnSqlValue column value]
    q s NoConstraint = queryDB (s ++ ";") []

