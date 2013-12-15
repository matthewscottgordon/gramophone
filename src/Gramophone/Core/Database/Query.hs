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

{-# LANGUAGE  OverloadedStrings, FlexibleContexts #-}

module Gramophone.Core.Database.Query
       (
         Constraint(..),
         queryTable
       )
       where

import Gramophone.Core.Database.Types
import Gramophone.Core.Database.Monad

import Database.HDBC (SqlValue(..), quickQuery', toSql)

import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)
import Data.Text (Text(..))


queryDB :: (MonadDB m) => String -> [SqlValue] -> m [[SqlValue]]
queryDB sql values = do
  conn <- getConn
  liftIO $ quickQuery' conn sql values


data Constraint t v = EqualsConstraint (Column t v) v
                    | NoConstraint


class Table t where
  queryTable :: MonadDB m => Constraint t v -> m [Id t]
  
instance Table Recording where
  queryTable = queryTable' "recordings"
  
instance Table Artist where
  queryTable = queryTable' "artists"
  
instance Table Album where
  queryTable = queryTable' "albums"

queryTable' :: MonadDB m => String -> Constraint t v -> m [Id t]
queryTable' table constraint = map (convert . head) <$> doQuery constraint
  where
    doQuery (EqualsConstraint column value)
      = queryDB ("SELECT id FROM " ++ table ++ " WHERE " ++ (columnSqlName column) ++ " = ?;") [columnSqlValue column value]
    doQuery NoConstraint = queryDB ("xSELECT id FROM " ++ table ++ ";") []
  
  
