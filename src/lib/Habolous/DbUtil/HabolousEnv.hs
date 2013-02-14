module Habolous.DbUtil.HabolousEnv (connect,disconnect,clone) where

import qualified Habolous.Types.DatabaseConfig as FDB (DatabaseConfig(..),
                                                      connectionString)
import qualified Database.HDBC as DB (disconnect, clone)
import qualified Database.HDBC.PostgreSQL as PG (connectPostgreSQL, Connection)

connect :: FDB.DatabaseConfig -> IO PG.Connection
connect cf = PG.connectPostgreSQL $
              FDB.connectionString cf

disconnect :: PG.Connection -> IO ()
disconnect conn = DB.disconnect conn

clone :: PG.Connection -> IO PG.Connection
clone = DB.clone
