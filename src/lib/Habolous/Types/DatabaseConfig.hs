module Habolous.Types.DatabaseConfig (
                    DatabaseConfig(..), parseDatabaseConfig,
                    defaultDatabaseConfig, connectionString
                   ) where

import qualified Control.Monad.Error as CME (runErrorT, join)
import qualified Control.Monad.Trans as MT (liftIO)
import qualified Data.Text as T (Text, unpack, pack)
import qualified Data.ConfigFile as CF (readfile, emptyCP, CPError, get)

data DatabaseConfig = DatabaseConfig
    { host             :: T.Text,   -- db server host name (or IP)
      port             :: Integer,  -- db server port
      database         :: T.Text,   -- db name
      user             :: T.Text,   -- db login
      password         :: T.Text,   -- db login password
      poolSize         :: Integer   -- number of workers in worker pool
    } deriving (Show, Read)

defaultDatabaseConfig :: DatabaseConfig
defaultDatabaseConfig = DatabaseConfig {host = T.pack "localhost",
                                        port = 5432,
                                        database = T.pack "habolous",
                                        user = T.pack "habolous",
                                        password = T.pack "",
                                        poolSize = 1}

-- read database configuration from file
parseDatabaseConfig :: FilePath -> IO (Either CF.CPError DatabaseConfig)
parseDatabaseConfig f = CME.runErrorT $ do
    parser     <- CME.join $ MT.liftIO $ CF.readfile CF.emptyCP f
    cfhost     <- CF.get parser "Database" "host"
    cfport     <- CF.get parser "Database" "port"
    cfdatabase <- CF.get parser "Database" "database"
    cfuser     <- CF.get parser "Database" "user"
    cfpassword <- CF.get parser "Database" "password"
    cfpoolsize <- CF.get parser "Database" "poolsize"
    return DatabaseConfig {host = T.pack cfhost,
                           port = cfport,
                           database = T.pack cfdatabase,
                           user = T.pack cfuser,
                           password = T.pack cfpassword,
                           poolSize = cfpoolsize}

connectionString :: DatabaseConfig -> String
connectionString cf = "host=" ++ T.unpack (host cf) ++
                    " port=" ++ show (port cf) ++
                    " dbname=" ++ T.unpack (database cf) ++
                    " user=" ++ T.unpack (user cf) ++
                    " password=" ++ T.unpack (password cf)
