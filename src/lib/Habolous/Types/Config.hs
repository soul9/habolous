module Habolous.Types.Config (Config(..),
                             defaultConfig,
                             parseConfig,
                             parseHttpConfig) where

import qualified Happstack.Server.Types as HST (Conf(..), nullConf)
import qualified Habolous.Types.DatabaseConfig as FTDB
                                  (DatabaseConfig(..),
                                   defaultDatabaseConfig,
                                   parseDatabaseConfig)
import qualified Control.Monad.Error as CME (runErrorT, join)
import qualified Control.Monad.Trans as MT (liftIO)
import qualified Data.ConfigFile as CF (readfile, emptyCP, CPError, get)

data Config = Config
    {httpConfig :: HST.Conf,
     databaseConfig :: FTDB.DatabaseConfig}

instance Show Config where
    show cfg = "Happstack.Server.Conf{show not implemented}"
                ++ show (databaseConfig cfg)

defaultConfig :: Config
defaultConfig = defaultConfig
    {httpConfig = HST.nullConf,
     databaseConfig = FTDB.defaultDatabaseConfig}

parseConfig :: FilePath -> IO (Either CF.CPError Config)
parseConfig f = do
    httpconf <- MT.liftIO $ parseHttpConfig f
    dbconf <- MT.liftIO $ FTDB.parseDatabaseConfig f
    return $ case httpconf of
                  Left err -> Left err
                  Right hc -> case dbconf of
                                     Left e -> Left e
                                     Right dc -> Right Config
                                                  {httpConfig = hc,
                                                   databaseConfig = dc}

parseHttpConfig :: FilePath -> IO (Either CF.CPError HST.Conf)
parseHttpConfig f = CME.runErrorT $ do
    parser <- CME.join $ MT.liftIO $ CF.readfile CF.emptyCP f
    cfport <- CF.get parser "Http" "port"
    return HST.nullConf { HST.port = cfport }
