module Habolous.Routes.DatabaseVersion (
                          routes
                         ) where

import qualified Habolous.Templates.DatabaseVersion as FTDBV (html)
import qualified Habolous.Types.HabolousEnv as FTE
                        (Habolous, HabolousEnv(..), getEnv)
import qualified Happstack.Server.Routing as HSRO (dir, nullDir)
import qualified Happstack.Server.Types as HST (Response)
import qualified Happstack.Server.Response as HSRE (ok, toResponse)
import qualified Database.HDBC as DB (dbServerVer, hdbcClientVer)

routes :: FTE.Habolous HST.Response
routes =
    HSRO.dir "databaseversion" $ HSRO.nullDir >> do
        srver <- getDatabaseServerVersion
        clver <- getDatabaseClientVersion
        HSRE.ok $ HSRE.toResponse $
         FTDBV.html srver clver

getDatabaseServerVersion :: FTE.Habolous String
getDatabaseServerVersion = do
    env <- FTE.getEnv
    return $ DB.dbServerVer (FTE.databaseConnection env)

getDatabaseClientVersion :: FTE.Habolous String
getDatabaseClientVersion = do
    env <- FTE.getEnv
    return $ DB.hdbcClientVer (FTE.databaseConnection env)
