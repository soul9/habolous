module Habolous.Routes (serve) where

import qualified Happstack.Server.SimpleHTTP as HSS (simpleHTTP)
import qualified Happstack.Server.Types as HST (Response)
import qualified Habolous.Types.Config as FTC (Config(..))
import qualified Habolous.Types.HabolousEnv as FTE (initEnv, endEnv,
                                                  Habolous, runHabolous)
import qualified Habolous.DbUtil.HabolousEnv as FDBEN (connect)
import qualified Habolous.Routes.Version as FRV (routes)
import qualified Habolous.Routes.DatabaseVersion as FRDBV (routes)
import qualified Habolous.Routes.Login as FRL (withAuth)

serve :: FTC.Config -> IO ()
serve c = do
    conn <- FDBEN.connect $ FTC.databaseConfig c
    HSS.simpleHTTP (FTC.httpConfig c) $ do
        env <- FTE.initEnv c conn
        (res, nenv) <- FTE.runHabolous serveReq env
        _ <- FTE.runHabolous FTE.endEnv nenv
        return res

serveReq :: FTE.Habolous HST.Response
serveReq = FRL.withAuth [FRV.routes,
                         FRDBV.routes]
