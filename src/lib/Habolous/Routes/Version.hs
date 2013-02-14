module Habolous.Routes.Version (routes) where

import qualified Habolous.Templates.Version as FTV (html)
import qualified Paths_habolous as PF  (version)
import qualified Data.Version as DV  (showVersion)
import qualified Happstack.Server.Routing as HSRO (dir, nullDir)
import qualified Happstack.Server.Types as HST (Response)
import qualified Happstack.Server.Response as HSRE (ok, toResponse)
import qualified Happstack.Server.Monads as HSM (Happstack)

routes :: (HSM.Happstack m) => m HST.Response
routes =
    HSRO.dir "version" $ HSRO.nullDir >> do
        HSRE.ok $ HSRE.toResponse $
          FTV.html $ DV.showVersion PF.version
