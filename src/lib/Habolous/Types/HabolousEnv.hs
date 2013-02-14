{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Habolous.Types.HabolousEnv (HabolousEnv(..),
                                 Habolous(..),
                                 runHabolous,
                                 initEnv,
                                 endEnv,
                                 getEnv,
                                 putEnv) where

import qualified Control.Monad.State as CMS (StateT, liftIO,
                                             get, put, runStateT,
                                             Monad, MonadState)
import qualified Control.Monad as M (MonadPlus, Functor)
import qualified Control.Monad.Trans as CMT (MonadIO)
import qualified Control.Applicative as CA (Applicative, Alternative)
import qualified Happstack.Server.Monads as HSM (Happstack, ServerMonad,
                                                 ServerPartT, FilterMonad,
                                                 WebMonad)
import qualified Happstack.Server.Types as HST (Response)
import qualified Happstack.Server.RqData as HSRQ (HasRqData)
import qualified Habolous.Types.Config as FTC (Config(..))
import qualified Habolous.Types.Session as FTS (Session, nullSession)
import qualified Habolous.DbUtil.HabolousEnv as FDBEN (disconnect, clone)
import qualified Database.HDBC.PostgreSQL as PG (Connection)

data HabolousEnv = HabolousEnv
    {databaseConnection :: PG.Connection,
     config :: FTC.Config,
     session :: FTS.Session}

instance Show HabolousEnv where
    show env = (show (config env)) ++ (show (session env))

newtype Habolous a = Habolous (CMS.StateT HabolousEnv (HSM.ServerPartT IO) a)
  deriving (CMS.Monad, CMS.MonadState HabolousEnv,
            M.Functor, CA.Applicative, CA.Alternative,
            CMT.MonadIO, M.MonadPlus, HSM.ServerMonad,
            HSM.FilterMonad HST.Response, HSM.WebMonad HST.Response,
            HSRQ.HasRqData, HSM.Happstack)

runHabolous :: Habolous a -> HabolousEnv -> (HSM.ServerPartT IO) (a, HabolousEnv)
runHabolous (Habolous m) env = CMS.runStateT m env

initEnv :: (Monad m, CMT.MonadIO m) =>
    FTC.Config -> PG.Connection -> m HabolousEnv
initEnv cf conn = do
    cn <- CMS.liftIO $ FDBEN.clone conn
    sess <- FTS.nullSession
    return HabolousEnv {databaseConnection = cn,
                       config = cf,
                       session = sess}

endEnv :: Habolous ()
endEnv = do
    env <- getEnv
    CMS.liftIO $ FDBEN.disconnect $ databaseConnection env


getEnv :: Habolous HabolousEnv
getEnv = CMS.get

putEnv :: HabolousEnv -> Habolous ()
putEnv = CMS.put
