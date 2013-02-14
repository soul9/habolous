module Habolous.Types.Session (Session(..), nullSession) where

import qualified Control.Monad.Trans as MT (MonadIO, liftIO)
import qualified Habolous.Types.Employee as FTE (Employee, nullEmployee)
import qualified Data.Time.LocalTime as C (LocalTime(..),
                                           zonedTimeToLocalTime,
                                           getZonedTime,
                                           midnight)

data Session = Session {sessionId :: String,
                        sessionEmployee :: FTE.Employee,
                        expires :: C.LocalTime}
                       deriving (Show, Read, Eq)

nullSession :: (MT.MonadIO m) => m Session
nullSession = do
    et <- MT.liftIO C.getZonedTime
    return Session {sessionId = "",
                    sessionEmployee = FTE.nullEmployee,
                    expires = C.LocalTime {C.localDay = C.localDay $
                                            C.zonedTimeToLocalTime et,
                                           C.localTimeOfDay = C.midnight}}
