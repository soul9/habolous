module Habolous.DbUtil.Session (createSession,
                               getSession) where

import qualified Habolous.Types.Session as FTS (Session(..))
import qualified Habolous.Types.HabolousEnv as FTEN (Habolous)
import qualified Habolous.Types.Employee as FTE (Employee(..), nullEmployee)
import qualified Habolous.DbUtil.Employee as FDBE (getEmployeeById)
import qualified Habolous.DbUtil.Helpers as FDBH (simpleSqlRun,
                                                 simpleQueryGetRow)
import qualified Control.Monad.Trans as MT (liftIO)
import qualified Data.List as DL (lookup)
import qualified Data.Maybe as MB (fromMaybe)
import qualified Data.Time.LocalTime as LT (ZonedTime(zonedTimeToLocalTime),
                                            getZonedTime)
import qualified Database.HDBC as DB (toSql, fromSql, SqlValue)

sqlToSession :: [(String, DB.SqlValue)] -> FTEN.Habolous FTS.Session
sqlToSession row = do
    let sid = DB.fromSql $
               MB.fromMaybe (DB.toSql "") $
               DL.lookup "session_uuid" row
        empid = DB.fromSql $
                 MB.fromMaybe (DB.toSql (0 :: Integer)) $
                 DL.lookup "employee_id" row
    t <- MT.liftIO LT.getZonedTime
    e <- FDBE.getEmployeeById empid
    return FTS.Session {FTS.sessionId = sid,
                        FTS.sessionEmployee = case e of
                                                  Nothing -> FTE.nullEmployee
                                                  Just empl -> empl,
                        FTS.expires = DB.fromSql $
                                      MB.fromMaybe (DB.toSql
                                       (LT.zonedTimeToLocalTime t)) $
                                      DL.lookup "expires" row}

sessionToSql :: FTS.Session -> [DB.SqlValue]
sessionToSql session = [DB.toSql $ FTS.sessionId session,
                        DB.toSql $ FTE.employeeId $
                         FTS.sessionEmployee session,
                        DB.toSql $ FTS.expires session]

createSession :: FTS.Session -> FTEN.Habolous Integer
createSession session = do
    FDBH.simpleSqlRun query values
    where
      query = "INSERT INTO habolous.session (session_uuid, "
              ++ "employee_id, expires) VALUES (?,?,?);"
      values = sessionToSql session

getSession :: String -> FTEN.Habolous (Maybe FTS.Session)
getSession uuid = do
    let query = "select * from habolous.session "
                ++ "where session_uuid=?;"
        values = [DB.toSql uuid]
    dbs <- FDBH.simpleQueryGetRow query values
    case dbs of
        [] -> return Nothing
        _  -> do
            s <- sqlToSession dbs
            return $ Just s
