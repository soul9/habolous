module Habolous.DbUtil.Helpers (simpleQueryGetRow,
                               simpleQueryGetRowById,
                               simpleQueryGetRows,
                               simpleSqlRun, columns) where

import qualified Control.Monad.State as CMS (liftIO)
import qualified Data.Maybe as MB (fromMaybe)
import qualified Habolous.Types.HabolousEnv as FBE
                       (Habolous, HabolousEnv(..), getEnv)
import qualified Database.HDBC as DB (Statement, SqlValue, SqlError,
                                      prepare, run, execute, handleSql,
                                      fetchRowAL, fetchAllRowsAL, toSql,
                                      commit, rollback, fromSql)

simpleQueryGetRow :: String -> [DB.SqlValue]
                     -> FBE.Habolous [(String, DB.SqlValue)]
simpleQueryGetRow query values = do
    env <- FBE.getEnv
    stmt <- CMS.liftIO $ handleSqlPrepare env query
    case stmt of
        Just st -> do
                       _ <- CMS.liftIO $
                             DB.handleSql (handleSqlRun env) $
                             DB.execute st $ map DB.toSql values
                       -- can't be more than one row since ids
                       -- are (should be) unique
                       row <- CMS.liftIO $
                               DB.handleSql (handleFetch env) $
                               DB.fetchRowAL st
                       return $ MB.fromMaybe [] row
        Nothing -> return []

simpleQueryGetRowById :: String -> Integer
                     -> FBE.Habolous [(String, DB.SqlValue)]
simpleQueryGetRowById query rowid = simpleQueryGetRow query value
                                    where
                                      value = [DB.toSql rowid]

simpleQueryGetRows :: String -> [DB.SqlValue]
                     -> FBE.Habolous [[(String, DB.SqlValue)]]
simpleQueryGetRows query values = do
    env <- FBE.getEnv
    stmt <- CMS.liftIO $ handleSqlPrepare env query
    case stmt of
        Just st -> do
                       _ <- CMS.liftIO $
                             DB.handleSql (handleSqlRun env) $
                             DB.execute st values
                       row <- CMS.liftIO $
                               DB.handleSql (handleFetchAll env) $
                               DB.fetchAllRowsAL st
                       return row
        Nothing -> return []

simpleSqlRun :: String -> [DB.SqlValue] -> FBE.Habolous Integer
simpleSqlRun query values = do
    env <- FBE.getEnv
    rows <- CMS.liftIO $
             DB.handleSql (handleSqlRun env) $
             DB.run (FBE.databaseConnection env) query values
    CMS.liftIO $
     DB.handleSql (handleSqlException env) $
     DB.commit (FBE.databaseConnection env)
    return rows

columns :: String -> FBE.Habolous [String]
columns tbl = do
    rows <- simpleQueryGetRows query value
    return $ map (\(_, val)->DB.fromSql val) (concat rows)
    where
      query = "select column_name from information_schema.columns"
              ++ " where table_name = ?;"
      value = [DB.toSql tbl]

-- Error/Exception handling
handleSqlPrepare :: FBE.HabolousEnv -> String -> IO (Maybe DB.Statement)
handleSqlPrepare cf query = do
    DB.handleSql errorHandler $ prep query
    where
      errorHandler :: DB.SqlError -> IO (Maybe DB.Statement)
      errorHandler err = do
          handleSqlException cf err
          return Nothing
      prep :: String -> IO (Maybe DB.Statement)
      prep q = do
          st <- DB.prepare (FBE.databaseConnection cf) q
          return $ Just st

handleFetchAll :: FBE.HabolousEnv -> DB.SqlError -> IO [[(String, DB.SqlValue)]]
handleFetchAll cf err = do
    handleSqlException cf err
    return []

handleFetch :: FBE.HabolousEnv -> DB.SqlError
               -> IO (Maybe [(String, DB.SqlValue)])
handleFetch cf err = do
    handleSqlException cf err
    return Nothing

handleSqlRun :: FBE.HabolousEnv -> DB.SqlError -> IO (Integer)
handleSqlRun cf err = do
    handleSqlException cf err
    return 0

handleSqlException :: FBE.HabolousEnv -> DB.SqlError -> IO ()
handleSqlException cf err = do
    DB.rollback (FBE.databaseConnection cf)
    fail $ "SQL Error: " ++ show err
