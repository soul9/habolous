module Habolous.DbUtil.Group (getGroupById,
                             getGroup,
                             getGroupByName,
                             createGroup,
                             updateGroup,
                             deleteGroup) where

import qualified Habolous.Types.Group as FBG (Group(..))
import qualified Data.Text as T (pack)
import qualified Database.HDBC as DB (SqlValue, toSql, fromSql)
import qualified Habolous.Types.HabolousEnv as FBE (Habolous)
import qualified Habolous.DbUtil.Helpers as FDBH (simpleQueryGetRowById,
                                                 simpleSqlRun,
                                                 simpleQueryGetRow)
import qualified Data.List as DL (lookup)
import qualified Data.Maybe as MB (fromMaybe)

sqlToGroup :: [(String, DB.SqlValue)] -> FBG.Group
sqlToGroup row = let gn = DB.fromSql $
                            MB.fromMaybe (DB.toSql "") $
                            DL.lookup "group_name" row
                     gid = DB.fromSql $
                            MB.fromMaybe (DB.toSql (0 :: Integer)) $
                            DL.lookup "group_id" row
                 in FBG.Group {FBG.name = T.pack gn,
                               FBG.groupId = gid
                              }
groupToSql :: FBG.Group -> [DB.SqlValue]
groupToSql group = [DB.toSql (FBG.name group)]
                   ++ case FBG.groupId group of
                       0 -> []
                       gid -> [DB.toSql gid]

getGroupById :: Integer
                -> FBE.Habolous (Maybe FBG.Group)
getGroupById groupid = do
    let query = "SELECT * FROM habolous.group WHERE group_id=?;"
    row <- FDBH.simpleQueryGetRowById query groupid
    case row of
        [] -> return Nothing
        _  -> return $ Just $ sqlToGroup row

getGroupByName :: String -> FBE.Habolous (Maybe FBG.Group)
getGroupByName name = do
    let query = "SELECT * FROM habolous.group "
              ++ "WHERE group_name=?"
        values = [DB.toSql name]
    row <- FDBH.simpleQueryGetRow query values
    case row of
        [] -> return Nothing
        _  -> return $ Just $ sqlToGroup row

getGroup :: Integer -> FBE.Habolous FBG.Group
getGroup gid = do
    group <- getGroupById gid
    case group of
        Nothing -> fail $ "No such Group (group_id): " ++ show gid
        Just g -> return g

createGroup :: FBG.Group
               -> FBE.Habolous Integer
createGroup group = FDBH.simpleSqlRun query values
                    where
                      query = "INSERT INTO habolous.group "
                              ++ "(group_name) VALUES (?);"
                      values = groupToSql group

updateGroup :: FBG.Group
               -> FBE.Habolous Integer
updateGroup group = FDBH.simpleSqlRun query values
                    where
                      query = "UPDATE habolous.group SET "
                              ++ "group_name=? "
                              ++ "WHERE group_id=?;"
                      values = groupToSql group

deleteGroup :: FBG.Group
               -> FBE.Habolous Integer
deleteGroup group = FDBH.simpleSqlRun query values
                    where
                      query = "DELETE FROM habolous.group "
                              ++ "WHERE group_id=?;"
                      values = [DB.toSql (FBG.groupId group)]
