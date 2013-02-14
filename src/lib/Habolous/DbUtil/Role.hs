module Habolous.DbUtil.Role (getRoleById,
                            getRole,
                            getRoleByName,
                            createRole,
                            updateRole,
                            deleteRole) where

import qualified Habolous.Types.Role as FTR
import qualified Database.HDBC as DB (SqlValue, toSql, fromSql)
import qualified Habolous.Types.HabolousEnv as FBE (Habolous)
import qualified Habolous.DbUtil.Helpers as FDBH (simpleQueryGetRowById,
                                                 simpleSqlRun,
                                                 simpleQueryGetRow)
import qualified Data.Maybe as MB (fromMaybe)
import qualified Data.Text as T (pack)
import qualified Data.List as DL (lookup)

sqlToRole :: [(String, DB.SqlValue)] -> FTR.Role
sqlToRole row = let rn = DB.fromSql $
                           MB.fromMaybe (DB.toSql "") $
                           DL.lookup "role_name" row
                    ri = DB.fromSql $
                           MB.fromMaybe (DB.toSql (0 :: Integer)) $
                           DL.lookup "role_id" row
                in FTR.Role {FTR.name   = T.pack rn,
                             FTR.roleId = ri
                            }

roleToSql :: FTR.Role -> [DB.SqlValue]
roleToSql role = [DB.toSql $ FTR.name role]
                 ++ case FTR.roleId role of
                     0   -> []
                     rid -> [DB.toSql rid]

getRoleById :: Integer -> FBE.Habolous (Maybe FTR.Role)
getRoleById roleid = do
    let query = "SELECT * FROM habolous.role WHERE role_id=?;"
    row <- FDBH.simpleQueryGetRowById query roleid
    case row of
        [] -> return Nothing
        _  -> return $ Just $ sqlToRole row

getRole :: Integer -> FBE.Habolous FTR.Role
getRole rid = do
    role <- getRoleById rid
    case role of
        Nothing -> fail $ "No such Role (role_id): " ++ show rid
        Just r -> return r

getRoleByName :: String -> FBE.Habolous (Maybe FTR.Role)
getRoleByName name = do
    let query = "SELECT * FROM habolous.role WHERE role_name=?"
        values = [DB.toSql name]
    row <- FDBH.simpleQueryGetRow query values
    case row of
        [] -> return Nothing
        _  -> return $ Just $ sqlToRole row

createRole :: FTR.Role -> FBE.Habolous Integer
createRole role = FDBH.simpleSqlRun query values
                  where
                    query = "INSERT INTO habolous.role "
                            ++ "(role_name) VALUES (?);"
                    values = roleToSql role

updateRole :: FTR.Role -> FBE.Habolous Integer
updateRole role = FDBH.simpleSqlRun query values
                  where
                    query = "UPDATE habolous.role SET "
                            ++ "role_name=? WHERE role_id=?;"
                    values = roleToSql role

deleteRole :: FTR.Role -> FBE.Habolous Integer
deleteRole role = FDBH.simpleSqlRun query values
                  where
                    query = "DELETE FROM habolous.role WHERE role_id=?;"
                    values = [DB.toSql (FTR.roleId role)]
