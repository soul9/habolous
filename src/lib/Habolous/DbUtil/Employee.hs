module Habolous.DbUtil.Employee (getEmployeeById,
                                getEmployeeByLogin,
                                getEmployee,
                                createEmployee,
                                updateEmployee,
                                deleteEmployee,
                                checkPassword) where

import qualified Habolous.Types.Employee as FTE (Employee(..))
import qualified Data.Text as T (pack)
import qualified Database.HDBC as DB (toSql, fromSql, SqlValue)
import qualified Habolous.Types.HabolousEnv as FTEN (Habolous)
import qualified Habolous.DbUtil.Helpers as FDBH (simpleQueryGetRowById,
                                                 simpleQueryGetRow,
                                                 simpleSqlRun)
import qualified Habolous.Types.Person as FBP (Person(..))
import qualified Habolous.Types.Group as FBG (Group(..))
import qualified Habolous.Types.Role as FBR (Role(..))
import qualified Habolous.DbUtil.Person as FDBP (getPerson)
import qualified Habolous.DbUtil.Group as FDBG (getGroup)
import qualified Habolous.DbUtil.Role as FDBR (getRole)
import qualified Data.List as DL (lookup)
import qualified Data.Maybe as MB (fromMaybe)

sqlToEmployee :: [(String, DB.SqlValue)] -> FTEN.Habolous FTE.Employee
sqlToEmployee row = do
    let elog = DB.fromSql $
                MB.fromMaybe (DB.toSql "") $
                DL.lookup "login_name" row
        epass = DB.fromSql $
                 MB.fromMaybe (DB.toSql "") $
                 DL.lookup "password" row
        eid = DB.fromSql $
               MB.fromMaybe (DB.toSql (0 :: Integer)) $
               DL.lookup "employee_id" row
        egid = DB.fromSql $
                MB.fromMaybe (DB.toSql (0 :: Integer)) $
                DL.lookup "employee_group_id" row
        erid = DB.fromSql $
                MB.fromMaybe (DB.toSql (0 :: Integer)) $
                DL.lookup "employee_role_id" row
        epid = DB.fromSql $
                MB.fromMaybe (DB.toSql (0 :: Integer)) $
                DL.lookup "employee_person_id" row
    p <- FDBP.getPerson epid
    g <- FDBG.getGroup egid
    r <- FDBR.getRole erid
    return FTE.Employee {FTE.loginName = T.pack elog,
                         FTE.password = T.pack epass,
                         FTE.employeeId = eid,
                         FTE.employeeGroup = g,
                         FTE.employeeRole = r,
                         FTE.employeePerson = p}

employeeToSql :: FTE.Employee -> [DB.SqlValue]
employeeToSql employee = [DB.toSql $ FTE.loginName employee,
                          DB.toSql $ FTE.password employee,
                          DB.toSql $ FBG.groupId $
                           FTE.employeeGroup employee,
                          DB.toSql $ FBP.personId $
                           FTE.employeePerson employee,
                          DB.toSql $ FBR.roleId $
                           FTE.employeeRole employee]
                          ++ case FTE.employeeId employee of
                                 0 -> []
                                 n -> [DB.toSql n]

employeeColumnNames :: ([String], [String])
employeeColumnNames = let names = ["login_name",
                                   "password",
                                   "employee_group_id",
                                   "employee_person_id",
                                   "employee_role_id",
                                   "employee_id"]
                               in (names, map (\_->"?") names)

getEmployeeById :: Integer -> FTEN.Habolous (Maybe FTE.Employee)
getEmployeeById employeeid =  do
    let query = "SELECT * FROM habolous.employee WHERE employee_id=?;"
    row <- FDBH.simpleQueryGetRowById query employeeid
    case row of
        [] -> return $ Nothing
        _  -> do
            employee <- sqlToEmployee row
            return $ Just employee

getEmployee :: Integer -> FTEN.Habolous (FTE.Employee)
getEmployee eid = do
    employee <- getEmployeeById eid
    case employee of
        Nothing -> fail $ "No such employee (employee_id): " ++ show eid
        Just e -> return e

createEmployee :: FTE.Employee -> FTEN.Habolous Integer
createEmployee employee = FDBH.simpleSqlRun query values
                          where
                            (colnames, _) =
                                    employeeColumnNames
                            cols = tail $ concatMap (\x -> ","++x) $
                                    filter ("employee_id"/=) colnames
                            valps = tail $ concatMap (\_ -> ",?") $
                                     filter ("employee_id"/=) colnames
                            query = "INSERT INTO habolous.employee ("
                                    ++ cols ++ ") VALUES ("
                                    ++ valps ++ ");"
                            values = employeeToSql employee

updateEmployee :: FTE.Employee -> FTEN.Habolous Integer
updateEmployee employee = FDBH.simpleSqlRun query values
                          where
                            (colnames, valuepos) =
                                 employeeColumnNames
                            qq = tail $ concat $ map (\x->","++x) $
                                  zipWith (\x y-> x++"="++y) (filter
                                   ("employee_id"/=) colnames) valuepos
                            query = "UPDATE habolous.employee SET "
                                    ++ qq ++ " "
                                    ++ " WHERE employee_id=?;"
                            values = employeeToSql employee

deleteEmployee :: FTE.Employee -> FTEN.Habolous Integer
deleteEmployee employee = FDBH.simpleSqlRun query values
                          where
                            query = "DELETE FROM habolous.employee "
                                    ++ "WHERE employee_id=?;"
                            values = [DB.toSql (FTE.employeeId employee)]

getEmployeeByLogin :: String -> FTEN.Habolous (Maybe FTE.Employee)
getEmployeeByLogin login = do
    sqle <- FDBH.simpleQueryGetRow query value -- login_name is unique
    case sqle of
        [] -> return Nothing
        _  -> do
            e <- sqlToEmployee sqle
            return $ Just e
    where
      query = "SELECT * FROM habolous.employee "
              ++ "WHERE login_name=?;"
      value = [DB.toSql login]

checkPassword :: String -> String -> FTEN.Habolous (Maybe FTE.Employee)
checkPassword login password = do
    mbemp <- getEmployeeByLogin login
    case mbemp of
        Nothing -> return Nothing
        Just e  -> do
            case (T.pack password) == (FTE.password e) of
                True  -> return $ Just e
                False  -> return $ Nothing