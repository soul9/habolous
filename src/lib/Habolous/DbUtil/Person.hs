module Habolous.DbUtil.Person (getPersonById,
                              getPerson,
                              getPersons,
                              createPerson,
                              deletePerson,
                              updatePerson) where

import qualified Habolous.Types.HabolousEnv as FBE (Habolous)
import qualified Habolous.Types.Person as FBP (Person(..))
import qualified Habolous.DbUtil.Helpers as FDBH (simpleQueryGetRowById,
                                                 simpleQueryGetRows,
                                                 simpleSqlRun)
import qualified Database.HDBC as DB (SqlValue, toSql, fromSql)
import qualified Data.List as DL (lookup)
import qualified Data.Maybe as MB (fromMaybe)
import qualified Data.Time.Calendar as C (Day(ModifiedJulianDay))
import qualified Data.Text as T (pack)

sqlToPerson :: [(String, DB.SqlValue)] -> FBP.Person
sqlToPerson row = let pfn = DB.fromSql $
                              MB.fromMaybe (DB.toSql "") $
                              DL.lookup "firstname" row
                      pln = DB.fromSql $
                              MB.fromMaybe (DB.toSql "") $
                              DL.lookup "lastname" row
                      pid = DB.fromSql $
                              MB.fromMaybe (DB.toSql (0 :: Integer)) $
                              DL.lookup "person_id" row
                      pbd = DB.fromSql $
                              MB.fromMaybe (DB.toSql (C.ModifiedJulianDay 0)) $
                              DL.lookup "birthday" row
                  in FBP.Person {FBP.firstName = T.pack pfn,
                                 FBP.lastName  = T.pack pln,
                                 FBP.birthday  = pbd,
                                 FBP.personId  = pid
                                 }

personToSql :: FBP.Person -> [DB.SqlValue]
personToSql person = [DB.toSql (FBP.firstName person),
                      DB.toSql (FBP.lastName person),
                      DB.toSql (FBP.birthday person)]
                     ++ case FBP.personId person of
                            0 -> []
                            pid -> [DB.toSql pid]

getPersonById :: Integer -> FBE.Habolous (Maybe FBP.Person)
getPersonById personid = do
    let query = "SELECT * FROM habolous.person WHERE person_id=?;"
    row <- FDBH.simpleQueryGetRowById query personid
    case row of
        [] -> return Nothing
        _  -> return $ Just $ sqlToPerson row

getPerson :: Integer -> FBE.Habolous FBP.Person
getPerson pid = do
    person <- getPersonById pid
    case person of
        Nothing -> fail $ "No such Person (person_id): " ++ show pid
        Just p -> return p

getPersons :: String -> [DB.SqlValue]
              -> FBE.Habolous ([FBP.Person])
getPersons query values = do
    -- FIXME: have a list of possible column names
    -- and be able to filter on multiple criteria
    rows <- FDBH.simpleQueryGetRows query values
    return $ map sqlToPerson rows

createPerson :: FBP.Person -> FBE.Habolous Integer
createPerson person = FDBH.simpleSqlRun query values
                      where
                        query = "INSERT INTO habolous.person "
                                ++ "(firstname, lastname, birthday)"
                                ++ "VALUES (?,?,?);"
                        values = personToSql person

updatePerson :: FBP.Person -> FBE.Habolous Integer
updatePerson person = FDBH.simpleSqlRun query values
                      where
                        query = "UPDATE habolous.person SET "
                                ++ "firstname=?,lastname=?,"
                                ++ "birthday=? WHERE person_id=?;"
                        values = personToSql person

deletePerson :: FBP.Person -> FBE.Habolous Integer
deletePerson person = FDBH.simpleSqlRun query values
                      where
                        query = "DELETE FROM habolous.person "
                                ++ "WHERE person_id=?;"
                        values = [DB.toSql (FBP.personId person)]
