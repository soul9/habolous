module Main where

-- BIG FAT WARNING: this code is ugly
-- TODO: add tests for all functions in DB code?

import qualified System.Environment as ENV (getArgs)

import qualified Control.Monad as M
import qualified Control.Monad.Trans as MT
import qualified Data.Text as T
import qualified Habolous.Types.HabolousEnv as FTEN
import qualified Habolous.Types.Config as FTC
import qualified Habolous.Types.Group as FTG
import qualified Habolous.Types.Person as FTP
import qualified Habolous.Types.Role as FTR
import qualified Habolous.Types.Employee as FTE
import qualified Habolous.DbUtil.HabolousEnv as FDBEN
import qualified Habolous.DbUtil.Group as FDBG
import qualified Habolous.DbUtil.Person as FDBP
import qualified Habolous.DbUtil.Role as FDBR
import qualified Habolous.DbUtil.Employee as FDBE
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import qualified Control.Concurrent.MVar as MVar
import qualified Database.HDBC as DB
-- TODO: qualified imports
import Happstack.Server hiding (path)
import Happstack.Server.SURI
import Happstack.Server.Internal.MessageWrap

main :: IO ()
main = do
    args <- ENV.getArgs
    case length args of
        1 -> do cf <- FTC.parseConfig $ head args
                case cf of
                    Right c -> do req <- mkRequest GET "/foo" [] Map.empty L.empty
                                  conn <- FDBEN.connect $ FTC.databaseConfig c
                                  r <- simpleHTTP'' (do let n = 10
                                                        env <- FTEN.initEnv c conn
                                                        (res, nenv) <- FTEN.runHabolous (do
                                                            -- Start cleanup
                                                            _ <- testUpdateGroups n
                                                            _ <- testUpdateRoles n
                                                            _ <- testUpdatePersons n
                                                            _ <- testUpdateEmployees n
                                                            _ <- testDeleteEmployees n
                                                            _ <- testDeleteGroups n
                                                            _ <- testDeleteRoles n
                                                            _ <- testDeletePersons n
                                                            -- End cleanup
                                                            gres <- testGroups n
                                                            pres <- testPersons n
                                                            rres <- testRoles n
                                                            eres <- testEmployees n
                                                            return $ show [gres, pres, rres, eres]) env
                                                        _ <- FTEN.runHabolous FTEN.endEnv nenv
                                                        return res) req
                                  print r
                    Left e -> print e
        _ -> putStrLn "Usage: filldb /path/to/configuration.conf"

-- Helper since we do need a request to test Habolous stuff
mkRequest :: Method -> String -> [(String, Cookie)] -> Headers -> L.ByteString -> IO Request
mkRequest meth uri cookies headers bo =
    do let u = toSURI uri
       ib <- MVar.newEmptyMVar
       b  <- MVar.newMVar (Body bo)
       return $ Request { rqMethod      = meth
                        , rqPaths       = (pathEls (path u))
                        , rqUri         = (path u)
                        , rqQuery       = (query u)
                        , rqInputsQuery = (queryInput u)
                        , rqInputsBody  = ib
                        , rqCookies     = cookies
                        , rqVersion     = HttpVersion 1 1
                        , rqHeaders     = headers
                        , rqBody        = b
                        , rqPeer        = ("",0)
                        , rqSecure      = False
                        }

-- Group Functions
mkGroup :: Integer -> FTG.Group
mkGroup i = FTG.nullGroup{FTG.name=T.pack $ show i}

testCreateGroups :: Integer -> FTEN.Habolous ([Integer])
testCreateGroups n = M.mapM FDBG.createGroup $ map mkGroup [1,2..n]

testUpdateGroups :: Integer -> FTEN.Habolous ([Integer])
testUpdateGroups n = M.mapM (\x-> do
    g <- FDBG.getGroupByName $ show x
    case g of
        Nothing -> return 0
        Just gr -> FDBG.updateGroup gr{FTG.name=T.pack $
                    "Update" ++ show x}) [1,2..n]

testDeleteGroups :: Integer -> FTEN.Habolous ([Integer])
testDeleteGroups n = M.mapM (\x-> do
    g <- FDBG.getGroupByName $ "Update" ++ show x
    case g of
        Nothing -> return 0
        Just gr -> FDBG.deleteGroup gr) [1,2..n]

testGroups :: Integer -> FTEN.Habolous ([[Integer]])
testGroups n = do
    cr <- testCreateGroups n
    MT.liftIO $ print $ "cregr " ++ show cr
    up <- testUpdateGroups n
    MT.liftIO $ print $ "updgr " ++ show up
    rm <- testDeleteGroups n
    MT.liftIO $ print $ "delgr " ++ show rm
    return [cr,up,rm]

-- Role Functions
mkRole :: Integer -> FTR.Role
mkRole i = FTR.nullRole{FTR.name=T.pack $ show i}

testCreateRoles :: Integer -> FTEN.Habolous ([Integer])
testCreateRoles n = M.mapM FDBR.createRole $ map mkRole [1,2..n]

testUpdateRoles :: Integer -> FTEN.Habolous ([Integer])
testUpdateRoles n = M.mapM (\x-> do
    r <- FDBR.getRoleByName $ show x
    case r of
        Nothing -> return 0
        Just ro -> FDBR.updateRole ro{FTR.name=T.pack $
                    "Update" ++ show x}) [1,2..n]

testDeleteRoles :: Integer -> FTEN.Habolous ([Integer])
testDeleteRoles n = M.mapM (\x-> do
    r <- FDBR.getRoleByName $ "Update" ++ show x
    case r of
        Nothing -> return 0
        Just ro -> FDBR.deleteRole ro) [1,2..n]

testRoles :: Integer -> FTEN.Habolous ([[Integer]])
testRoles n = do
    cr <- testCreateRoles n
    MT.liftIO $ print $ "crero " ++ show cr
    up <- testUpdateRoles n
    MT.liftIO $ print $ "updro " ++ show up
    rm <- testDeleteRoles n
    MT.liftIO $ print $ "delro " ++ show rm
    return [cr,up,rm]

-- Person Functions
mkPerson :: Integer -> FTP.Person
mkPerson i = FTP.nullPerson{FTP.firstName=T.pack $ "first" ++ show i,
                            FTP.lastName=T.pack $ "last" ++ show i}

testCreatePersons :: Integer -> FTEN.Habolous ([Integer])
testCreatePersons n = M.mapM FDBP.createPerson $ map mkPerson [1,2..n]

testUpdatePersons :: Integer -> FTEN.Habolous ([Integer])
testUpdatePersons n = M.mapM (\x-> do
    ps <- FDBP.getPersons "SELECT * FROM habolous.person WHERE firstname=? AND lastname=?;"
           [DB.toSql ("first" ++ show x), DB.toSql ("last" ++ show x)]
    case ps of
        p:[] -> FDBP.updatePerson p {FTP.firstName=T.pack $ "Update" ++ show x}
        l    -> do
            res <- M.mapM (\p-> FDBP.updatePerson p {FTP.firstName=T.pack $ "Update" ++ show x}) l
            return $ sum res) [1,2..n]

testDeletePersons :: Integer -> FTEN.Habolous ([Integer])
testDeletePersons n = M.mapM (\x-> do
    ps <- FDBP.getPersons "SELECT * FROM habolous.person WHERE firstname=? AND lastname=?;"
           [DB.toSql ("Update" ++ show x), DB.toSql ("last" ++ show x)]
    case ps of
        p:[] -> FDBP.deletePerson p
        l    -> do
            res <- M.mapM FDBP.deletePerson l
            return $ sum res) [1,2..n]

testPersons :: Integer -> FTEN.Habolous ([[Integer]])
testPersons n = do
    cr <- testCreatePersons n
    MT.liftIO $ print $ "crepe " ++ show cr
    up <- testUpdatePersons n
    MT.liftIO $ print $ "updpe " ++ show up
    rm <- testDeletePersons n
    MT.liftIO $ print $ "delpe " ++ show rm
    return [cr, up, rm]

-- Employee Functions
mkEmployee :: Integer -> FTEN.Habolous FTE.Employee
mkEmployee i = do
    g <- do
        mg <- FDBG.getGroupByName $ "Update"++show i
        case mg of
            Nothing -> return FTG.nullGroup
            Just gg -> return gg
    r <- do
        mr <- FDBR.getRoleByName $ "Update"++show i
        case mr of
            Nothing -> return FTR.nullRole
            Just rr -> return rr
    p <- do ps <- FDBP.getPersons "SELECT * FROM habolous.person WHERE firstname=? AND lastname=?;"
                   [DB.toSql ("Update" ++ show i), DB.toSql ("last" ++ show i)]
            case ps of
                [] -> return FTP.nullPerson
                p:_ -> return p
    return $ FTE.nullEmployee {
        FTE.loginName = T.pack $ show i,
        FTE.password  = T.pack $ show i,
        FTE.employeeGroup = g,
        FTE.employeeRole = r,
        FTE.employeePerson = p}

testCreateEmployees :: Integer -> FTEN.Habolous ([Integer])
testCreateEmployees n = do
    es <- M.mapM mkEmployee [1,2..n]
    M.mapM FDBE.createEmployee es

testUpdateEmployees :: Integer -> FTEN.Habolous ([Integer])
testUpdateEmployees n = M.mapM (\x-> do
    me <- FDBE.getEmployeeByLogin $ show x
    case me of
        Nothing -> return $ 0
        Just e -> FDBE.updateEmployee e {FTE.loginName = T.pack $ "Update" ++ show x}) [1,2..n]

testDeleteEmployees :: Integer -> FTEN.Habolous ([Integer])
testDeleteEmployees n = M.mapM (\x-> do
    me <- FDBE.getEmployeeByLogin $ "Update" ++ show x
    case me of
        Nothing -> return $ 0
        Just e -> FDBE.deleteEmployee e) [1,2..n]

testEmployees :: Integer -> FTEN.Habolous ([[Integer]])
testEmployees n = do
    _ <- testCreateGroups n
    _ <- testCreatePersons n
    _ <- testCreateRoles n
    _ <- testUpdateGroups n
    _ <- testUpdateRoles n
    _ <- testUpdatePersons n
    cr <- testCreateEmployees n
    MT.liftIO $ print $ "creem " ++ show cr
    up <- testUpdateEmployees n
    MT.liftIO $ print $ "updem " ++ show up
    rm <- testDeleteEmployees n
    MT.liftIO $ print $ "delem " ++ show rm
    _ <- testDeleteGroups n
    _ <- testDeleteRoles n
    _ <- testDeletePersons n
    return [cr, up, rm]
