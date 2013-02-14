module Habolous.Routes.Login (withAuth) where

import qualified Control.Monad as M (msum)
import qualified Control.Monad.Trans as MT (liftIO)
import qualified Habolous.Templates.Login as FTL (html)
import qualified Habolous.Types.Employee as FTEM (nullEmployee)
import qualified Habolous.Types.Session as FTS (Session(..), nullSession)
import qualified Habolous.Types.HabolousEnv as FTE
        (Habolous, getEnv, putEnv, HabolousEnv(..))
import qualified Habolous.DbUtil.Session as FDBS
        (getSession, createSession)
import qualified Habolous.DbUtil.Employee as FDBE (checkPassword)
import qualified Happstack.Server.Response as HSRE (ok, toResponse, seeOther)
import qualified Happstack.Server.Routing as HSR (method, dir, nullDir)
import qualified Happstack.Server.Types as HST (Response, Method(GET,POST))
import qualified Happstack.Server.Cookie as HSC (CookieLife(Session),
                                                 Cookie(..), addCookie)
import qualified Happstack.Server.RqData as HSRQ (decodeBody, look, body,
                                                  defaultBodyPolicy,
                                                  lookCookie,
                                                  getDataFn, RqData)
import qualified Data.UUID as DU (toString)
import qualified Data.UUID.V4 as DU4 (nextRandom)
import qualified Data.Text as T (pack)
import qualified Data.Text.Encoding as E (encodeUtf8)
import qualified Data.ByteString as B (unpack)
import qualified Crypto.Hash.SHA3 as SHA3 (hash)
import qualified Text.Printf as TP (printf)

initSession :: FTE.Habolous ()
initSession = do
    c <- lookAuthCookie
    case c of
        Nothing -> return ()
        Just s -> do
            env <- FTE.getEnv
            FTE.putEnv env {FTE.session = s}

routes :: [FTE.Habolous HST.Response]
routes = [HSR.dir "login" $ HSR.nullDir >> do HSR.method HST.POST
                                              handlePost,
          HSR.dir "login" $ HSR.nullDir >> do HSR.method HST.GET
                                              handleGet,
          HSRE.seeOther "/login" $ HSRE.toResponse $
           "The webpage you have requested can be found at /login"]

withAuth :: [FTE.Habolous HST.Response] -> FTE.Habolous HST.Response
withAuth actions = do
    initSession
    l <- isLoggedIn
    case l of
        True  -> M.msum actions
        False -> M.msum routes

isLoggedIn :: FTE.Habolous Bool
isLoggedIn = do
    env <- FTE.getEnv
    return $ FTS.sessionEmployee (FTE.session env) /= FTEM.nullEmployee

failedAuthResp :: FTE.Habolous HST.Response
failedAuthResp = HSRE.ok $ HSRE.toResponse $
                  FTL.html "Failed to log you in: Please authenticate to enter"
                   Nothing

handleGet :: FTE.Habolous HST.Response
handleGet = do
    l <- isLoggedIn
    case l of
        False -> HSRE.ok $ HSRE.toResponse $
                  FTL.html "Please authenticate to enter" Nothing
        True  -> authSuccess

getAuthPair :: HSRQ.RqData (String, String)
getAuthPair = do
    login <- HSRQ.body $ HSRQ.look "login"
    password <- HSRQ.body $ HSRQ.look "password"
    return (login, concatMap (TP.printf "%02x") $
               B.unpack $ SHA3.hash 512 $ E.encodeUtf8 $ T.pack password)

handlePost :: FTE.Habolous HST.Response
handlePost = do
    HSRQ.decodeBody (HSRQ.defaultBodyPolicy "/tmp/" 0 4096 4096)
    lp <- HSRQ.getDataFn getAuthPair
    case lp of
        Left _ -> failedAuthResp -- FIXME: handle errors
        Right (l, p) -> do
            memp <- FDBE.checkPassword l p
            uuid <- MT.liftIO $ DU4.nextRandom
            s <- FTS.nullSession
            case memp of
                Nothing -> failedAuthResp
                Just e -> do
                    let sess = s {FTS.sessionId = (DU.toString uuid),
                                  FTS.sessionEmployee = e}
                    env <- FTE.getEnv
                    FTE.putEnv env {FTE.session = sess}
                    r <- FDBS.createSession sess
                    case r of
                        0 -> failedAuthResp
                        _ -> do
                          HSC.addCookie HSC.Session $
                           HSC.Cookie "1" "/" "" "habolousSession"
                             (DU.toString uuid) False True
                          authSuccess

lookAuthCookie :: FTE.Habolous (Maybe FTS.Session)
lookAuthCookie = do
    cookie <- HSRQ.getDataFn $ HSRQ.lookCookie "habolousSession"
    case cookie of
        Left _ -> return Nothing
        Right c -> do
            let uuid = HSC.cookieValue c
            s <- FDBS.getSession uuid
            return s

authSuccess :: FTE.Habolous HST.Response
authSuccess = do
    env <- FTE.getEnv
    HSRE.ok $ HSRE.toResponse $ FTL.html "Welcome" $
     Just $ FTS.sessionEmployee $ FTE.session env