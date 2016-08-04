{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Pokemon.Login (login) where

import           Control.Monad                (join)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.List                    as List
import           Data.Text                    (Text)
import qualified Data.Text.Encoding           as Encoding
import           Network.HTTP.Conduit
import qualified Network.HTTP.Types           as W
import qualified Network.HTTP.Types.URI       as URI

import qualified Pokemon.AuthRequest          as AuthRequest
import qualified Pokemon.Config               as Config
import qualified Pokemon.LoginRequest         as LoginRequest
import           Pokemon.LoginToken           (LoginToken)
import qualified Pokemon.LoginToken           as LoginToken
import           Pokemon.Profile              (Profile)
import qualified Pokemon.Profile              as Profile


httpStep :: MonadResource m
         => Request -> Manager -> m (Maybe (Request, String))
httpStep req manager =
  continue <$> httpLbs req { redirectCount = 0 } manager
  where
    continue res = do
      nextReq <- getRedirectedRequest
        Config.loginReq
        (responseHeaders res)
        (responseCookieJar res)
        (W.statusCode (responseStatus res))
      nextUrl <- read . show <$> lookup "Location" (responseHeaders res)
      return (nextReq, nextUrl)


getLoginToken :: MonadResource m
              => Manager -> m (CookieJar, LoginToken, String)
getLoginToken manager = do
  let req = Config.loginReq
  next <- httpStep req manager
  case next of
    Nothing -> fail "get token"
    Just (nextReq, nextUrl) -> do
      res <- httpLbs nextReq manager
      case LoginToken.decode $ responseBody res of
        Nothing    -> fail "login token"
        Just token -> return (responseCookieJar res, token, nextUrl)


getLoginTicket :: MonadResource m
               => Profile -> Manager -> CookieJar -> LoginToken -> String -> m String
getLoginTicket profile manager cookieJar token nextUrl = do
  nextReq <- parseRequest nextUrl
  let req = urlEncodedBody (LoginRequest.toList loginData) (Config.setUserAgent nextReq) { cookieJar = Just cookieJar }
  next <- httpStep req manager
  case next >>= \(r, u) -> (,) r <$> getTicket u of
    Nothing          -> fail "login"
    Just (_, ticket) -> return ticket
  where
    getTicket = List.stripPrefix "http://sso.pokemon.com/sso/oauth2.0/callbackAuthorize?ticket="

    loginData =
      LoginRequest.create token "submit"
        (Profile.username profile)
        (Profile.password profile)


getAccessToken :: MonadResource m
               => Manager -> CookieJar -> String -> m BS.ByteString
getAccessToken manager cookieJar ticket = do
  let authData = AuthRequest.create ticket
  let req = urlEncodedBody (AuthRequest.toList authData) Config.loginOAuth { cookieJar = Just cookieJar }
  body <- LBS.toStrict . responseBody <$> httpLbs req manager
  case join . lookup "access_token" . URI.parseQuery $ body of
    Nothing    -> fail "access token"
    Just token -> return token


login :: MonadResource m
      => Profile -> Manager -> m Text
login profile manager = do
  liftIO $ putStrLn "[=] Retrieving login token"
  (cookieJar, loginToken, nextUrl) <- getLoginToken manager
  liftIO $ putStrLn "[=] Retrieving login ticket"
  ticket <- getLoginTicket profile manager cookieJar loginToken nextUrl
  liftIO $ putStrLn "[=] Retrieving access token"
  token <- Encoding.decodeUtf8 <$> getAccessToken manager cookieJar ticket
  liftIO $ putStrLn $ "[=] Token: " ++ show token
  return token


-- vim:sw=2
