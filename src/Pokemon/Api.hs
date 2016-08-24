{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
module Pokemon.Api
  ( Api
  , call
  , run
  ) where

import           Control.Applicative          ((<|>))
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.State          (StateT)
import qualified Control.Monad.State          as State
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString              as BS
import           Data.Default.Class           (def)
import qualified Data.Maybe                   as Maybe
import           Data.ProtoLens               (showMessage)
import qualified Data.Result                  as Result
import qualified Data.Text                    as Text
import           Data.Time.Clock              (NominalDiffTime)
import           Data.Time.Clock.POSIX        (getPOSIXTime)
import           Data.Word                    (Word64)
import           Lens.Family2                 ((^.))
import           Network.HTTP.Conduit         (Manager, Request, newManager,
                                               parseUrlThrow,
                                               tlsManagerSettings)
import           System.Random                (randomIO)

import qualified Pokemon.Config               as Config
import qualified Pokemon.Encrypt              as Encrypt
import qualified Pokemon.Envelope             as Envelope
import qualified Pokemon.Geolocation.GeoClue2 as GeoClue2
import qualified Pokemon.Geolocation.Geocode  as Geocode
import           Pokemon.Location             (Location)
import qualified Pokemon.Login                as Login
import qualified Pokemon.Network              as Network
import           Pokemon.Profile              (Profile)
import qualified Pokemon.Profile              as Profile
import qualified Pokemon.Proto                as Proto



data Context s = Context
  { _state    :: s
  , manager   :: Manager
  , endpoint  :: Request
  , nextId    :: Word64
  , sessionId :: Encrypt.SessionHash
  , location  :: Location
  , auth      :: Envelope.Auth
  , startTime :: NominalDiffTime
  }


newtype Api m s a = Api { unApi :: StateT (Context s) m a }
  deriving (Functor, Applicative, Monad, MonadIO)


getLocation :: String -> IO Location
getLocation address = do
  putStrLn "[=] Detecting location..."
  detectedLocation <- GeoClue2.detect
  putStrLn $ "[=] Detected location: " ++ show detectedLocation
  manualLocation <- Geocode.fromAddress address
  putStrLn $ "[=] Manual location: " ++ show manualLocation
  return $ Result.fromResult def $ detectedLocation <|> manualLocation


getApiUrl :: MonadResource m => Proto.ResponseEnvelope -> m (Maybe Request)
getApiUrl res =
  let apiUrl = res ^. Proto.apiUrl in
  if Text.null apiUrl
    then return Nothing
    else Just <$> parseUrlThrow ("https://" ++ Text.unpack apiUrl ++ "/rpc")


getAuthTicket :: Proto.ResponseEnvelope -> Maybe Envelope.Auth
getAuthTicket res =
  let ticket = res ^. Proto.authTicket in
  if ticket == def
    then Nothing
    else Just $ Envelope.AuthTicket ticket


rpcCall :: MonadResource m => [Proto.Request] -> StateT (Context s) m [BS.ByteString]
rpcCall reqs = do
  ctx@Context { manager, endpoint, nextId, sessionId, location, auth, startTime } <- State.get
  liftIO $ putStrLn $ "[=] Making RPC call with " ++ Envelope.authName auth

  now  <- liftIO getPOSIXTime
  iv   <- liftIO randomIO
  let envelope = Envelope.envelope nextId sessionId iv now startTime auth location reqs
  res <- Network.call manager endpoint envelope

  newEndpoint <- getApiUrl res

  State.put ctx
    { nextId   = nextId + fromIntegral (length reqs)
    , endpoint = Maybe.fromMaybe endpoint newEndpoint
    , auth     = Maybe.fromMaybe auth (getAuthTicket res)
    }

  case res ^. Proto.returns of
    [""]    -> rpcCall reqs -- retry
    returns -> return returns


call :: MonadResource m => [Proto.Request] -> Api m s [BS.ByteString]
call = Api . rpcCall


run :: MonadResource m => s -> Profile -> Api m s a -> m a
run s profile api = do
  manager <- liftIO $ newManager tlsManagerSettings
  ctx <- context manager
  fst <$> State.runStateT (unApi api) ctx
  where
    context manager = Context s manager Config.apiReq
      <$> liftIO randomIO
      <*> liftIO randomIO
      <*> liftIO (getLocation $ Profile.address profile)
      <*> (Envelope.AccessToken <$> Login.login profile manager)
      <*> liftIO getPOSIXTime


-- vim:sw=2
