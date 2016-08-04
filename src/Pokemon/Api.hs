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


rpcCall :: MonadResource m => [Proto.Request] -> StateT (Context s) m [BS.ByteString]
rpcCall reqs = do
  ctx@Context { manager, endpoint, nextId, location, auth, startTime } <- State.get
  liftIO $ putStrLn $ "[=] Making RPC call with " ++ Envelope.authName auth

  now <- liftIO getPOSIXTime
  uk22 <- liftIO randomIO
  iv <- liftIO randomIO
  let envelope = Envelope.envelope nextId uk22 iv now startTime auth location reqs
  res <- Network.call manager endpoint envelope

  newEndpoint <- parseUrlThrow $ "https://" ++ Text.unpack (res ^. Proto.apiUrl) ++ "/rpc"
  let newAuthTicket = res ^. Proto.authTicket

  State.put ctx
    { endpoint = newEndpoint
    , nextId   = nextId + fromIntegral (length reqs)
    , auth     = Envelope.AuthTicket newAuthTicket
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
      <*> liftIO (getLocation $ Profile.address profile)
      <*> (Envelope.AccessToken <$> Login.login profile manager)
      <*> liftIO getPOSIXTime


-- vim:sw=2
