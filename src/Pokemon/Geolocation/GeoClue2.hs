{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pokemon.Geolocation.GeoClue2 where

import qualified Control.Concurrent.STM as STM
import           Control.Monad          (void)
import           Control.Monad.Catch    (SomeException, catch)
import           Data.Default.Class     (def)
import qualified Data.List              as List
import qualified Data.Maybe             as Maybe
import           DBus                   (IsVariant, MethodCall, MethodReturn,
                                         ObjectPath, Signal, formatObjectPath,
                                         fromVariant, methodCall,
                                         methodCallBody, methodCallDestination,
                                         methodReturnBody, signalBody,
                                         toVariant)
import           DBus.Client            (Client, MatchRule, addMatch, call_,
                                         connectSystem, matchAny,
                                         matchInterface, matchMember, matchPath,
                                         removeMatch)
import           Safe                   (headMay)

import           Pokemon.Location       (Location (..))


callGeoClue2 :: Client -> MethodCall -> IO MethodReturn
callGeoClue2 system mth =
  call_ system mth
    { methodCallDestination = Just "org.freedesktop.GeoClue2" }


getClient :: Client -> IO ObjectPath
getClient system = do
  reply <- callGeoClue2 system (methodCall
      "/org/freedesktop/GeoClue2/Manager"
      "org.freedesktop.GeoClue2.Manager"
      "GetClient")

  case headMay (methodReturnBody reply) >>= fromVariant of
    Nothing        -> fail "bad reply"
    Just geoClient -> return geoClient


startClient :: Client -> ObjectPath -> IO ()
startClient system client =
  void $ callGeoClue2 system (methodCall
      client
      "org.freedesktop.GeoClue2.Client"
      "Start")


getProperty :: IsVariant a => Client -> ObjectPath -> String -> IO a
getProperty system loc prop = do
  reply <- callGeoClue2 system (methodCall
      loc
      "org.freedesktop.DBus.Properties"
      "Get")
    { methodCallBody =
        [ toVariant ("org.freedesktop.GeoClue2.Location" :: String)
        , toVariant prop
        ]
    }

  case headMay (methodReturnBody reply) >>= fromVariant >>= fromVariant of
    Nothing    -> fail $ "bad reply for property " ++ prop ++ ": " ++ show reply
    Just value -> return value


getLocation :: Client -> ObjectPath -> IO Location
getLocation system loc =
  Location
    <$> get "Description"
    <*> get "Accuracy"
    <*> get "Latitude"
    <*> get "Longitude"
    <*> pure (altitude def)
  where
    get :: IsVariant a => String -> IO a
    get = getProperty system loc


locationHandler :: Client -> STM.TVar (Maybe Location) -> Signal -> IO ()
locationHandler system ready sig = do
  reply <- mapM (getLocation system) locations
  STM.atomically (STM.writeTVar ready (headMay reply))
  where
    locations =
      filter (List.isInfixOf "/Location/" . formatObjectPath)
      . Maybe.mapMaybe fromVariant
      . signalBody
      $ sig


matchLocationUpdated :: ObjectPath -> MatchRule
matchLocationUpdated client = matchAny
  { matchPath = Just client
  , matchInterface = Just "org.freedesktop.GeoClue2.Client"
  , matchMember = Just "LocationUpdated"
  }


detectGeoClue :: IO Location
detectGeoClue = do
  system <- connectSystem
  client <- getClient system
  startClient system client
  ready <- STM.newTVarIO Nothing
  handler <- addMatch system
      (matchLocationUpdated client)
      (locationHandler system ready)
  location <- STM.atomically $ do
    done <- STM.readTVar ready
    case done of
      Nothing  -> STM.retry
      Just loc -> return loc
  removeMatch system handler
  return location


detect :: Monad m => IO (m Location)
detect =
  fmap return detectGeoClue
  `catch`
  \(e :: SomeException) ->
    return . fail . show $ e


-- vim:sw=2
