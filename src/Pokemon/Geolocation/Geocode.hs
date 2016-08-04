{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pokemon.Geolocation.Geocode where

import           Control.Monad.Catch        (SomeException, catch)
import           Data.Default.Class         (def)
import           Geography.Geocoding.Google (geoDecode, geoEncode)
import           Pokemon.Location           (Location (..))


fromAddressGeocode :: String -> IO Location
fromAddressGeocode a = do
  (lat, lng) <- getRight =<< geoEncode a
  address    <- getRight =<< geoDecode (lat, lng)
  return Location
    { description = address
    , accuracy    = 0
    , latitude    = lat
    , longitude   = lng
    , altitude    = altitude def
    }
  where
    getRight (Left err) = fail err
    getRight (Right ok) = return ok


fromAddress :: Monad m => String -> IO (m Location)
fromAddress a =
  fmap return (fromAddressGeocode a)
  `catch`
  \(e :: SomeException) ->
    return . fail . show $ e


-- vim:sw=2
