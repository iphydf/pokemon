{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pokemon.Location where

import           Data.Binary         (Binary (..))
import           Data.Binary.IEEE754 (getFloat64be, putFloat64be)
import           Data.Default.Class  (Default (..))


data Location = Location
  { description :: String
  , accuracy    :: Double
  , latitude    :: Double
  , longitude   :: Double
  , altitude    :: Double
  }
  deriving (Show)


instance Binary Location where
  put Location { latitude, longitude, altitude } = do
    putFloat64be latitude
    putFloat64be longitude
    putFloat64be altitude

  get = fromLatLngAlt
    <$> getFloat64be
    <*> getFloat64be
    <*> getFloat64be


instance Default Location where
  def = Location
    -- Somewhere in London
    { description = "London, UK"
    , accuracy    = 10
    , latitude    = 51.507351
    , longitude   = -0.127758
    , altitude    = 14 -- Elevation in London.
    }


fromLatLngAlt :: Double -> Double -> Double -> Location
fromLatLngAlt lat lng alt = Location
  { description = "<code>"
  , accuracy    = 0
  , latitude    = lat
  , longitude   = lng
  , altitude    = alt
  }


fromLatLngAcc :: Double -> Double -> Double -> Location
fromLatLngAcc lat lng acc = Location
  { description = "<code>"
  , accuracy    = acc
  , latitude    = lat
  , longitude   = lng
  , altitude    = 0
  }


-- vim:sw=2
