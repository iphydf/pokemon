module Algorithms.Geometry.S2.S2LatLngBench where

import           Criterion.Main

import           Algorithms.Geometry.S2.CPP.S2LatLng
import           Algorithms.Geometry.S2.S1Angle      (S1Angle)
import           Algorithms.Geometry.S2.S2LatLng     (S2LatLng)
import qualified Algorithms.Geometry.S2.S2LatLng     as S2LatLng
import           Algorithms.Geometry.S2.S2Point      (S2Point)


suite :: [Benchmark]
suite =
  [
  ]
{-
  equiv2 "fromRadians" S2LatLng.fromRadians c_S2LatLng_fromRadians
  equiv2 "fromDegrees" S2LatLng.fromDegrees c_S2LatLng_fromDegrees
  equiv2 "fromLatLng" S2LatLng.fromLatLng c_S2LatLng_fromLatLng
  equiv1 "fromPoint" S2LatLng.fromPoint c_S2LatLng_fromPoint
  equiv2 "fromE5" S2LatLng.fromE5 c_S2LatLng_fromE5
  equiv2 "fromE6" S2LatLng.fromE6 c_S2LatLng_fromE6
  equiv2 "fromE7" S2LatLng.fromE7 c_S2LatLng_fromE7
  equiv1 "latitude" S2LatLng.latitude c_S2LatLng_latitude
  equiv1 "longitude" S2LatLng.longitude c_S2LatLng_longitude
  equiv1 "lat" S2LatLng.lat c_S2LatLng_lat
  equiv1 "lng" S2LatLng.lng c_S2LatLng_lng
  equiv1 "normalised" S2LatLng.normalised c_S2LatLng_normalised
  equiv1 "toPoint" S2LatLng.toPoint c_S2LatLng_toPoint
  equiv2 "getDistance" S2LatLng.getDistance c_S2LatLng_getDistance
  equiv2 "sub" S2LatLng.sub c_S2LatLng_sub
  equiv2 "add" S2LatLng.add c_S2LatLng_add
  equiv2 "mul" S2LatLng.mul c_S2LatLng_mul
-}
