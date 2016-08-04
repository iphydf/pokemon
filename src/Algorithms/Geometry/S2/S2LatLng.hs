{-# LANGUAGE NamedFieldPuns #-}
module Algorithms.Geometry.S2.S2LatLng where

import           Data.AEq                       (AEq, (~==))
import           Data.Int                       (Int32)
import           Foreign.Ptr                    (castPtr)
import           Foreign.Storable               (Storable (..))
import           Test.QuickCheck.Arbitrary      (Arbitrary (..))
import qualified Test.QuickCheck.Gen            as Gen

import           Algorithms.Geometry.S2.CMath
import           Algorithms.Geometry.S2.S1Angle as S1Angle
import           Algorithms.Geometry.S2.S2Point as S2Point


data S2LatLng = S2LatLng
  { latRadians :: Double
  , lngRadians :: Double
  }
  deriving (Eq, Read, Show)

instance AEq S2LatLng where
  a ~== b =
    latRadians a ~== latRadians b &&
    lngRadians a ~== lngRadians b

instance Arbitrary S2LatLng where
  arbitrary = S2LatLng
    <$> arbitrary
    <*> arbitrary

instance Storable S2LatLng where
  sizeOf _ = sizeOf (latRadians undefined) * 2
  alignment _ = alignment (latRadians undefined)

  peek ptr = S2LatLng
    <$> peekElemOff (castPtr ptr) 0
    <*> peekElemOff (castPtr ptr) 1

  poke ptr p = do
    pokeElemOff (castPtr ptr) 0 (latRadians p)
    pokeElemOff (castPtr ptr) 1 (lngRadians p)


-- | Approximate "effective" radius of the Earth in meters.
earthRadius :: Double
earthRadius = 6367000


fromRadians :: Double -> Double -> S2LatLng
fromRadians = S2LatLng


fromLatLng :: S1Angle -> S1Angle -> S2LatLng
fromLatLng latAngle lngAngle = S2LatLng
  (S1Angle.radians latAngle)
  (S1Angle.radians lngAngle)


fromDegrees :: Double -> Double -> S2LatLng
fromDegrees latDeg lngDeg = fromLatLng
  (S1Angle.fromDegrees latDeg)
  (S1Angle.fromDegrees lngDeg)


fromE5 :: Int32 -> Int32 -> S2LatLng
fromE5 latE5 lngE5 = fromLatLng
  (S1Angle.fromE5 latE5)
  (S1Angle.fromE5 lngE5)


fromE6 :: Int32 -> Int32 -> S2LatLng
fromE6 latE6 lngE6 = fromLatLng
  (S1Angle.fromE6 latE6)
  (S1Angle.fromE6 lngE6)


fromE7 :: Int32 -> Int32 -> S2LatLng
fromE7 latE7 lngE7 = fromLatLng
  (S1Angle.fromE7 latE7)
  (S1Angle.fromE7 lngE7)


latitude :: S2Point -> S1Angle
latitude S2Point { x, y, z } =
  -- We use atan2 rather than asin because the input vector is not necessarily
  -- unit length, and atan2 is much more accurate than asin near the poles.
  S1Angle.fromRadians
    (atan2 z $ sqrt $ x * x + y * y)


longitude :: S2Point -> S1Angle
longitude S2Point { x, y } =
  -- Note that atan2(0, 0) is defined to be zero.
  S1Angle.fromRadians $ atan2 y x


fromPoint :: S2Point -> S2LatLng
fromPoint S2Point { x, y, z } =
  S2LatLng
    (atan2 z $ sqrt $ x * x + y * y)
    (atan2 y x)


lat :: S2LatLng -> S1Angle
lat = S1Angle.fromRadians . latRadians


latDegrees :: S2LatLng -> Double
latDegrees = (180 / pi *) . latRadians


lng :: S2LatLng -> S1Angle
lng = S1Angle.fromRadians . lngRadians


lngDegrees :: S2LatLng -> Double
lngDegrees = (180 / pi *) . lngRadians


isValid :: S2LatLng -> Bool
isValid p =
  abs (S1Angle.radians $ lat p) <= pi / 2 &&
  abs (S1Angle.radians $ lng p) <= pi


normalised :: S2LatLng -> S2LatLng
normalised p = S2LatLng
  (max (-pi / 2) (min (pi / 2) (S1Angle.radians $ lat p)))
  (remainder (S1Angle.radians $ lng p) (pi * 2))


toPoint :: S2LatLng -> S2Point
toPoint p =
  let
    phi    = (S1Angle.radians $ lat p)
    theta  = (S1Angle.radians $ lng p)
    cosphi = cos phi
  in
  S2Point
    (cos theta * cosphi)
    (sin theta * cosphi)
    (sin phi)


-- | Return the distance (measured along the surface of the sphere) to the given
-- point.
--
-- This implements the Haversine formula, which is numerically stable for
-- small distances but only gets about 8 digits of precision for very large
-- distances (e.g. antipodal points). Note that 8 digits is still accurate
-- to within about 10cm for a sphere the size of the Earth.
--
-- This could be fixed with another sin() and cos() below, but at that point
-- you might as well just convert both arguments to S2Points and compute the
-- distance that way (which gives about 15 digits of accuracy for all
-- distances).
--
-- Return the distance (measured along the surface of the sphere) to the
-- given S2LatLng. This is mathematically equivalent to:
--
-- S1Angle.fromRadians $ S2Point.angle (toPoint p1) (S2Point.toPoint p2)
--
-- but this implementation is slightly more efficient.
getDistance :: S2LatLng -> S2LatLng -> S1Angle
getDistance p1 p2 =
  let
    lat1 = (S1Angle.radians $ lat p1)
    lat2 = (S1Angle.radians $ lat p2)
    lng1 = (S1Angle.radians $ lng p1)
    lng2 = (S1Angle.radians $ lng p2)
    dlat = sin (0.5 * (lat2 - lat1))
    dlng = sin (0.5 * (lng2 - lng1))
    x = dlat * dlat + dlng * dlng * cos lat1 * cos lat2
  in
  S1Angle.fromRadians
    (2 * atan2 (sqrt x) (sqrt (max 0 (1.0 - x))))


-- | Returns the surface distance to the given point assuming a constant radius.
getDistanceRadius :: Double -> S2LatLng -> S2LatLng -> Double
getDistanceRadius radius p1 p2 =
  S1Angle.radians (getDistance p1 p2) * radius


-- | Returns the surface distance to the given point assuming the default Earth
-- radius of 'earthRadius'.
getEarthDistance :: S2LatLng -> S2LatLng -> Double
getEarthDistance = getDistanceRadius earthRadius


add :: S2LatLng -> S2LatLng -> S2LatLng
add p1 p2 = S2LatLng
  (latRadians p1 + latRadians p2)
  (lngRadians p1 + lngRadians p2)


sub :: S2LatLng -> S2LatLng -> S2LatLng
sub p1 p2 = S2LatLng
  (latRadians p1 - latRadians p2)
  (lngRadians p1 - lngRadians p2)


mul :: S2LatLng -> Double -> S2LatLng
mul S2LatLng { latRadians, lngRadians } m = S2LatLng
  (latRadians * m)
  (lngRadians * m)


-- | Returns true if both the latitude and longitude of the given point are
-- within 'maxError' radians of this point.
approxEquals :: Double -> S2LatLng -> S2LatLng -> Bool
approxEquals maxError p1 p2 =
  abs (latRadians p1 - latRadians p2) < maxError &&
  abs (lngRadians p1 - lngRadians p2) < maxError


-- | Returns true if the given point is within {@code 1e-9} radians of this
-- point. This corresponds to a distance of less than {@code 1cm} at the
-- surface of the Earth.
approxEquals1cm :: S2LatLng -> S2LatLng -> Bool
approxEquals1cm = approxEquals 1e-9
