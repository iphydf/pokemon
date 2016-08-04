module Algorithms.Geometry.S2.CPP.S2LatLng where

import           Data.Int                        (Int32)
import           Foreign.Ptr                     (Ptr)

import           Algorithms.Geometry.S2.S1Angle  (S1Angle)
import           Algorithms.Geometry.S2.S2LatLng (S2LatLng)
import qualified Algorithms.Geometry.S2.S2LatLng as S2LatLng
import           Algorithms.Geometry.S2.S2Point  (S2Point)


foreign import ccall c_S2LatLng_fromRadians       :: Ptr S2LatLng -> Ptr Double -> Ptr Double -> IO ()
foreign import ccall c_S2LatLng_fromDegrees       :: Ptr S2LatLng -> Ptr Double -> Ptr Double -> IO ()
foreign import ccall c_S2LatLng_fromLatLng        :: Ptr S2LatLng -> Ptr S1Angle -> Ptr S1Angle -> IO ()
foreign import ccall c_S2LatLng_fromPoint         :: Ptr S2LatLng -> Ptr S2Point -> IO ()
foreign import ccall c_S2LatLng_fromE5            :: Ptr S2LatLng -> Ptr Int32 -> Ptr Int32 -> IO ()
foreign import ccall c_S2LatLng_fromE6            :: Ptr S2LatLng -> Ptr Int32 -> Ptr Int32 -> IO ()
foreign import ccall c_S2LatLng_fromE7            :: Ptr S2LatLng -> Ptr Int32 -> Ptr Int32 -> IO ()
foreign import ccall c_S2LatLng_latitude          :: Ptr S1Angle -> Ptr S2Point -> IO ()
foreign import ccall c_S2LatLng_longitude         :: Ptr S1Angle -> Ptr S2Point -> IO ()
foreign import ccall c_S2LatLng_lat               :: Ptr S1Angle -> Ptr S2LatLng -> IO ()
foreign import ccall c_S2LatLng_lng               :: Ptr S1Angle -> Ptr S2LatLng -> IO ()
foreign import ccall c_S2LatLng_normalised        :: Ptr S2LatLng -> Ptr S2LatLng -> IO ()
foreign import ccall c_S2LatLng_toPoint           :: Ptr S2Point -> Ptr S2LatLng -> IO ()
foreign import ccall c_S2LatLng_getDistance       :: Ptr S1Angle -> Ptr S2LatLng -> Ptr S2LatLng -> IO ()
foreign import ccall c_S2LatLng_sub               :: Ptr S2LatLng -> Ptr S2LatLng -> Ptr S2LatLng -> IO ()
foreign import ccall c_S2LatLng_add               :: Ptr S2LatLng -> Ptr S2LatLng -> Ptr S2LatLng -> IO ()
foreign import ccall c_S2LatLng_mul               :: Ptr S2LatLng -> Ptr S2LatLng -> Ptr Double -> IO ()
