module Algorithms.Geometry.S2.CPP.S1Angle where

import           Data.Int                       (Int32)
import           Foreign.Ptr                    (Ptr)

import           Algorithms.Geometry.S2.S1Angle (S1Angle)
import qualified Algorithms.Geometry.S2.S1Angle as S1Angle
import           Algorithms.Geometry.S2.S2Point (S2Point)


foreign import ccall c_S1Angle_fromDegrees :: Ptr S1Angle -> Ptr Double                 -> IO ()
foreign import ccall c_S1Angle_fromRadians :: Ptr S1Angle -> Ptr Double                 -> IO ()
foreign import ccall c_S1Angle_fromE5      :: Ptr S1Angle -> Ptr Int32                  -> IO ()
foreign import ccall c_S1Angle_fromE6      :: Ptr S1Angle -> Ptr Int32                  -> IO ()
foreign import ccall c_S1Angle_fromE7      :: Ptr S1Angle -> Ptr Int32                  -> IO ()
foreign import ccall c_S1Angle_degrees     :: Ptr Double  -> Ptr S1Angle                -> IO ()
foreign import ccall c_S1Angle_radians     :: Ptr Double  -> Ptr S1Angle                -> IO ()
foreign import ccall c_S1Angle_between     :: Ptr S1Angle -> Ptr S2Point -> Ptr S2Point -> IO ()
