module Algorithms.Geometry.S2.CPP.S2Cap where

import           Data.Int                       (Int32)
import           Foreign.Ptr                    (Ptr)

import           Algorithms.Geometry.S2.S2Cap   (S2Cap)
import qualified Algorithms.Geometry.S2.S2Cap   as S2Cap
import           Algorithms.Geometry.S2.S2Point (S2Point)


{-
foreign import ccall c_S2Cap_fromDegrees :: Ptr S2Cap -> Ptr Double                 -> IO ()
foreign import ccall c_S2Cap_fromRadians :: Ptr S2Cap -> Ptr Double                 -> IO ()
foreign import ccall c_S2Cap_fromE5      :: Ptr S2Cap -> Ptr Int32                  -> IO ()
foreign import ccall c_S2Cap_fromE6      :: Ptr S2Cap -> Ptr Int32                  -> IO ()
foreign import ccall c_S2Cap_fromE7      :: Ptr S2Cap -> Ptr Int32                  -> IO ()
foreign import ccall c_S2Cap_degrees     :: Ptr Double  -> Ptr S2Cap                -> IO ()
foreign import ccall c_S2Cap_radians     :: Ptr Double  -> Ptr S2Cap                -> IO ()
foreign import ccall c_S2Cap_between     :: Ptr S2Cap -> Ptr S2Point -> Ptr S2Point -> IO ()
-}
