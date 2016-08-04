module Algorithms.Geometry.S2.CPP.S2Point where

import           Data.Int                       (Int32)
import           Foreign.Ptr                    (Ptr)

import           Algorithms.Geometry.S2.S2Point (S2Point)
import qualified Algorithms.Geometry.S2.S2Point as S2Point


foreign import ccall c_S2Point_sub       :: Ptr S2Point -> Ptr S2Point -> Ptr S2Point -> IO ()
foreign import ccall c_S2Point_add       :: Ptr S2Point -> Ptr S2Point -> Ptr S2Point -> IO ()
foreign import ccall c_S2Point_mul       :: Ptr S2Point -> Ptr S2Point -> Ptr Double -> IO ()
foreign import ccall c_S2Point_div       :: Ptr S2Point -> Ptr S2Point -> Ptr Double -> IO ()
foreign import ccall c_S2Point_neg       :: Ptr S2Point -> Ptr S2Point -> IO ()
foreign import ccall c_S2Point_angle     :: Ptr Double -> Ptr S2Point -> Ptr S2Point -> IO ()
foreign import ccall c_S2Point_crossProd :: Ptr S2Point -> Ptr S2Point -> Ptr S2Point -> IO ()
foreign import ccall c_S2Point_dotProd   :: Ptr Double -> Ptr S2Point -> Ptr S2Point -> IO ()
foreign import ccall c_S2Point_norm2     :: Ptr Double -> Ptr S2Point -> IO ()
foreign import ccall c_S2Point_norm      :: Ptr Double -> Ptr S2Point -> IO ()
foreign import ccall c_S2Point_normalise :: Ptr S2Point -> Ptr S2Point -> IO ()
foreign import ccall c_S2Point_fabs      :: Ptr S2Point -> Ptr S2Point -> IO ()
foreign import ccall c_S2Point_largestAbsComponent :: Ptr Int -> Ptr S2Point -> IO ()
foreign import ccall c_S2Point_ortho     :: Ptr S2Point -> Ptr S2Point -> IO ()
