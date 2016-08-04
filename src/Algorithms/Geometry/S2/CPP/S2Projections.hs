module Algorithms.Geometry.S2.CPP.S2Projections where

import           Data.Int                             (Int32)
import           Foreign.Ptr                          (Ptr)

import           Algorithms.Geometry.S2.Metric        (AreaMetric, LengthMetric)
import           Algorithms.Geometry.S2.R2Vector      (R2Vector)
import           Algorithms.Geometry.S2.S2            (Face)
import           Algorithms.Geometry.S2.S2Point       (S2Point)
import           Algorithms.Geometry.S2.S2Projections (Projection)
import qualified Algorithms.Geometry.S2.S2Projections as S2Projections


foreign import ccall c_S2Projections_projection         :: Ptr Projection -> IO ()
foreign import ccall c_S2Projections_maxEdgeAspect      :: Ptr Double -> IO ()
foreign import ccall c_S2Projections_maxDiagAspect      :: Ptr Double -> IO ()
foreign import ccall c_S2Projections_stToUV             :: Ptr Double -> Ptr Double -> IO ()
foreign import ccall c_S2Projections_uvToST             :: Ptr Double -> Ptr Double -> IO ()
foreign import ccall c_S2Projections_faceUvToXyz        :: Ptr S2Point -> Ptr Double -> Ptr Double -> Ptr Face -> IO ()
foreign import ccall c_S2Projections_faceXyzToUv        :: Ptr (Maybe R2Vector) -> Ptr S2Point -> Ptr Face -> IO ()
foreign import ccall c_S2Projections_xyzToFace          :: Ptr Face -> Ptr S2Point -> IO ()
foreign import ccall c_S2Projections_getUNorm           :: Ptr S2Point -> Ptr Double -> Ptr Face -> IO ()
foreign import ccall c_S2Projections_getVNorm           :: Ptr S2Point -> Ptr Double -> Ptr Face -> IO ()
foreign import ccall c_S2Projections_getNorm            :: Ptr S2Point -> Ptr Face -> IO ()
foreign import ccall c_S2Projections_getUAxis           :: Ptr S2Point -> Ptr Face -> IO ()
foreign import ccall c_S2Projections_getVAxis           :: Ptr S2Point -> Ptr Face -> IO ()
