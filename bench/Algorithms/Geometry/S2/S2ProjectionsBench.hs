module Algorithms.Geometry.S2.S2ProjectionsBench where

import           Criterion.Main

import           Algorithms.Geometry.S2.CPP.S2Projections
import           Algorithms.Geometry.S2.Metric            (AreaMetric,
                                                           LengthMetric)
import           Algorithms.Geometry.S2.R2Vector          (R2Vector)
import           Algorithms.Geometry.S2.S2                (Face)
import           Algorithms.Geometry.S2.S2Point           (S2Point)
import           Algorithms.Geometry.S2.S2Projections     (Projection)
import qualified Algorithms.Geometry.S2.S2Projections     as S2Projections


suite :: [Benchmark]
suite =
  [
  ]
{-
  --equiv0 "sub" S2Projections.sub c_S2Projections_sub
  equiv0 "projection" S2Projections.projection c_S2Projections_projection
  equiv0 "maxEdgeAspect" S2Projections.maxEdgeAspect c_S2Projections_maxEdgeAspect
  equiv0 "maxDiagAspect" S2Projections.maxDiagAspect c_S2Projections_maxDiagAspect
  equiv1 "stToUV" S2Projections.stToUV c_S2Projections_stToUV
  equiv1 "uvToST" S2Projections.uvToST c_S2Projections_uvToST
  equiv3 "faceUvToXyz" S2Projections.faceUvToXyz c_S2Projections_faceUvToXyz
  equiv2 "faceXyzToUv" S2Projections.faceXyzToUv c_S2Projections_faceXyzToUv
  -- TODO: this causes assertion errors in C++
  --equiv1 "xyzToFace" S2Projections.xyzToFace c_S2Projections_xyzToFace
  equiv2 "getUNorm" S2Projections.getUNorm c_S2Projections_getUNorm
  equiv2 "getVNorm" S2Projections.getVNorm c_S2Projections_getVNorm
  equiv1 "getNorm" S2Projections.getNorm c_S2Projections_getNorm
  equiv1 "getUAxis" S2Projections.getUAxis c_S2Projections_getUAxis
  equiv1 "getVAxis" S2Projections.getVAxis c_S2Projections_getVAxis
-}
