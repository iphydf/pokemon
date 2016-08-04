module Algorithms.Geometry.S2.S2CellIdBench where

import           Criterion.Main
import           Data.Word                            (Word64)
import           Foreign.Cpp

import           Algorithms.Geometry.S2.CPP.S2CellId
import           Algorithms.Geometry.S2.S2            (Face, Level, unFace)
import           Algorithms.Geometry.S2.S2CellId      (S2CellId)
import qualified Algorithms.Geometry.S2.S2CellId      as S2CellId
import qualified Algorithms.Geometry.S2.S2LatLng      as S2LatLng
import           Algorithms.Geometry.S2.S2Point       (S2Point)
import qualified Algorithms.Geometry.S2.S2Projections as S2Projections


foreign import ccall optc_S2CellId_fromFaceIJ :: Int -> Int -> Int -> Word64


suite :: [Benchmark]
suite =
  let p = S2LatLng.fromDegrees 80 44 in
  let (face, i, j) = S2Projections.xyzToFaceIJ $ S2LatLng.toPoint p in

  [ bench "fromFaceIJ (C++/opt)" $ nf (optc_S2CellId_fromFaceIJ (unFace face) i) j
  , bench "fromFaceIJ (C++)" $ nfIO (ffi3 c_S2CellId_fromFaceIJ face i j)
  , bench "fromFaceIJ" $ nf (S2CellId.fromFaceIJ face i) j
  , bench "fromLatLng (C++)" $ nfIO (ffi1 c_S2CellId_fromLatLng p)
  , bench "fromLatLng" $ nf S2CellId.fromLatLng p
  ]
{-
  equiv1 "lsb" S2CellId.lsb c_S2CellId_lsb
  equiv1 "lsbForLevel" S2CellId.lsbForLevel c_S2CellId_lsbForLevel
  equiv1 "parent" S2CellId.parent c_S2CellId_parent
  equiv2 "parentForLevel" S2CellId.parentForLevel c_S2CellId_parentForLevel
  equiv3 "fromFacePosLevel" S2CellId.fromFacePosLevel c_S2CellId_fromFacePosLevel
  equiv3 "fromFaceIJ" S2CellId.fromFaceIJ c_S2CellId_fromFaceIJ
  equiv1 "fromPoint" S2CellId.fromPoint c_S2CellId_fromPoint
  equiv1 "fromLatLng" S2CellId.fromLatLng c_S2CellId_fromLatLng
-}
