module Main (main) where

import           Criterion.Main

import qualified Algorithms.Geometry.S2.S1AngleBench       as S1AngleBench
import qualified Algorithms.Geometry.S2.S2CellIdBench      as S2CellIdBench
import qualified Algorithms.Geometry.S2.S2LatLngBench      as S2LatLngBench
import qualified Algorithms.Geometry.S2.S2PointBench       as S2PointBench
import qualified Algorithms.Geometry.S2.S2ProjectionsBench as S2ProjectionsBench


main :: IO ()
main = defaultMain
  [ bgroup "S1Angle" S1AngleBench.suite
  , bgroup "S2CellId" S2CellIdBench.suite
  , bgroup "S2LatLng" S2LatLngBench.suite
  , bgroup "S2Point" S2PointBench.suite
  , bgroup "S2Projections" S2ProjectionsBench.suite
  ]
