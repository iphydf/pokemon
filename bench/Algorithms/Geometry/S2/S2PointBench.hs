module Algorithms.Geometry.S2.S2PointBench where

import           Criterion.Main

import           Algorithms.Geometry.S2.CPP.S2Point
import           Algorithms.Geometry.S2.S2Point     (S2Point)
import qualified Algorithms.Geometry.S2.S2Point     as S2Point


suite :: [Benchmark]
suite =
  [
  ]
{-
  equiv2 "sub" S2Point.sub c_S2Point_sub
  equiv2 "add" S2Point.add c_S2Point_add
  equiv2 "mul" S2Point.mul c_S2Point_mul
  equiv2 "div" S2Point.div c_S2Point_div
  equiv1 "neg" S2Point.neg c_S2Point_neg
  equiv2 "angle" S2Point.angle c_S2Point_angle
  equiv2 "crossProd" S2Point.crossProd c_S2Point_crossProd
  equiv2 "dotProd" S2Point.dotProd c_S2Point_dotProd
  equiv1 "norm2" S2Point.norm2 c_S2Point_norm2
  equiv1 "norm" S2Point.norm c_S2Point_norm
  equiv1 "fabs" S2Point.fabs c_S2Point_fabs
  equiv1 "largestAbsComponent" S2Point.largestAbsComponent c_S2Point_largestAbsComponent
  equiv1 "ortho" S2Point.ortho c_S2Point_ortho
-}
