module Algorithms.Geometry.S2.S1AngleSpec where

import           Data.Int                           (Int32)
import           Foreign.Ptr                        (Ptr)
import           Test.CppEquivalence
import           Test.Hspec
import           Test.QuickCheck

import           Algorithms.Geometry.S2.CPP.S1Angle
import           Algorithms.Geometry.S2.S1Angle     (S1Angle)
import qualified Algorithms.Geometry.S2.S1Angle     as S1Angle
import           Algorithms.Geometry.S2.S2Point     (S2Point)


spec :: Spec
spec = do
  equiv1 "fromDegrees" S1Angle.fromDegrees c_S1Angle_fromDegrees
  equiv1 "fromRadians" S1Angle.fromRadians c_S1Angle_fromRadians
  equiv1 "fromE5" S1Angle.fromE5 c_S1Angle_fromE5
  equiv1 "fromE6" S1Angle.fromE6 c_S1Angle_fromE6
  equiv1 "fromE7" S1Angle.fromE7 c_S1Angle_fromE7
  equiv1 "degrees" S1Angle.degrees c_S1Angle_degrees
  equiv1 "radians" S1Angle.radians c_S1Angle_radians
  equiv2 "between" S1Angle.between c_S1Angle_between
