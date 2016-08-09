module Algorithms.Geometry.S2.S2CapSpec where

import           Control.Monad                          (unless)
import           Data.Default.Class                     (def)
import           Data.Int                               (Int32)
import qualified Data.Maybe                             as Maybe
import           Foreign.Ptr                            (Ptr)
import           System.Environment                     (lookupEnv)
import           Test.CppEquivalence
import           Test.Hspec
import           Test.QuickCheck

import           Algorithms.Geometry.S2.CPP.S2Cap
import qualified Algorithms.Geometry.S2.S1Angle         as S1Angle
import           Algorithms.Geometry.S2.S2Cap           (S2Cap)
import qualified Algorithms.Geometry.S2.S2Cap           as S2Cap
import qualified Algorithms.Geometry.S2.S2LatLng        as S2LatLng
import           Algorithms.Geometry.S2.S2Point         (S2Point)
import qualified Algorithms.Geometry.S2.S2RegionCoverer as S2RegionCoverer


spec :: Spec
spec =
  it "does the right thing" $ do
    let lat = 51.507335
    let lng = -0.127689
    let radius = 1000
    let region = S2Cap.fromAxisAngle (S2LatLng.toPoint $ S2LatLng.fromDegrees lat lng) (S1Angle.fromDegrees (360 * radius / (2 * pi * S2LatLng.earthRadius)))
    let coverer = def { S2RegionCoverer.minLevel = 15, S2RegionCoverer.maxLevel = 15 }
    let coverage = S2RegionCoverer.getCovering coverer region
    travis <- Maybe.isJust <$> lookupEnv "TRAVIS"
    unless travis $
      print coverage
{-
  equiv1 "fromDegrees" S2Cap.fromDegrees c_S2Cap_fromDegrees
  equiv1 "fromRadians" S2Cap.fromRadians c_S2Cap_fromRadians
  equiv1 "fromE5" S2Cap.fromE5 c_S2Cap_fromE5
  equiv1 "fromE6" S2Cap.fromE6 c_S2Cap_fromE6
  equiv1 "fromE7" S2Cap.fromE7 c_S2Cap_fromE7
  equiv1 "degrees" S2Cap.degrees c_S2Cap_degrees
  equiv1 "radians" S2Cap.radians c_S2Cap_radians
  equiv2 "between" S2Cap.between c_S2Cap_between
-}
