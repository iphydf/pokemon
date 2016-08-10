module Algorithms.Geometry.S2.S2CellIdSpec where

import           Data.Word                           (Word32, Word64)
import           Foreign.Cpp                         (ffi1)
import           Foreign.Ptr                         (Ptr)
import           Test.CppEquivalence                 (equiv1, equiv2, equiv3)
import           Test.Hspec
import           Test.QuickCheck

import           Algorithms.Geometry.S2.CPP.S2CellId
import           Algorithms.Geometry.S2.S2           (Face, Level)
import           Algorithms.Geometry.S2.S2CellId     (S2CellId (..))
import qualified Algorithms.Geometry.S2.S2CellId     as S2CellId
import           Algorithms.Geometry.S2.S2LatLng     (S2LatLng)
import           Algorithms.Geometry.S2.S2Point      (S2Point)


spec :: Spec
spec = do
  equiv1 "lsb" S2CellId.lsb c_S2CellId_lsb
  equiv1 "lsbForLevel" S2CellId.lsbForLevel c_S2CellId_lsbForLevel
  equiv1 "parent" S2CellId.parent c_S2CellId_parent
  equiv2 "parentForLevel" S2CellId.parentForLevel c_S2CellId_parentForLevel
  equiv3 "fromFacePosLevel" S2CellId.fromFacePosLevel c_S2CellId_fromFacePosLevel
  equiv3 "fromFaceIJ" S2CellId.fromFaceIJ c_S2CellId_fromFaceIJ
  equiv1 "fromPoint" S2CellId.fromPoint c_S2CellId_fromPoint
  equiv1 "fromLatLng" S2CellId.fromLatLng c_S2CellId_fromLatLng
  equiv1 "level" S2CellId.level c_S2CellId_level

  describe "level" $
    it "behaves correctly for lower half zero" $ do
      let cid = S2CellId 0x1234567800000000
      expected <- ffi1 c_S2CellId_level cid
      S2CellId.level cid `shouldBe` expected
