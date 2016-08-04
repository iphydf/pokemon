{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algorithms.Geometry.S2.S2 where

import           Data.AEq                  (AEq)
import           Data.Bits                 (shiftL)
import           Data.Int                  (Int32)
import           Foreign.Storable          (Storable)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))
import qualified Test.QuickCheck.Gen       as Gen


newtype Face = Face { unFace :: Int }
  deriving (Eq, Read, Show, Storable, AEq)

instance Arbitrary Face where
  arbitrary = Face <$> Gen.elements [0..5]


newtype Level = Level { unLevel :: Int }
  deriving (Eq, Read, Show, Storable, AEq)

instance Arbitrary Level where
  arbitrary = Level <$> Gen.elements [0..maxCellLevel]


-- | This is the number of levels needed to specify a leaf cell. This
-- constant is defined here so that the S2::Metric class can be
-- implemented without including s2cellid.h.
maxCellLevel :: Int
maxCellLevel = 30 -- Valid levels: 0..maxCellLevel

maxSize :: Int32
maxSize = 1 `shiftL` maxCellLevel
