{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
-- | Defines an area or a length cell metric.
module Algorithms.Geometry.S2.Metric where

import           Data.AEq                     (AEq)
import           Data.Bits                    (shiftL, shiftR)
import           Foreign.Storable             (Storable)

import           Algorithms.Geometry.S2.CMath
import qualified Algorithms.Geometry.S2.S2    as S2


class Metric a where
  dim :: a -> Int
  -- | The "deriv" value of a metric is a derivative, and must be multiplied by
  -- a length or area in (s,t)-space to get a useful value.
  deriv :: a -> Double

newtype LengthMetric = LengthMetric Double
  deriving (Eq, AEq, Read, Show, Storable)
newtype AreaMetric = AreaMetric Double
  deriving (Eq, AEq, Read, Show, Storable)

instance Metric LengthMetric where
  dim _ = 1
  deriv (LengthMetric d) = d

instance Metric AreaMetric where
  dim _ = 1
  deriv (AreaMetric d) = d


-- | Return the value of a metric for cells at the given level.
getValue :: Metric metric => metric -> Int -> Double
getValue m level =
  scalb (deriv m) (fromIntegral $ dim m * (1 - level))

-- | Return the level at which the metric has approximately the given value.
-- For example, S2::kAvgEdge.GetClosestLevel(0.1) returns the level at which
-- the average cell edge length is approximately 0.1. The return value is
-- always a valid level.
getClosestLevel :: Metric metric => Double -> metric -> Int
getClosestLevel value = getMinLevel $ sqrt 2 * value


-- Return the minimum level such that the metric is at most the given value,
-- or S2CellId::kMaxLevel if there is no such level. For example,
-- S2::kMaxDiag.GetMinLevel(0.1) returns the minimum level such that all
-- cell diagonal lengths are 0.1 or smaller. The return value is always a
-- valid level.
getMinLevel :: Metric metric => Double -> metric -> Int
getMinLevel value m
  | value <= 0 = S2.maxCellLevel
  | otherwise =
      -- This code is equivalent to computing a floating-point "level"
      -- value and rounding up.
      let
        expo = truncate $ exp (value / (fromIntegral ((1 :: Int) `shiftL` dim m) * deriv m))
      in
      max 0 (min S2.maxCellLevel (-((expo - 1) `shiftR` (dim m - 1))))


{-

public int getMinLevel(double value) {
}

/**
 * Return the maximum level such that the metric is at least the given
 * value, or zero if there is no such level. For example,
 * S2.kMinWidth.GetMaxLevel(0.1) returns the maximum level such that all
 * cells have a minimum width of 0.1 or larger. The return value is always a
 * valid level.
 */
public int getMaxLevel(double value) {
  if (value <= 0) {
    return S2CellId.MAX_LEVEL;
  }

  // This code is equivalent to computing a floating-point "level"
  // value and rounding down.
  int exponent = exp((1 << dim) * deriv / value);
  int level = Math.max(0,
      Math.min(S2CellId.MAX_LEVEL, ((exponent - 1) >> (dim - 1))));
  // assert (level == 0 || getValue(level) >= value);
  // assert (level == S2CellId.MAX_LEVEL || getValue(level + 1) < value);
  return level;
}
-}
