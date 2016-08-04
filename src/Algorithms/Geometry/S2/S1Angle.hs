{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algorithms.Geometry.S2.S1Angle where

import           Control.DeepSeq                (NFData)
import           Data.AEq                       (AEq)
import           Data.Int                       (Int32)
import           Foreign.Storable               (Storable)
import           Test.QuickCheck.Arbitrary      (Arbitrary)

import           Algorithms.Geometry.S2.S2Point as S2Point


newtype S1Angle = S1Angle { radians :: Double }
  deriving (Eq, Ord, Read, Show, AEq, Arbitrary, Storable, NFData)


degrees :: S1Angle -> Double
degrees = (180 / pi *) . radians


e5, e6, e7 :: S1Angle -> Int32
e5 = truncate . (1e5 *) . degrees
e6 = truncate . (1e6 *) . degrees
e7 = truncate . (1e7 *) . degrees


fromRadians :: Double -> S1Angle
fromRadians = S1Angle


fromDegrees :: Double -> S1Angle
fromDegrees = S1Angle . (pi / 180 *)


fromE5, fromE6, fromE7 :: Int32 -> S1Angle
fromE5 = fromDegrees . (1e-5 *) . fromIntegral
fromE6 = fromDegrees . (1e-6 *) . fromIntegral
fromE7 = fromDegrees . (1e-7 *) . fromIntegral


between :: S2Point -> S2Point -> S1Angle
between p1 p2 = S1Angle $ S2Point.angle p1 p2
