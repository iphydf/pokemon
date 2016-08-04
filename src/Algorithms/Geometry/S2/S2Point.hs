{-# LANGUAGE NamedFieldPuns #-}
module Algorithms.Geometry.S2.S2Point where

import           Data.AEq                  (AEq, (~==))
import           Foreign.Ptr               (castPtr)
import           Foreign.Storable          (Storable (..))
import           Test.QuickCheck.Arbitrary (Arbitrary (..))


data S2Point = S2Point { x, y, z :: Double }
  deriving (Eq, Read, Show)

instance AEq S2Point where
  a ~== b =
    x a ~== x b &&
    y a ~== y b &&
    z a ~== z b

instance Arbitrary S2Point where
  arbitrary = S2Point <$> arbitrary <*> arbitrary <*> arbitrary

instance Storable S2Point where
  sizeOf _ = sizeOf (x undefined) * 3
  alignment _ = alignment (x undefined)

  peek ptr = S2Point
    <$> peekElemOff (castPtr ptr) 0
    <*> peekElemOff (castPtr ptr) 1
    <*> peekElemOff (castPtr ptr) 2

  poke ptr p = do
    pokeElemOff (castPtr ptr) 0 (x p)
    pokeElemOff (castPtr ptr) 1 (y p)
    pokeElemOff (castPtr ptr) 2 (z p)


-- A unique "origin" on the sphere for operations that need a fixed reference
-- point.  In particular, this is the "point at infinity" used for
-- point-in-polygon testing (by counting the number of edge crossings).
--
-- It should *not* be a point that is commonly used in edge tests in order to
-- avoid triggering code to handle degenerate cases.  (This rules out the north
-- and south poles.)  It should also not be on the boundary of any low-level
-- S2Cell for the same reason.
origin :: S2Point
origin = normalise $ S2Point 0.00457 1 0.0321


get :: Int -> S2Point -> Double
get 0 = x
get 1 = y
get _ = z


sub :: S2Point -> S2Point -> S2Point
sub p1 p2 = S2Point
  (x p1 - x p2)
  (y p1 - y p2)
  (z p1 - z p2)


add :: S2Point -> S2Point -> S2Point
add p1 p2 = S2Point
  (x p1 + x p2)
  (y p1 + y p2)
  (z p1 + z p2)


mul :: S2Point -> Double -> S2Point
mul S2Point { x, y, z } m = S2Point
  (x * m) (y * m) (z * m)


div :: S2Point -> Double -> S2Point
div S2Point { x, y, z } m = S2Point
  (x / m) (y / m) (z / m)


neg :: S2Point -> S2Point
neg S2Point { x, y, z } =
  S2Point (-x) (-y) (-z)


angle :: S2Point -> S2Point -> Double
angle p1 p2 =
  atan2 (norm (crossProd p1 p2)) (dotProd p1 p2)


crossProd :: S2Point -> S2Point -> S2Point
crossProd p1 p2 = S2Point
  (y p1 * z p2 - z p1 * y p2)
  (z p1 * x p2 - x p1 * z p2)
  (x p1 * y p2 - y p1 * x p2)


dotProd :: S2Point -> S2Point -> Double
dotProd p1 p2 =
  x p1 * x p2 +
  y p1 * y p2 +
  z p1 * z p2


norm2 :: S2Point -> Double
norm2 p =
  x p * x p +
  y p * y p +
  z p * z p


norm :: S2Point -> Double
norm = sqrt . norm2


normalise :: S2Point -> S2Point
normalise p =
  case norm p of
    0 -> mul p 0
    n -> mul p (1 / n)


fabs :: S2Point -> S2Point
fabs S2Point { x, y, z } = S2Point
  (abs x) (abs y) (abs z)


-- Return the index of the largest component fabs
largestAbsComponent :: S2Point -> Int
largestAbsComponent p =
  let temp = fabs p in
  if x temp > y temp
    then if x temp > z temp
      then 0
      else 2
    else if y temp > z temp
      then 1
      else 2

-- Return a vector orthogonal to the passed one.
ortho :: S2Point -> S2Point
ortho p =
  normalise $ crossProd p $ case largestAbsComponent p of
    1 -> S2Point 1 0 0
    2 -> S2Point 0 1 0
    _ -> S2Point 0 0 1


isUnitLength :: S2Point -> Bool
isUnitLength p = abs (norm2 p - 1) <= 1e-15
