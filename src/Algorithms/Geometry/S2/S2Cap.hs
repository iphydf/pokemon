{-# LANGUAGE NamedFieldPuns #-}
-- This module represents a spherical cap, i.e. a portion of a sphere cut off
-- by a plane.  The cap is defined by its axis and height.  This
-- representation has good numerical accuracy for very small caps (unlike the
-- (axis, min-distance-from-origin) representation), and is also efficient for
-- containment tests (unlike the (axis, angle) representation).
--
-- Here are some useful relationships between the cap height (h), the cap
-- opening angle (theta), the maximum chord length from the cap's center (d),
-- and the radius of cap's base (a).  All formulas assume a unit radius.
--
--     h = 1 - cos(theta)
--       = 2 sin^2(theta/2)
--   d^2 = 2 h
--       = a^2 + h^2
--
-- Caps may be constructed from either an axis and a height, or an axis and
-- an angle.  To avoid ambiguity, there are no public constructors except
-- the default constructor.
--
-- This class is intended to be copied by value as desired.  It uses
-- the default copy constructor and assignment operator, however it is
-- not a "plain old datatype" (POD) because it has virtual functions.
module Algorithms.Geometry.S2.S2Cap where

import           Data.AEq                       (AEq (..))
import           Data.Default.Class             (Default (..))

import           Algorithms.Geometry.S2.S1Angle as S1Angle
import           Algorithms.Geometry.S2.S2Point as S2Point


data S2Cap = S2Cap
  { axis   :: S2Point
  , height :: Double
  }
  deriving (Eq, Read, Show)

instance AEq S2Cap where
  a ~== b =
    axis a ~== axis b &&
    height a ~== height b

instance Default S2Cap where
  -- The default constructor returns an empty S2Cap.
  def = S2Cap (S2Point 1 0 0) (-1)


-- Return the cap height corresponding to the given non-negative cap angle in
-- radians.  Cap angles of Pi radians or larger yield a full cap.
getHeightForAngle :: Double -> Double
getHeightForAngle radians
  -- Caps of Pi radians or more are full.
  | radians >= pi = 2

  -- The height of the cap can be computed as 1 - cos(radians), but this isn't
  -- very accurate for angles close to zero (where cos(radians) is almost 1).
  -- Computing it as 2 * (sin(radians / 2) ** 2) gives much better precision.
  | otherwise =
      let d = sin (0.5 * radians) in
      2 * d * d;


-- Create a cap given its axis and the cap height, i.e. the maximum
-- projected distance along the cap axis from the cap center.
-- 'axis' should be a unit-length vector.
fromAxisHeight :: S2Point -> Double -> S2Cap
fromAxisHeight = S2Cap

-- Create a cap given its axis and the cap opening angle, i.e. maximum
-- angle between the axis and a point on the cap.  'axis' should be a
-- unit-length vector, and 'angle' should be non-negative.  If 'angle' is
-- 180 degrees or larger, the cap will contain the entire unit sphere.
fromAxisAngle :: S2Point -> S1Angle -> S2Cap
fromAxisAngle axis angle =
  S2Cap axis (getHeightForAngle (S1Angle.radians angle))

-- Create a cap given its axis and its area in steradians.  'axis' should be
-- a unit-length vector, and 'area' should be between 0 and 4 * M_PI.
fromAxisArea :: S2Point -> Double -> S2Cap
fromAxisArea axis area = undefined
  S2Cap axis (area / (2 * pi))


-- Return an empty cap, i.e. a cap that contains no points.
empty :: S2Cap
empty = def


-- Return a full cap, i.e. a cap that contains all points.
full :: S2Cap
full = S2Cap (S2Point 1 0 0) 2


area :: S2Cap -> Double
area S2Cap { height } = 2 * pi * max 0 height


-- Return the cap opening angle in radians, or a negative number for
-- empty caps.
angle :: S2Cap -> S1Angle
angle = undefined


-- We allow negative heights (to represent empty caps) but not heights
-- greater than 2.
isValid :: S2Cap -> Bool
isValid S2Cap { axis, height } = S2Point.isUnitLength axis && height <= 2


-- Return true if the cap is empty, i.e. it contains no points.
isEmpty :: S2Cap -> Bool
isEmpty S2Cap { height } = height < 0


-- Return true if the cap is full, i.e. it contains all points.
isFull :: S2Cap -> Bool
isFull S2Cap { height } = height >= 2


-- Return the complement of the interior of the cap.  A cap and its
-- complement have the same boundary but do not share any interior points.
-- The complement operator is not a bijection, since the complement of a
-- singleton cap (containing a single point) is the same as the complement
-- of an empty cap.
complement :: S2Cap -> S2Cap
complement = undefined


-- Return true if and only if this cap contains the given other cap
-- (in a set containment sense, e.g. every cap contains the empty cap).
contains :: S2Cap -> S2Cap -> Bool
contains c1 c2 = undefined


-- Return true if and only if this cap intersects the given other cap,
-- i.e. whether they have any points in common.
intersects :: S2Cap -> S2Cap -> Bool
intersects c1 c2 = undefined


-- Return true if and only if the interior of this cap intersects the
-- given other cap.  (This relationship is not symmetric, since only
-- the interior of this cap is used.)
interiorIntersects :: S2Cap -> S2Cap -> Bool
interiorIntersects c1 c2 = undefined


-- Return true if and only if the given point is contained in the interior
-- of the region (i.e. the region excluding its boundary).  'p' should be
-- be a unit-length vector.
interiorContains :: S2Cap -> S2Cap -> Bool
interiorContains c1 c2 = undefined


-- Increase the cap height if necessary to include the given point.
-- If the cap is empty the axis is set to the given point, but otherwise
-- it is left unchanged.  'p' should be a unit-length vector.
addPoint :: S2Cap -> S2Point -> S2Cap
addPoint cap p = undefined


-- Increase the cap height if necessary to include "other".  If the current
-- cap is empty it is set to the given other cap.
addCap :: S2Cap -> S2Cap -> S2Cap
addCap c1 c2 = undefined


-- Return a cap that contains all points within a given distance of this
-- cap.  Note that any expansion of the empty cap is still empty.
expanded :: S2Cap -> S1Angle -> S2Cap
expanded c distance = undefined


-- The point 'p' should be a unit-length vector.
containsPoint :: S2Cap -> S2Point -> Bool
containsPoint = undefined
