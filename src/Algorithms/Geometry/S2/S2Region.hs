-- An S2Region represents a two-dimensional region over the unit sphere.
-- It is an abstract interface with various concrete subtypes.
--
-- The main purpose of this interface is to allow complex regions to be
-- approximated as simpler regions.  So rather than having a wide variety
-- of virtual methods that are implemented by all subtypes, the interface
-- is restricted to methods that are useful for computing approximations.
module Algorithms.Geometry.S2.S2Region where

import           Algorithms.Geometry.S2.S2Cap        (S2Cap)
import qualified Algorithms.Geometry.S2.S2Cap        as S2Cap
import           Algorithms.Geometry.S2.S2Cell       (S2Cell)
import           Algorithms.Geometry.S2.S2LatLngRect (S2LatLngRect)
import           Algorithms.Geometry.S2.S2Point      (S2Point)


class S2Region a where
  -- Return a bounding spherical cap. This is not guaranteed to be exact.
  getCapBound :: a -> S2Cap

  -- Return a bounding latitude-longitude rectangle that contains the region.
  -- The bounds are not guaranteed to be tight.
  getRectBound :: a -> S2LatLngRect

  -- If this method returns true, the region completely contains the given
  -- cell.  Otherwise, either the region does not contain the cell or the
  -- containment relationship could not be determined.
  containsCell :: a -> S2Cell -> Bool

  -- If this method returns false, the region does not intersect the given
  -- cell.  Otherwise, either region intersects the cell, or the intersection
  -- relationship could not be determined.
  mayIntersect :: a -> S2Cell -> Bool

  -- Return true if and only if the given point is contained by the region.
  -- The point 'p' is generally required to be unit length, although some
  -- subtypes may relax this restriction.
  -- NOTE: If you will be calling this function on one specific subtype only,
  -- or if performance is a consideration, please use the non-virtual
  -- method Contains(S2Point const& p) declared below!
  virtualContainsPoint :: a -> S2Point -> Bool


------------------------------------------------------------------------
-- S2Region implementation for S2Cap (see above for details):

instance S2Region S2Cap where
  getCapBound = undefined
  getRectBound = undefined
  containsCell = undefined
  mayIntersect = undefined
  virtualContainsPoint = S2Cap.containsPoint
