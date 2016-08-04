{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | This module specifies the details of how the cube faces are projected onto
-- the unit sphere. This includes getting the face ordering and orientation
-- correct so that sequentially increasing cell ids follow a continuous
-- space-filling curve over the entire sphere, and defining the transformation
-- from cell-space to cube-space (see s2.h) in order to make the cells more
-- uniform in size.
--
--
--  We have implemented three different projections from cell-space (s,t) to
--  cube-space (u,v): linear, quadratic, and tangent. They have the following
--  tradeoffs:
--
--  Linear - This is the fastest transformation, but also produces the least
--  uniform cell sizes. Cell areas vary by a factor of about 5.2, with the
--  largest cells at the center of each face and the smallest cells in the
--  corners.
--
--  Tangent - Transforming the coordinates via atan() makes the cell sizes more
--  uniform. The areas vary by a maximum ratio of 1.4 as opposed to a maximum
--  ratio of 5.2. However, each call to atan() is about as expensive as all of
--  the other calculations combined when converting from points to cell ids,
--  i.e.  it reduces performance by a factor of 3.
--
--  Quadratic - This is an approximation of the tangent projection that is much
--  faster and produces cells that are almost as uniform in size. It is about 3
--  times faster than the tangent projection for converting cell ids to points,
--  and 2 times faster for converting points to cell ids. Cell areas vary by a
--  maximum ratio of about 2.1.
--
--  Here is a table comparing the cell uniformity using each projection. "Area
--  ratio" is the maximum ratio over all subdivision levels of the largest cell
--  area to the smallest cell area at that level, "edge ratio" is the maximum
--  ratio of the longest edge of any cell to the shortest edge of any cell at
--  the same level, and "diag ratio" is the ratio of the longest diagonal of
--  any cell to the shortest diagonal of any cell at the same level. "ToPoint"
--  and "FromPoint" are the times in microseconds required to convert cell ids
--  to and from points (unit vectors) respectively.
--
--  Area Edge Diag ToPoint FromPoint Ratio Ratio Ratio (microseconds)
--  -------------------------------------------------------
--  Linear: 5.200 2.117 2.959 0.103 0.123
--  Tangent: 1.414 1.414 1.704 0.290 0.306
--  Quadratic: 2.082 1.802 1.932 0.116 0.161
--
--  The worst-case cell aspect ratios are about the same with all three
--  projections. The maximum ratio of the longest edge to the shortest edge
--  within the same cell is about 1.4 and the maximum ratio of the diagonals
--  within the same cell is about 1.7.
--
-- This data was produced using s2cell_unittest and s2cellid_unittest.
--
module Algorithms.Geometry.S2.S2Projections where

import           Data.AEq                        (AEq)
import           Data.Bits                       (shiftL)
import           Foreign.Ptr                     (castPtr)
import           Foreign.Storable                (Storable (..))

import           Algorithms.Geometry.S2.Metric   (AreaMetric (..),
                                                  LengthMetric (..))
import qualified Algorithms.Geometry.S2.Metric   as Metric
import           Algorithms.Geometry.S2.R2Vector (R2Vector (R2Vector))
import qualified Algorithms.Geometry.S2.R2Vector as R2Vector
import           Algorithms.Geometry.S2.S2       (Face (..))
import qualified Algorithms.Geometry.S2.S2       as S2
import           Algorithms.Geometry.S2.S2Point  (S2Point (..))
import qualified Algorithms.Geometry.S2.S2Point  as S2Point


data Projection
  = S2_LINEAR_PROJECTION
  | S2_TAN_PROJECTION
  | S2_QUADRATIC_PROJECTION
  deriving (Eq, Read, Show)

instance AEq Projection

instance Storable Projection where
  sizeOf _ = sizeOf (undefined :: Int)
  alignment _ = alignment (undefined :: Int)

  peek ptr = fromInt <$> (peek $ castPtr ptr :: IO Int)
  poke ptr = poke (castPtr ptr) . toInt


fromInt :: Int -> Projection
fromInt 0 = S2_LINEAR_PROJECTION
fromInt 1 = S2_TAN_PROJECTION
fromInt _ = S2_QUADRATIC_PROJECTION

toInt :: Projection -> Int
toInt S2_LINEAR_PROJECTION    = 0
toInt S2_TAN_PROJECTION       = 1
toInt S2_QUADRATIC_PROJECTION = 2


projection :: Projection
projection = S2_QUADRATIC_PROJECTION

-- All of the values below were obtained by a combination of hand analysis and
-- Mathematica. In general, S2_TAN_PROJECTION produces the most uniform
-- shapes and sizes of cells, S2_LINEAR_PROJECTION is considerably worse, and
-- S2_QUADRATIC_PROJECTION is somewhere in between (but generally closer to
-- the tangent projection than the linear one).


-- The minimum area of any cell at level k is at least MIN_AREA.GetValue(k),
-- and the maximum is at most MAX_AREA.GetValue(k). The average area of all
-- cells at level k is exactly AVG_AREA.GetValue(k).
minArea :: AreaMetric
minArea = AreaMetric $ case projection of
  S2_LINEAR_PROJECTION    -> 1 / (3 * sqrt 3) -- 0.192
  S2_TAN_PROJECTION       -> (pi * pi) / (16 * sqrt 2) -- 0.436
  S2_QUADRATIC_PROJECTION -> 2 * sqrt 2 / 9 -- 0.314

maxArea :: AreaMetric
maxArea = AreaMetric $ case projection of
  S2_LINEAR_PROJECTION    -> 1 -- 1.000
  S2_TAN_PROJECTION       -> pi * pi / 16 -- 0.617
  S2_QUADRATIC_PROJECTION -> 0.65894981424079037 -- 0.659

avgArea :: AreaMetric
avgArea = AreaMetric (pi / 6) -- 0.524


-- Each cell is bounded by four planes passing through its four edges and
-- the center of the sphere. These metrics relate to the angle between each
-- pair of opposite bounding planes, or equivalently, between the planes
-- corresponding to two different s-values or two different t-values. For
-- example, the maximum angle between opposite bounding planes for a cell at
-- level k is MAX_ANGLE_SPAN.GetValue(k), and the average angle span for all
-- cells at level k is approximately AVG_ANGLE_SPAN.GetValue(k).
minAngleSpan :: LengthMetric
minAngleSpan = LengthMetric $ case projection of
  S2_LINEAR_PROJECTION    -> 0.5 -- 0.500
  S2_TAN_PROJECTION       -> pi / 4 -- 0.785
  S2_QUADRATIC_PROJECTION -> 2.0 / 3 -- 0.667

maxAngleSpan :: LengthMetric
maxAngleSpan = LengthMetric $ case projection of
  S2_LINEAR_PROJECTION    -> 1 -- 1.000
  S2_TAN_PROJECTION       -> pi / 4 -- 0.785
  S2_QUADRATIC_PROJECTION -> 0.85244858959960922 -- 0.852

avgAngleSpan :: LengthMetric
avgAngleSpan = LengthMetric (pi / 4) -- 0.785


-- The width of geometric figure is defined as the distance between two
-- parallel bounding lines in a given direction. For cells, the minimum
-- width is always attained between two opposite edges, and the maximum
-- width is attained between two opposite vertices. However, for our
-- purposes we redefine the width of a cell as the perpendicular distance
-- between a pair of opposite edges. A cell therefore has two widths, one
-- in each direction. The minimum width according to this definition agrees
-- with the classic geometric one, but the maximum width is different. (The
-- maximum geometric width corresponds to MAX_DIAG defined below.)
--
-- For a cell at level k, the distance between opposite edges is at least
-- MIN_WIDTH.GetValue(k) and at most MAX_WIDTH.GetValue(k). The average
-- width in both directions for all cells at level k is approximately
-- AVG_WIDTH.GetValue(k).
--
-- The width is useful for bounding the minimum or maximum distance from a
-- point on one edge of a cell to the closest point on the opposite edge.
-- For example, this is useful when "growing" regions by a fixed distance.
minWidth :: LengthMetric
minWidth = LengthMetric $ case projection of
  S2_LINEAR_PROJECTION    -> 1 / sqrt 6 -- 0.408
  S2_TAN_PROJECTION       -> pi / (4 * sqrt 2) -- 0.555
  S2_QUADRATIC_PROJECTION -> sqrt 2 / 3 -- 0.471

maxWidth :: LengthMetric
maxWidth = LengthMetric (Metric.deriv maxAngleSpan)

avgWidth :: LengthMetric
avgWidth = LengthMetric $ case projection of
  S2_LINEAR_PROJECTION    -> 0.70572967292222848 -- 0.706
  S2_TAN_PROJECTION       -> 0.71865931946258044 -- 0.719
  S2_QUADRATIC_PROJECTION -> 0.71726183644304969 -- 0.717


-- The minimum edge length of any cell at level k is at least
-- MIN_EDGE.GetValue(k), and the maximum is at most MAX_EDGE.GetValue(k).
-- The average edge length is approximately AVG_EDGE.GetValue(k).
--
-- The edge length metrics can also be used to bound the minimum, maximum,
-- or average distance from the center of one cell to the center of one of
-- its edge neighbors. In particular, it can be used to bound the distance
-- between adjacent cell centers along the space-filling Hilbert curve for
-- cells at any given level.
minEdge :: LengthMetric
minEdge = LengthMetric $ case projection of
  S2_LINEAR_PROJECTION    -> sqrt 2 / 3 -- 0.471
  S2_TAN_PROJECTION       -> pi / (4 * sqrt 2) -- 0.555
  S2_QUADRATIC_PROJECTION -> sqrt 2 / 3 -- 0.471

maxEdge :: LengthMetric
maxEdge = LengthMetric (Metric.deriv maxAngleSpan)

avgEdge :: LengthMetric
avgEdge = LengthMetric $ case projection of
  S2_LINEAR_PROJECTION    -> 0.72001709647780182 -- 0.720
  S2_TAN_PROJECTION       -> 0.73083351627336963 -- 0.731
  S2_QUADRATIC_PROJECTION -> 0.72960687319305303 -- 0.730


-- The minimum diagonal length of any cell at level k is at least
-- MIN_DIAG.GetValue(k), and the maximum is at most MAX_DIAG.GetValue(k).
-- The average diagonal length is approximately AVG_DIAG.GetValue(k).
--
-- The maximum diagonal also happens to be the maximum diameter of any cell,
-- and also the maximum geometric width (see the discussion above). So for
-- example, the distance from an arbitrary point to the closest cell center
-- at a given level is at most half the maximum diagonal length.
minDiag :: LengthMetric
minDiag = LengthMetric $ case projection of
  S2_LINEAR_PROJECTION    -> sqrt 2 / 3 -- 0.471
  S2_TAN_PROJECTION       -> pi / (3 * sqrt 2) -- 0.740
  S2_QUADRATIC_PROJECTION -> 4 * sqrt 2 / 9 -- 0.629

maxDiag :: LengthMetric
maxDiag = LengthMetric $ case projection of
  S2_LINEAR_PROJECTION    -> sqrt 2 -- 1.414
  S2_TAN_PROJECTION       -> pi / sqrt 6 -- 1.283
  S2_QUADRATIC_PROJECTION -> 1.2193272972170106 -- 1.219

avgDiag :: LengthMetric
avgDiag = LengthMetric $ case projection of
  S2_LINEAR_PROJECTION    -> 1.0159089332094063 -- 1.016
  S2_TAN_PROJECTION       -> 1.0318115985978178 -- 1.032
  S2_QUADRATIC_PROJECTION -> 1.03021136949923584 -- 1.030


-- This is the maximum edge aspect ratio over all cells at any level, where
-- the edge aspect ratio of a cell is defined as the ratio of its longest
-- edge length to its shortest edge length.
maxEdgeAspect :: Double
maxEdgeAspect = case projection of
  S2_LINEAR_PROJECTION    -> sqrt 2 -- 1.414
  S2_TAN_PROJECTION       -> sqrt 2 -- 1.414
  S2_QUADRATIC_PROJECTION -> 1.44261527445268292 -- 1.443


-- This is the maximum diagonal aspect ratio over all cells at any level,
-- where the diagonal aspect ratio of a cell is defined as the ratio of its
-- longest diagonal length to its shortest diagonal length.
maxDiagAspect :: Double
maxDiagAspect = sqrt 3 -- 1.732

-------------------------- S2Cell Decomposition ------------------------/
--
-- The following methods define the cube-to-sphere projection used by
-- the S2Cell decomposition.
--
-- In the process of converting a latitude-longitude pair to a 64-bit cell
-- id, the following coordinate systems are used:
--
--  (id)
--    An S2CellId is a 64-bit encoding of a face and a Hilbert curve position
--    on that face.  The Hilbert curve position implicitly encodes both the
--    position of a cell and its subdivision level (see s2cellid.h).
--
--  (face, i, j)
--    Leaf-cell coordinates.  "i" and "j" are integers in the range
--    [0,(2**30)-1] that identify a particular leaf cell on the given face.
--    The (i, j) coordinate system is right-handed on each face, and the
--    faces are oriented such that Hilbert curves connect continuously from
--    one face to the next.
--
--  (face, s, t)
--    Cell-space coordinates.  "s" and "t" are real numbers in the range
--    [0,1] that identify a point on the given face.  For example, the point
--    (s, t) = (0.5, 0.5) corresponds to the center of the top-level face
--    cell.  This point is also a vertex of exactly four cells at each
--    subdivision level greater than zero.
--
--  (face, si, ti)
--    Discrete cell-space coordinates.  These are obtained by multiplying
--    "s" and "t" by 2**31 and rounding to the nearest unsigned integer.
--    Discrete coordinates lie in the range [0,2**31].  This coordinate
--    system can represent the edge and center positions of all cells with
--    no loss of precision (including non-leaf cells).
--
--  (face, u, v)
--    Cube-space coordinates.  To make the cells at each level more uniform
--    in size after they are projected onto the sphere, we apply apply a
--    nonlinear transformation of the form u=f(s), v=f(t).  The (u, v)
--    coordinates after this transformation give the actual coordinates on
--    the cube face (modulo some 90 degree rotations) before it is projected
--    onto the unit sphere.
--
--  (x, y, z)
--    Direction vector (S2Point).  Direction vectors are not necessarily unit
--    length, and are often chosen to be points on the biunit cube
--    [-1,+1]x[-1,+1]x[-1,+1].  They can be be normalized to obtain the
--    corresponding point on the unit sphere.
--
--  (lat, lng)
--    Latitude and longitude (S2LatLng).  Latitudes must be between -90 and
--    90 degrees inclusive, and longitudes must be between -180 and 180
--    degrees inclusive.
--
-- Note that the (i, j), (s, t), (si, ti), and (u, v) coordinate systems are
-- right-handed on all six faces.


-- Convert an s or t value  to the corresponding u or v value.  This is
-- a non-linear transformation from [-1,1] to [-1,1] that attempts to
-- make the cell sizes more uniform.
stToUV :: Double -> Double
stToUV s = case projection of
  S2_LINEAR_PROJECTION -> 2 * s - 1
  S2_TAN_PROJECTION ->
    -- Unfortunately, tan(pi / 4) is slightly less than 1.0. This isn't due
    -- to
    -- a flaw in the implementation of tan(), it's because the derivative of
    -- tan(x) at x=pi/4 is 2, and it happens that the two adjacent floating
    -- point numbers on either side of the infinite-precision value of pi/4
    -- have
    -- tangents that are slightly below and slightly above 1.0 when rounded
    -- to
    -- the nearest double-precision result.
    let s' = tan ((pi / 4) * s) in
    s' + (1.0 / fromIntegral ((1 :: Int) `shiftL` 53)) * s'
  S2_QUADRATIC_PROJECTION ->
    if s >= 0.5
      then (1 / 3.0) * (4 * s * s - 1)
      else (1 / 3.0) * (1 - 4 * (1 - s) * (1 - s))


-- The inverse of the STtoUV transformation.  Note that it is not always
-- true that UVtoST(STtoUV(x)) == x due to numerical errors.
uvToST :: Double -> Double
uvToST u = case projection of
  S2_LINEAR_PROJECTION -> 0.5 * (u + 1)
  S2_TAN_PROJECTION ->
    (4 * (1 / pi)) * atan u
  S2_QUADRATIC_PROJECTION ->
    if u >= 0
      then 0.5 * sqrt (1 + 3 * u)
      else 1 - 0.5 * sqrt (1 - 3 * u)


-- Convert (face, u, v) coordinates to a direction vector (not necessarily
-- unit length).
faceUvToXyz :: Double -> Double -> Face -> S2Point
faceUvToXyz u v = \case
  Face 0 -> S2Point   1    u    v
  Face 1 -> S2Point (-u)   1    v
  Face 2 -> S2Point (-u) (-v)   1
  Face 3 -> S2Point (-1) (-v) (-u)
  Face 4 -> S2Point   v  (-1) (-u)
  Face _ -> S2Point   v    u  (-1)


validFaceXyzToUv :: S2Point -> Face -> R2Vector
validFaceXyzToUv S2Point { x, y, z } = \case
  Face 0 -> R2Vector ( y / x) ( z / x)
  Face 1 -> R2Vector (-x / y) ( z / y)
  Face 2 -> R2Vector (-x / z) (-y / z)
  Face 3 -> R2Vector ( z / x) ( y / x)
  Face 4 -> R2Vector ( z / y) (-x / y)
  Face _ -> R2Vector (-y / z) (-x / z)


faceXyzToUv :: S2Point -> Face -> Maybe R2Vector
faceXyzToUv p face@(Face f)
  | f <  3 && S2Point.get  f      p <= 0 = Nothing
  | f >= 3 && S2Point.get (f - 3) p >= 0 = Nothing
  | otherwise = Just $ validFaceXyzToUv p face


xyzToFaceUV :: S2Point -> (Face, R2Vector)
xyzToFaceUV p =
  let face = xyzToFace p in
  (face, validFaceXyzToUv p face)


xyzToFace :: S2Point -> Face
xyzToFace p =
  let face = S2Point.largestAbsComponent p in
  if S2Point.get face p < 0
    then Face (face + 3)
    else Face face


-- | Return the i- or j-index of the leaf cell containing the given s- or
-- t-value.
stToIJ :: Double -> Int
stToIJ s =
  fromIntegral $ max 0 (min (S2.maxSize - 1) (floor (fromIntegral S2.maxSize * s)))


xyzToFaceIJ :: S2Point -> (Face, Int, Int)
xyzToFaceIJ p =
  let
    (face, uv) = xyzToFaceUV p
    i = stToIJ . uvToST . R2Vector.x $ uv
    j = stToIJ . uvToST . R2Vector.y $ uv
  in
  (face, i, j)


getUNorm :: Double -> Face -> S2Point
getUNorm u = \case
  Face 0 -> S2Point   u  (-1)   0
  Face 1 -> S2Point   1    u    0
  Face 2 -> S2Point   1    0    u
  Face 3 -> S2Point (-u)   0    1
  Face 4 -> S2Point   0  (-u)   1
  Face _ -> S2Point   0  (-1) (-u)


getVNorm :: Double -> Face -> S2Point
getVNorm v = \case
  Face 0 -> S2Point (-v)   0    1
  Face 1 -> S2Point   0  (-v)   1
  Face 2 -> S2Point   0  (-1) (-v)
  Face 3 -> S2Point   v  (-1)   0
  Face 4 -> S2Point   1    v    0
  Face _ -> S2Point   1    0    v


getNorm :: Face -> S2Point
getNorm = faceUvToXyz 0 0


getUAxis :: Face -> S2Point
getUAxis = \case
  Face 0 -> S2Point   0  1   0
  Face 1 -> S2Point (-1) 0   0
  Face 2 -> S2Point (-1) 0   0
  Face 3 -> S2Point   0  0 (-1)
  Face 4 -> S2Point   0  0 (-1)
  Face _ -> S2Point   0  1   0


getVAxis :: Face -> S2Point
getVAxis = \case
  Face 0 -> S2Point 0   0  1
  Face 1 -> S2Point 0   0  1
  Face 2 -> S2Point 0 (-1) 0
  Face 3 -> S2Point 0 (-1) 0
  Face 4 -> S2Point 1   0  0
  Face _ -> S2Point 1   0  0
