{-# LANGUAGE NamedFieldPuns #-}
-- An S2RegionCoverer is a class that allows arbitrary regions to be
-- approximated as unions of cells (S2CellUnion).  This is useful for
-- implementing various sorts of search and precomputation operations.
--
-- Typical usage:
--
-- S2RegionCoverer coverer;
-- coverer.set_max_cells(5);
-- S2Cap cap = S2Cap::FromAxisAngle(...);
-- vector<S2CellId> covering;
-- coverer.GetCovering(cap, &covering);
--
-- This yields a vector of at most 5 cells that is guaranteed to cover the
-- given cap (a disc-shaped region on the sphere).
--
-- The approximation algorithm is not optimal but does a pretty good job in
-- practice.  The output does not always use the maximum number of cells
-- allowed, both because this would not always yield a better approximation,
-- and because max_cells() is a limit on how much work is done exploring the
-- possible covering as well as a limit on the final output size.
--
-- One can also generate interior coverings, which are sets of cells which
-- are entirely contained within a region.  Interior coverings can be
-- empty, even for non-empty regions, if there are no cells that satisfy
-- the provided constraints and are contained by the region.  Note that for
-- performance reasons, it is wise to specify a max_level when computing
-- interior coverings - otherwise for regions with small or zero area, the
-- algorithm may spend a lot of time subdividing cells all the way to leaf
-- level to try to find contained cells.
module Algorithms.Geometry.S2.S2RegionCoverer where

import           Data.Default.Class                 (Default (..))

import qualified Algorithms.Geometry.S2.S2          as S2
import           Algorithms.Geometry.S2.S2Cell
import           Algorithms.Geometry.S2.S2CellId
import           Algorithms.Geometry.S2.S2CellUnion as S2CellUnion
import           Algorithms.Geometry.S2.S2Point
import           Algorithms.Geometry.S2.S2Region


data S2RegionCoverer = S2RegionCoverer
  { minLevel :: Int
  , maxLevel :: Int
    -- ^ Set the minimum and maximum cell level to be used.  The default is to
    -- use all cell levels.  Requires: max_level() >= min_level().
    --
    -- To find the cell level corresponding to a given physical distance, use
    -- the S2Cell metrics defined in s2.h.  For example, to find the cell
    -- level that corresponds to an average edge length of 10km, use:
    --
    --     int level = S2::kAvgEdge.GetClosestLevel(
    --                 geostore::S2Earth::KmToRadians(length_km));
    --
    -- Note: min_level() takes priority over max_cells(), i.e. cells below the
    -- given level will never be used even if this causes a large number of
    -- cells to be returned.
  , levelMod :: Int
    -- ^ If specified, then only cells where (level - min_level) is a multiple
    -- of "level_mod" will be used (default 1).  This effectively allows the
    -- branching factor of the S2CellId hierarchy to be increased.  Currently
    -- the only parameter values allowed are 1, 2, or 3, corresponding to
    -- branching factors of 4, 16, and 64 respectively.
  , maxCells :: Int
    -- ^ Sets the maximum desired number of cells in the approximation (defaults
    -- to 'defaultMaxCells').  Note the following:
    --
    --  - For any setting of max_cells(), up to 6 cells may be returned if that
    --    is the minimum number of cells required (e.g. if the region intersects
    --    all six face cells).  Up to 3 cells may be returned even for very tiny
    --    convex regions if they happen to be located at the intersection of
    --    three cube faces.
    --
    --  - For any setting of max_cells(), an arbitrary number of cells may be
    --    returned if min_level() is too high for the region being approximated.
    --
    --  - If max_cells() is less than 4, the area of the covering may be
    --    arbitrarily large compared to the area of the original region even if
    --    the region is convex (e.g. an S2Cap or S2LatLngRect).
    --
    -- Accuracy is measured by dividing the area of the covering by the area of
    -- the original region.  The following table shows the median and worst case
    -- values for this area ratio on a test case consisting of 100,000 spherical
    -- caps of random size (generated using s2regioncoverer_unittest):
    --
    --   max_cells:        3      4     5     6     8    12    20   100   1000
    --   median ratio:  5.33   3.32  2.73  2.34  1.98  1.66  1.42  1.11   1.01
    --   worst case:  215518  14.41  9.72  5.26  3.91  2.75  1.92  1.20   1.02
  }


instance Default S2RegionCoverer where
  def = S2RegionCoverer
    { minLevel = 0
    , maxLevel = S2.maxCellLevel
    , levelMod = 1
    , maxCells = defaultMaxCells
    }


-- By default, the covering uses at most 8 cells at any level.  This gives
-- a reasonable tradeoff between the number of cells used and the accuracy
-- of the approximation (see table below).
defaultMaxCells :: Int
defaultMaxCells = 8


-- Return a vector of cell ids that covers (getCovering) or is contained
-- within (getInteriorCovering) the given region and satisfies the various
-- restrictions specified above.
getCovering :: S2Region region => S2RegionCoverer -> region -> [S2CellId]
getCovering coverer@S2RegionCoverer { minLevel, levelMod } region =
  -- Rather than just returning the raw list of cell ids generated by
  -- GetCoveringInternal(), we construct a cell union and then denormalize it.
  -- This has the effect of replacing four child cells with their parent
  -- whenever this does not violate the covering parameters specified
  -- (min_level, level_mod, etc).  This strategy significantly reduces the
  -- number of cells returned in many cases, and it is cheap compared to
  -- computing the covering in the first place.
  S2CellUnion.denormalize (getCellUnion coverer region) minLevel levelMod


getInteriorCovering :: S2Region region => S2RegionCoverer -> region -> [S2CellId]
getInteriorCovering = error "getInteriorCovering"


-- Return a normalized cell union that covers (GetCellUnion) or is contained
-- within (GetInteriorCellUnion) the given region and satisfies the
-- restrictions *EXCEPT* for min_level() and level_mod().  These criteria
-- cannot be satisfied using a cell union because cell unions are
-- automatically normalized by replacing four child cells with their parent
-- whenever possible.  (Note that the list of cell ids passed to the cell
-- union constructor does in fact satisfy all the given restrictions.)
getCellUnion :: S2Region region => S2RegionCoverer -> region -> S2CellUnion
getCellUnion = error "getCellUnion"

getInteriorCellUnion :: S2Region region => S2RegionCoverer -> region -> S2CellUnion
getInteriorCellUnion = error "getInteriorCellUnion"


-- Given a connected region and a starting point, return a set of cells at
-- the given level that cover the region.
getSimpleCovering :: S2Region region => S2RegionCoverer -> region -> S2Point -> Int -> [S2CellId]
getSimpleCovering = error "getSimpleCovering"


data Candidate = Candidate
  { cell        :: S2Cell
  , isTerminal  :: Bool   -- ^ Cell should not be expanded further.
  , numChildren :: Int        -- ^ Number of children that intersect the region.
  , children    :: [Candidate]  -- ^ Actual size may be 0, 4, 16, or 64 elements.
  }


-- If the cell intersects the given region, return a new candidate with no
-- children, otherwise return NULL.  Also marks the candidate as "terminal"
-- if it should not be expanded further.
newCandidate :: S2RegionCoverer -> S2Cell -> Candidate
newCandidate = error "newCandidate"


-- Return the log base 2 of the maximum number of children of a candidate.
maxChildrenShift :: S2RegionCoverer -> Int
maxChildrenShift S2RegionCoverer { levelMod } = 2 * levelMod


-- Process a candidate by either adding it to the result_ vector or
-- expanding its children and inserting it into the priority queue.
-- Passing an argument of NULL does nothing.
addCandidate :: S2RegionCoverer -> Candidate -> S2RegionCoverer
addCandidate = error "addCandidate"


-- Populate the children of "candidate" by expanding the given number of
-- levels from the given cell.  Returns the number of children that were
-- marked "terminal".
expandChildren :: S2RegionCoverer -> Candidate -> S2Cell -> Int -> Int
expandChildren = error "expandChildren"


-- Computes a set of initial candidates that cover the given region.
getInitialCandidates :: S2RegionCoverer -> S2RegionCoverer
getInitialCandidates = error "getInitialCandidates"


-- Generates a covering and stores it in result_.
getCoveringInternal :: S2Region region => S2RegionCoverer -> region -> S2RegionCoverer
getCoveringInternal = error "getCoveringInternal"


-- Given a region and a starting cell, return the set of all the
-- edge-connected cells at the same level that intersect "region".
-- The output cells are returned in arbitrary order.
floodFill :: S2Region region => region -> S2CellId -> [S2CellId]
floodFill = error "floodFill"


{-
-- We save a temporary copy of the pointer passed to GetCovering() in order
-- to avoid passing this parameter around internally.  It is only used (and
-- only valid) for the duration of a single GetCovering() call.
S2Region const* region_;

-- A temporary variable used by GetCovering() that holds the cell ids that
-- have been added to the covering so far.
scoped_ptr<vector<S2CellId> > result_;

-- We keep the candidates in a priority queue.  We specify a vector to hold
-- the queue entries since for some reason priority_queue<> uses a deque by
-- default.
struct CompareQueueEntries;
typedef pair<int, Candidate*> QueueEntry;
typedef priority_queue<QueueEntry, vector<QueueEntry>,
                       CompareQueueEntries> CandidateQueue;
scoped_ptr<CandidateQueue> pq_;

-- True if we're computing an interior covering.
bool interior_covering_;

-- Counter of number of candidates created, for performance evaluation.
int candidates_created_counter_;

DISALLOW_EVIL_CONSTRUCTORS(S2RegionCoverer);
-}
