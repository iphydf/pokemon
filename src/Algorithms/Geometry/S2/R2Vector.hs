module Algorithms.Geometry.S2.R2Vector where

import           Data.AEq         (AEq, (~==))
import           Foreign.Ptr      (castPtr)
import           Foreign.Storable (Storable (..))


data R2Vector = R2Vector { x, y :: Double }
  deriving (Eq, Show, Read)

instance AEq R2Vector where
  a ~== b =
    x a ~== x b &&
    y a ~== y b

instance Storable R2Vector where
  sizeOf _ = sizeOf (x undefined) * 2
  alignment _ = alignment (x undefined)

  peek ptr = R2Vector
    <$> peekElemOff (castPtr ptr) 0
    <*> peekElemOff (castPtr ptr) 1

  poke ptr p = do
    pokeElemOff (castPtr ptr) 0 (x p)
    pokeElemOff (castPtr ptr) 1 (y p)
