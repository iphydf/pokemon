{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foreign.Cpp where

import           Data.Foldable         (forM_)
import qualified Data.Maybe            as Maybe
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Marshal.Utils (with)
import           Foreign.Ptr           (Ptr, castPtr)
import           Foreign.Storable      (Storable (..))


instance Storable a => Storable (Maybe a) where
  sizeOf a = sizeOf (Maybe.fromJust a) + sizeOf (undefined :: Bool)
  alignment a = alignment (Maybe.fromJust a)

  peek ptr = do
    a <- peek (castPtr ptr)
    isJust <- peekByteOff (castPtr ptr) (sizeOf a)
    return $ if isJust
      then Just a
      else Nothing

  poke ptr a = do
    pokeByteOff (castPtr ptr) (sizeOf (Maybe.fromJust a)) (Maybe.isJust a)
    forM_ a (poke (castPtr ptr))


ffi0 f =
  alloca $ \r ->
    f r >> peek r

ffi1 f a1 =
  alloca $ \r ->
  with a1 $ \pa1 ->
    f r pa1 >> peek r

ffi2 f a1 a2 =
  alloca $ \r ->
  with a1 $ \pa1 ->
  with a2 $ \pa2 ->
    f r pa1 pa2 >> peek r

ffi3 f a1 a2 a3 =
  alloca $ \r ->
  with a1 $ \pa1 ->
  with a2 $ \pa2 ->
  with a3 $ \pa3 ->
    f r pa1 pa2 pa3 >> peek r

ffi6 f a1 a2 a3 a4 a5 a6 =
  alloca $ \r ->
  with a1 $ \pa1 ->
  with a2 $ \pa2 ->
  with a3 $ \pa3 ->
  with a4 $ \pa4 ->
  with a5 $ \pa5 ->
  with a6 $ \pa6 ->
    f r pa1 pa2 pa3 pa4 pa5 pa6 >> peek r
