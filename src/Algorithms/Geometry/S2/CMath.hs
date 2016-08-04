module Algorithms.Geometry.S2.CMath where

import           Foreign.C.Types (CDouble (..))


-- | scalb(x, n) returns x*(2**n) computed by exponent manipulation.
scalb :: Double -> Double -> Double
scalb x y = realToFrac (c_scalb (realToFrac x) (realToFrac y))
{-# INLINE scalb #-}

foreign import ccall unsafe "math.h scalb"
   c_scalb :: CDouble -> CDouble -> CDouble


-- | The remainder function computes the floating-point remainder of x \/ y.
--
remainder :: Double -> Double -> Double
remainder x y = realToFrac (c_remainder (realToFrac x) (realToFrac y))
{-# INLINE remainder #-}

foreign import ccall unsafe "math.h remainder"
   c_remainder :: CDouble -> CDouble -> CDouble
