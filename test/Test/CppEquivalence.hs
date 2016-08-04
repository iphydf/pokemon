{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.CppEquivalence where

import           Control.Monad   (unless)
import           Data.AEq        (AEq, (~==))
import           Foreign.Cpp
import           Test.Hspec
import           Test.QuickCheck


shouldBeApprox :: (AEq a, Eq a, Show a) => a -> a -> Expectation
shouldBeApprox a b =
  unless (a ~== b) $
    a `shouldBe` b


equiv0 name f1 f2 =
  it ("is equivalent to its C++ implementation: " ++ name) $ do
    r <- ffi0 f2
    f1 `shouldBeApprox` r

equiv1 name f1 f2 =
  it ("is equivalent to its C++ implementation: " ++ name) $
    property $ \a1 -> do
      r <- ffi1 f2 a1
      f1 a1 `shouldBeApprox` r

equiv2 name f1 f2 =
  it ("is equivalent to its C++ implementation: " ++ name) $
    property $ \a1 a2 -> do
      r <- ffi2 f2 a1 a2
      f1 a1 a2 `shouldBeApprox` r

equiv3 name f1 f2 =
  it ("is equivalent to its C++ implementation: " ++ name) $
    property $ \a1 a2 a3 -> do
      r <- ffi3 f2 a1 a2 a3
      f1 a1 a2 a3 `shouldBeApprox` r

equiv6 name f1 f2 =
  it ("is equivalent to its C++ implementation: " ++ name) $
    property $ \a1 a2 a3 a4 a5 a6 -> do
      r <- ffi6 f2 a1 a2 a3 a4 a5 a6
      f1 a1 a2 a3 a4 a5 a6 `shouldBeApprox` r
