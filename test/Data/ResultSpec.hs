{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE Trustworthy #-}
module Data.ResultSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Applicative (pure, (<*>))
import qualified Data.Result         as R


-- | Checks that 'R.Result' satisfies the laws described in the 'Monad' and
-- 'Applicative' documentation.
--
-- Also see:
-- https://wiki.haskell.org/Monad_laws
-- https://hackage.haskell.org/package/base-4.9.0.0/docs/Prelude.html#t:Applicative
spec :: Spec
spec = do
  describe "Monad" $ do
    it "satisfies left identity" $
      property $ \a -> (return' a `bind'` f) `shouldBe` f a

    it "satisfies right identity" $
      property $ \m -> (m `bind'` return') `shouldBe` m

    it "satisfies associativity" $
      property $ \m -> ((m `bind'` f) `bind'` g) `shouldBe` (m `bind'` (\x -> f x `bind'` g))

  describe "Applicative" $ do
    it "satisfies identity" $
      property identity

    it "satisfies composition" $
      property $ \x y w -> do
        composition (R.Failure "nope") (R.Failure "no way") w
        composition (R.Failure "nope") (pure (y *)        ) w
        composition (pure (x *)      ) (R.Failure "no way") w
        composition (pure (x *)      ) (pure (y *)        ) w

    it "satisfies homomorphism" $
      property $ \x -> homomorphism (x *)

    it "satisfies interchange" $
      property $ \x y -> do
        interchange (R.Failure "nope") y
        interchange (pure (x *)      ) y

  where
    --
    -- Aliases constrained to the Result monad. These also help avoid lint
    -- warnings about using monad laws.
    --

    return' :: Int -> R.Result Int
    return' = return

    bind' :: R.Result Int -> (Int -> R.Result Int) -> R.Result Int
    bind' = (>>=)

    pure' :: a -> R.Result a
    pure' = pure

    --
    -- Applicative laws.
    --

    identity :: R.Result Int -> Expectation
    identity v =
      (pure' id <*> v) `shouldBe` v

    composition :: R.Result (Int -> Int) -> R.Result (Int -> Int) -> R.Result Int -> Expectation
    composition u v w =
      (pure' (.) <*> u <*> v <*> w) `shouldBe` (u <*> (v <*> w))

    homomorphism :: (Int -> Int) -> Int -> Expectation
    homomorphism h x =
      (pure' h <*> pure' x) `shouldBe` pure' (h x)

    interchange :: R.Result (Int -> Int) -> Int -> Expectation
    interchange u y =
      (u <*> pure' y) `shouldBe` (pure' ($ y) <*> u)

    --
    -- Functions f and g used in Monad laws.
    --

    f = \case
      x | x `mod` 2 == 0 -> R.Success $ x * 123
      _                  -> R.Failure "Holla"

    g = \case
      x | x `mod` 2 == 0 -> R.Failure "Oh noes"
      x                  -> R.Success $ x * 234
