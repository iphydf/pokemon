{-# LANGUAGE OverloadedStrings #-}
module Pokemon.EnvelopeSpec where

import           Control.Monad            (when)
import qualified Data.ByteString          as BS
import           Data.ProtoLens           (decodeMessage, showMessage)
import           Data.ProtoLens.Arbitrary (unArbitraryMessage)
import           Test.Hspec
import           Test.QuickCheck

import qualified Pokemon.Encrypt          as Encrypt
import qualified Pokemon.Envelope         as Envelope
import qualified Pokemon.Location         as Location
import qualified Pokemon.Proto            as Proto


spec :: Spec
spec = do
  describe "locationBytes" $
    it "does the same as the Python version" $ do
      Envelope.locationBytes (Location.fromLatLngAlt 1 1 1) `shouldBe` read "\"3ff00000000000003ff00000000000003ff0000000000000\""
      Envelope.locationBytes (Location.fromLatLngAlt 2 4 8) `shouldBe` read "\"400000000000000040100000000000004020000000000000\""

  describe "generateLocation1" $
    it "does the same as the Python version" $ do
      Envelope.generateLocation1 "abcde" (Location.fromLatLngAlt 1 1 1) `shouldBe` 1413111994
      Envelope.generateLocation1 "abcde" (Location.fromLatLngAlt 2 4 8) `shouldBe` 2262068850

  describe "generateLocation2" $
    it "does the same as the Python version" $
      Envelope.generateLocation2 (Location.fromLatLngAlt 1 1 1) `shouldBe` 4239058444

  describe "decodeSignature" $
    it "can decode signatures made by encodeSignature" $
      property $ \iv sig' ->
        let sig = unArbitraryMessage sig' in
        Envelope.decodeSignature (Envelope.encodeSignature iv sig) `shouldBe` Right sig
