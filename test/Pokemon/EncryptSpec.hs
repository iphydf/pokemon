{-# LANGUAGE OverloadedStrings #-}
module Pokemon.EncryptSpec where

import           Control.Monad   (when)
import qualified Data.ByteString as BS
import           System.Random   (randomIO)
import           Test.Hspec
import           Test.QuickCheck

import qualified Pokemon.Encrypt as Encrypt


spec :: Spec
spec = do
  describe "sub9E9D8" $ do
    it "is not its own inverse 1" $ do
      let input = Encrypt.Block $ BS.pack [0..255]
      output1 <- Encrypt.sub9E9D8 input
      output2 <- Encrypt.sub9E9D8 (Encrypt.Block output1)
      output3 <- Encrypt.sub9E9D8 (Encrypt.Block output2)
      --putStrLn $ "x       = " ++ show input
      --putStrLn $ "f(x)    = " ++ show (Encrypt.Block output1)
      --putStrLn $ "f(f(x)) = " ++ show (Encrypt.Block output2)
      --putStrLn $ "f^3(x)  = " ++ show (Encrypt.Block output3)
      output1 `shouldNotBe` output2
      output1 `shouldNotBe` output3
      output2 `shouldNotBe` output3

    it "is not its own inverse" $
      property $ \input -> do
        encrypted <- Encrypt.sub9E9D8 input
        decrypted <- Encrypt.sub9E9D8 (Encrypt.Block encrypted)
        encrypted `shouldNotBe` decrypted


  describe "encrypt" $ do
    it "produces output whose length depends on the input length" $
      property $ \iv input -> do
        Encrypt.CipherText encrypted <- Encrypt.encryptIO iv input
        let inputSize = BS.length (Encrypt.unPlainText input)
        let totalSize = inputSize + Encrypt.blockSize - (inputSize `rem` Encrypt.blockSize) + Encrypt.expectedIvSize
        BS.length encrypted `shouldBe` totalSize

    it "does not lose information" $ do
      let input = Encrypt.PlainText $ BS.pack [0..254]
      encrypted <- Encrypt.encryptIO Encrypt.nullIV input
      putStrLn $ "x    = " ++ show input
      putStrLn $ "f(x) = " ++ show encrypted
      BS.length (Encrypt.unCipherText encrypted) `shouldBe` Encrypt.blockSize + Encrypt.expectedIvSize

    it "is a pure function" $
      property $ \iv input -> do
        encrypted1 <- Encrypt.encryptIO iv input
        encrypted2 <- Encrypt.encryptIO iv input
        encrypted1 `shouldBe` encrypted2

    it "starts with the IV" $
      property $ \iv input -> do
        Encrypt.CipherText encrypted <- Encrypt.encryptIO iv input
        BS.unpack encrypted `shouldStartWith` BS.unpack (Encrypt.ivToBS iv)

    it "has no collisions for the same IV" $
      property $ \iv input1 input2 -> when (input1 /= input2) $ do
        encrypted1 <- Encrypt.encryptIO iv input1
        encrypted2 <- Encrypt.encryptIO iv input2
        encrypted1 `shouldNotBe` encrypted2

    it "has no collisions for the same input (different IV)" $
      property $ \iv1 iv2 input -> when (iv1 /= iv2) $ do
        encrypted1 <- Encrypt.encryptIO iv1 input
        encrypted2 <- Encrypt.encryptIO iv2 input
        encrypted1 `shouldNotBe` encrypted2

    it "has no collisions for different input and IV" $
      property $ \iv1 iv2 input1 input2 -> when (iv1 /= iv2 && input1 /= input2) $ do
        encrypted1 <- Encrypt.encryptIO iv1 input1
        encrypted2 <- Encrypt.encryptIO iv2 input2
        encrypted1 `shouldNotBe` encrypted2

    it "works like the ASM version" $
      property $ \iv input -> do
        encrypted1 <- Encrypt.encryptCleanIO iv input
        encrypted2 <- Encrypt.encryptIO iv input
        encrypted1 `shouldBe` encrypted2


  describe "random" $
    it "generates 32 byte IVs" $ do
      iv <- randomIO
      BS.length (Encrypt.ivToBS iv) `shouldBe` Encrypt.expectedIvSize


  describe "xxHash32" $ do
    it "is a pure function" $
      property $ \seed input -> do
        hash1 <- Encrypt.xxHash32IO seed input
        hash2 <- Encrypt.xxHash32IO seed input
        hash1 `shouldBe` hash2

    it "does the same as the Python version" $ do
      hash <- Encrypt.xxHash32IO 0x1B845238 "abcde"
      hash `shouldBe` 327543353


  describe "xxHash64" $
    it "is a pure function" $
      property $ \seed input -> do
        hash1 <- Encrypt.xxHash64IO seed input
        hash2 <- Encrypt.xxHash64IO seed input
        hash1 `shouldBe` hash2
