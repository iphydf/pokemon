{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pokemon.Encrypt
  ( expectedIvSize
  , IV
  , nullIV
  , ivToBS
  , SessionHash
  , sessionHashToBS
  , PlainText (..)
  , CipherText (..)
  , encryptIO
  , encryptCleanIO
  , encrypt
  , decryptIO
  , decrypt
  , xxHash32IO
  , xxHash32
  , xxHash64IO
  , xxHash64
  , sub9E9D8
  , Block (..)
  , blockSize
  ) where

import           Control.Arrow             (first)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Base16    as Base16
import           Data.String               (IsString)
import           Data.Word                 (Word32, Word64)
import           Foreign.C.String          (CString)
import           Foreign.C.Types           (CSize (..), CUInt (..),
                                            CULLong (..))
import           Foreign.Marshal.Alloc     (alloca, allocaBytes)
import           Foreign.Marshal.Utils     (with)
import           Foreign.Ptr               (Ptr, nullPtr)
import           Foreign.Storable          (peek)
import           System.IO.Unsafe          (unsafePerformIO)
import           System.Random             (Random (..))
import           Test.QuickCheck.Arbitrary (Arbitrary (..))
import qualified Test.QuickCheck.Gen       as Gen
import           Text.Read                 (readPrec)

{-
extern int encrypt(const unsigned char *input, size_t input_size,
	const unsigned char* iv, size_t iv_size,
	unsigned char* output, size_t * output_size);
-}
type EncryptFn
  =  CString   -- ^ const unsigned char *input
  -> CSize     -- ^ size_t input_size
  -> CString   -- ^ const unsigned char *iv
  -> CSize     -- ^ size_t iv_size
  -> CString   -- ^ unsigned char *output
  -> Ptr CSize -- ^ size_t *output_size
  -> IO Int

foreign import ccall "encrypt" c_encrypt :: EncryptFn
foreign import ccall "encrypt_clean" c_encrypt_clean :: EncryptFn

foreign import ccall "decrypt" c_decrypt :: CString -> CSize -> CString -> Ptr CSize -> IO ()

foreign import ccall unsafe "xxhash.h XXH32"
  c_XXH32 :: CString -- ^ Data
          -> CSize   -- ^ Size
          -> CUInt   -- ^ Seed
          -> IO CUInt

foreign import ccall unsafe "xxhash.h XXH64"
  c_XXH64 :: CString -- ^ Data
          -> CSize   -- ^ Size
          -> CULLong -- ^ Seed
          -> IO CULLong


sessionHashSize :: Int
sessionHashSize = 16


newtype SessionHash = SessionHash { sessionHashToBS :: BS.ByteString }
  deriving (Eq)

instance Show SessionHash where
  show = show . Base16.encode . sessionHashToBS

instance Read SessionHash where
  readPrec = do
    bs <- fst . Base16.decode <$> readPrec
    if BS.length bs /= sessionHashSize
      then fail $ "session hash length invalid: " ++ show (BS.length bs)
      else return $ SessionHash bs

instance Arbitrary SessionHash where
  arbitrary = SessionHash . BS.pack <$> Gen.vectorOf sessionHashSize arbitrary

instance Random SessionHash where
  randomR = fail "range-constrained random is not supported for session hash"

  random g =
    first (SessionHash . BS.pack)
      $ foldl
          (\(l, g') _ -> let (e, g'') = random g' in (e:l, g''))
          ([], g)
          [1..sessionHashSize]


expectedIvSize :: Int
expectedIvSize = 32


newtype IV = IV { ivToBS :: BS.ByteString }
  deriving (Eq)

instance Show IV where
  show = show . Base16.encode . ivToBS

instance Read IV where
  readPrec = do
    bs <- fst . Base16.decode <$> readPrec
    if BS.length bs /= expectedIvSize
      then fail $ "IV length invalid: " ++ show (BS.length bs)
      else return $ IV bs

instance Arbitrary IV where
  arbitrary = IV . BS.pack <$> Gen.vectorOf expectedIvSize arbitrary

instance Random IV where
  randomR = fail "range-constrained random is not supported for IVs"

  random g =
    first (IV . BS.pack)
      $ foldl
          (\(l, g') _ -> let (e, g'') = random g' in (e:l, g''))
          ([], g)
          [1..expectedIvSize]


nullIV :: IV
nullIV = IV . BS.pack . replicate 32 $ 0


newtype PlainText = PlainText { unPlainText :: BS.ByteString }
  deriving (Eq, IsString)

instance Show PlainText where
  show = show . Base16.encode . unPlainText

instance Read PlainText where
  readPrec = PlainText . fst . Base16.decode <$> readPrec

instance Arbitrary PlainText where
  arbitrary = PlainText . BS.pack <$> arbitrary


newtype CipherText = CipherText { unCipherText :: BS.ByteString }
  deriving (Eq, IsString)

instance Show CipherText where
  show = show . Base16.encode . unCipherText

instance Read CipherText where
  readPrec = CipherText . fst . Base16.decode <$> readPrec


callEncryptIO :: EncryptFn -> IV -> PlainText -> IO CipherText
callEncryptIO f (IV iv) (PlainText input) =
  BS.useAsCStringLen input $ \(input', inputLen) -> do
    let inputSize = fromIntegral inputLen
    BS.useAsCStringLen iv $ \(iv', ivLen) -> do
      let ivSize = fromIntegral ivLen
      alloca $ \size -> do
        ok1 <- f input' inputSize iv' ivSize nullPtr size
        if ok1 == -1
          then fail "failed to determine size"
          else do
            outputSize <- peek size
            allocaBytes (fromIntegral outputSize) $ \output' ->
              with outputSize $ \outputSize' -> do
                ok2 <- f input' inputSize iv' ivSize output' outputSize'
                if ok2 == -1
                  then fail "failed to encrypt"
                  else do
                    totalSize <- peek outputSize'
                    CipherText <$> BS.packCStringLen (output', fromIntegral totalSize)


encryptIO :: IV -> PlainText -> IO CipherText
encryptIO = callEncryptIO c_encrypt

encryptCleanIO :: IV -> PlainText -> IO CipherText
encryptCleanIO = callEncryptIO c_encrypt_clean


decryptIO :: CipherText -> IO PlainText
decryptIO (CipherText bs) =
  BS.useAsCStringLen bs $ \(input', inputLen) ->
    alloca $ \size -> do
      let inputSize = fromIntegral inputLen
      c_decrypt input' inputSize nullPtr size
      outputSize <- peek size
      allocaBytes (fromIntegral outputSize) $ \output' ->
        with outputSize $ \outputSize' -> do
          c_decrypt input' inputSize output' outputSize'
          totalSize <- peek outputSize'
          PlainText <$> BS.packCStringLen (output', fromIntegral totalSize)


xxHashIO :: (Integral a, Integral seed, Integral r)
         => (CString -> CSize -> a -> IO a) -> seed -> PlainText -> IO r
xxHashIO f seed (PlainText bs) =
  fromIntegral <$>
    BS.useAsCStringLen bs (\(str, len) ->
      f str (fromIntegral len) (fromIntegral seed))


xxHash32IO :: Word32 -> PlainText -> IO Word32
xxHash32IO = xxHashIO c_XXH32

xxHash64IO :: Word64 -> PlainText -> IO Word64
xxHash64IO = xxHashIO c_XXH64


xxHash32 :: Word32 -> PlainText -> Word32
xxHash32 seed = unsafePerformIO . xxHash32IO seed

xxHash64 :: Word64 -> PlainText -> Word64
xxHash64 seed = unsafePerformIO . xxHash64IO seed

encrypt :: IV -> PlainText -> BS.ByteString
encrypt iv = unCipherText . unsafePerformIO . encryptIO iv

decrypt :: CipherText -> BS.ByteString
decrypt = unPlainText . unsafePerformIO . decryptIO


--------------------------------------------------------------------------------
--
-- :: sub9E9D8
--
--------------------------------------------------------------------------------


blockSize :: Int
blockSize = 256


newtype Block = Block { unBlock :: BS.ByteString }
  deriving (Eq, IsString)

instance Show Block where
  show = show . Base16.encode . unBlock

instance Read Block where
  readPrec = Block . fst . Base16.decode <$> readPrec

instance Arbitrary Block where
  arbitrary = Block . BS.pack <$> Gen.vectorOf blockSize arbitrary

foreign import ccall "clean_sub_9E9D8" c_sub_9E9D8 :: CString -> CString -> IO ()

sub9E9D8 :: Block -> IO BS.ByteString
sub9E9D8 (Block input)
  | BS.length input /= blockSize = fail "invalid length"
  | otherwise =
      BS.useAsCString input $ \input' ->
        allocaBytes blockSize $ \output' -> do
          c_sub_9E9D8 input' output'
          BS.packCStringLen (output', blockSize)
