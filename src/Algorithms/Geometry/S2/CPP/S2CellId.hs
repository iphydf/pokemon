module Algorithms.Geometry.S2.CPP.S2CellId where

import           Data.Word                       (Word32, Word64)
import           Foreign.Ptr                     (Ptr)

import           Algorithms.Geometry.S2.S2       (Face, Level)
import           Algorithms.Geometry.S2.S2CellId (S2CellId)
import qualified Algorithms.Geometry.S2.S2CellId as S2CellId
import           Algorithms.Geometry.S2.S2LatLng (S2LatLng)
import           Algorithms.Geometry.S2.S2Point  (S2Point)


foreign import ccall c_S2CellId_lsb               :: Ptr Word64 -> Ptr S2CellId -> IO ()
foreign import ccall c_S2CellId_lsbForLevel       :: Ptr Word64 -> Ptr Level -> IO ()
foreign import ccall c_S2CellId_parent            :: Ptr S2CellId -> Ptr S2CellId -> IO ()
foreign import ccall c_S2CellId_parentForLevel    :: Ptr S2CellId -> Ptr S2CellId -> Ptr Level -> IO ()
foreign import ccall c_S2CellId_fromFacePosLevel  :: Ptr S2CellId -> Ptr Face -> Ptr Word64 -> Ptr Level -> IO ()
foreign import ccall c_S2CellId_fromFaceIJ        :: Ptr S2CellId -> Ptr Face -> Ptr Int -> Ptr Int -> IO ()
foreign import ccall c_S2CellId_fromPoint         :: Ptr S2CellId -> Ptr S2Point -> IO ()
foreign import ccall c_S2CellId_fromLatLng        :: Ptr S2CellId -> Ptr S2LatLng -> IO ()
