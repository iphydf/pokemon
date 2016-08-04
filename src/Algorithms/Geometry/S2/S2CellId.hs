{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
-- | An S2CellId is a 64-bit unsigned integer that uniquely identifies a cell in
-- the S2 cell decomposition. It has the following format:
--
-- <pre>
-- id = [face][face_pos]
-- </pre>
--
-- face: a 3-bit number (range 0..5) encoding the cube face.
--
-- face_pos: a 61-bit number encoding the position of the center of this cell
-- along the Hilbert curve over this face (see the Wiki pages for details).
--
-- Sequentially increasing cell ids follow a continuous space-filling curve over
-- the entire sphere. They have the following properties:
--  - The id of a cell at level k consists of a 3-bit face number followed by k
-- bit pairs that recursively select one of the four children of each cell. The
-- next bit is always 1, and all other bits are 0. Therefore, the level of a
-- cell is determined by the position of its lowest-numbered bit that is turned
-- on (for a cell at level k, this position is 2 * (MAX_LEVEL - k).)
--  - The id of a parent cell is at the midpoint of the range of ids spanned by
-- its children (or by its descendants at any level).
--
-- Leaf cells are often used to represent points on the unit sphere, and this
-- class provides methods for converting directly between these two
-- representations. For cells that represent 2D regions rather than discrete
-- point, it is better to use the S2Cell class.
module Algorithms.Geometry.S2.S2CellId where

import           Control.DeepSeq                      (NFData)
import           Data.AEq                             (AEq)
import           Data.Array.Unboxed                   (UArray, listArray, (!))
import           Data.Bits                            (shiftL, shiftR, (.&.),
                                                       (.|.))
import           Data.Int                             (Int32)
import           Data.Word                            (Word32, Word64)
import           Foreign.Storable                     (Storable)
import           Test.QuickCheck.Arbitrary            (Arbitrary (..))
import qualified Test.QuickCheck.Gen                  as Gen

import           Algorithms.Geometry.S2.S2            (Face (..), Level (..))
import qualified Algorithms.Geometry.S2.S2            as S2
import           Algorithms.Geometry.S2.S2LatLng      (S2LatLng)
import qualified Algorithms.Geometry.S2.S2LatLng      as S2LatLng
import           Algorithms.Geometry.S2.S2Point       (S2Point)
import qualified Algorithms.Geometry.S2.S2Point       as S2Point
import qualified Algorithms.Geometry.S2.S2Projections as S2Projections


newtype S2CellId = S2CellId { value :: Word64 }
  deriving (Eq, AEq, Read, Show, Storable, NFData)

instance Arbitrary S2CellId where
  arbitrary = (S2CellId <$> arbitrary) `Gen.suchThat` isValid


-- | Although only 60 bits are needed to represent the index of a leaf
-- cell, we need an extra bit in order to represent the position of
-- the center of the leaf cell along the Hilbert curve.
faceBits :: Int
faceBits = 3

lookupBits :: Int
lookupBits = 4

numFaces :: Int
numFaces = 6

posBits :: Int
posBits = 2 * S2.maxCellLevel + 1

swapMask :: Word32
swapMask = 0x01

invertMask :: Word32
invertMask = 0x02


-- | The default constructor returns an invalid cell id.
none :: S2CellId
none = S2CellId 0


-- | An invalid cell id guaranteed to be larger than any valid cell id.
-- Useful for creating indexes.
sentinel :: S2CellId
sentinel = S2CellId maxBound


getFace :: S2CellId -> Face
getFace S2CellId { value } = Face $ fromIntegral $ value `shiftR` posBits


isValid :: S2CellId -> Bool
isValid cid = unFace (getFace cid) < numFaces && (lsb cid .&. 0x1555555555555555 /= 0)


-- | Return the lowest-numbered bit that is on for this cell id, which is
-- equal to (uint64(1) << (2 * (kMaxLevel - level))).  So for example,
-- a.lsb() <= b.lsb() if and only if a.level() >= b.level(), but the
-- first test is more efficient.
lsb :: S2CellId -> Word64
lsb S2CellId { value } = value .&. (-value)


-- | Return the lowest-numbered bit that is on for cells at the given level.
lsbForLevel :: Level -> Word64
lsbForLevel (Level lev) =
  1 `shiftL` (2 * (S2.maxCellLevel - lev))


parent :: S2CellId -> S2CellId
parent cid =
  let newLsb = lsb cid `shiftL` 2 in
  S2CellId ((value cid .&. (-newLsb)) .|. newLsb);


-- | Return the cell at the previous level or at the given level (which must be
-- less than or equal to the current level).
parentForLevel :: S2CellId -> Level -> S2CellId
parentForLevel cid lev =
  let newLsb = lsbForLevel lev in
  S2CellId ((value cid .&. (-newLsb)) .|. newLsb);



-- Return a cell given its face (range 0..5), 61-bit Hilbert curve position
-- within that face, and level (range 0..MAX_LEVEL). The given position will
-- be modified to correspond to the Hilbert curve position at the center of
-- the returned cell. This is a static function rather than a constructor in
-- order to give names to the arguments.
fromFacePosLevel :: Face -> Word64 -> Level -> S2CellId
fromFacePosLevel (Face face) pos =
  parentForLevel $ S2CellId $ ((fromIntegral face :: Word64) `shiftL` posBits) + (pos .|. 1)


fromLatLng :: S2LatLng -> S2CellId
fromLatLng = fromPoint . S2LatLng.toPoint


fromPoint :: S2Point -> S2CellId
fromPoint p =
  let (face, i, j) = S2Projections.xyzToFaceIJ p in
  fromFaceIJ face i j


lookupPosTable :: UArray Word32 Word32
lookupPosTable = listArray (0, 1024) [0, 1, 682, 683, 14, 4, 685, 679, 17, 58, 688, 667, 20, 61, 702, 663, 234, 64, 705, 619, 237, 78, 708, 615, 240, 81, 762, 603, 254, 84, 765, 599, 257, 938, 768, 427, 260, 941, 782, 423, 314, 944, 785, 411, 317, 958, 788, 407, 320, 961, 1002, 363, 334, 964, 1005, 359, 337, 1018, 1008, 347, 340, 1021, 1022, 343, 5, 15, 678, 684, 9, 8, 675, 674, 31, 54, 693, 668, 24, 51, 697, 658, 230, 69, 719, 620, 227, 73, 712, 610, 245, 95, 758, 604, 249, 88, 755, 594, 271, 934, 773, 428, 264, 931, 777, 418, 310, 949, 799, 412, 307, 953, 792, 402, 325, 975, 998, 364, 329, 968, 995, 354, 351, 1014, 1013, 348, 344, 1011, 1017, 338, 59, 16, 666, 689, 55, 30, 669, 692, 33, 32, 651, 650, 36, 46, 647, 653, 218, 123, 720, 625, 221, 119, 734, 628, 203, 97, 736, 586, 199, 100, 750, 589, 272, 922, 827, 433, 286, 925, 823, 436, 288, 907, 801, 394, 302, 903, 804, 397, 379, 976, 986, 369, 375, 990, 989, 372, 353, 992, 971, 330, 356, 1006, 967, 333, 60, 21, 662, 703, 50, 25, 659, 696, 47, 37, 652, 646, 40, 41, 642, 643, 214, 124, 725, 639, 211, 114, 729, 632, 204, 111, 741, 582, 194, 104, 745, 579, 277, 918, 828, 447, 281, 915, 818, 440, 293, 908, 815, 390, 297, 898, 808, 387, 380, 981, 982, 383, 370, 985, 979, 376, 367, 997, 972, 326, 360, 1001, 962, 323, 65, 235, 618, 704, 68, 231, 621, 718, 122, 219, 624, 721, 125, 215, 638, 724, 129, 128, 555, 554, 132, 142, 551, 557, 186, 145, 539, 560, 189, 148, 535, 574, 491, 874, 833, 448, 487, 877, 836, 462, 475, 880, 890, 465, 471, 894, 893, 468, 384, 811, 897, 298, 398, 807, 900, 301, 401, 795, 954, 304, 404, 791, 957, 318, 79, 236, 614, 709, 72, 226, 611, 713, 118, 220, 629, 735, 115, 210, 633, 728, 143, 133, 556, 550, 136, 137, 546, 547, 182, 159, 540, 565, 179, 152, 530, 569, 492, 870, 847, 453, 482, 867, 840, 457, 476, 885, 886, 479, 466, 889, 883, 472, 389, 812, 911, 294, 393, 802, 904, 291, 415, 796, 950, 309, 408, 786, 947, 313, 80, 241, 602, 763, 94, 244, 605, 759, 96, 202, 587, 737, 110, 205, 583, 740, 144, 187, 561, 538, 158, 183, 564, 541, 160, 161, 522, 523, 174, 164, 525, 519, 497, 858, 848, 507, 500, 861, 862, 503, 458, 843, 864, 481, 461, 839, 878, 484, 443, 817, 912, 282, 439, 820, 926, 285, 417, 778, 928, 267, 420, 781, 942, 263, 85, 255, 598, 764, 89, 248, 595, 754, 101, 198, 588, 751, 105, 195, 578, 744, 149, 188, 575, 534, 153, 178, 568, 531, 165, 175, 518, 524, 169, 168, 515, 514, 511, 854, 853, 508, 504, 851, 857, 498, 454, 844, 869, 495, 451, 834, 873, 488, 444, 831, 917, 278, 434, 824, 921, 275, 431, 774, 933, 268, 424, 771, 937, 258, 939, 256, 426, 769, 935, 270, 429, 772, 923, 273, 432, 826, 919, 276, 446, 829, 875, 490, 449, 832, 871, 493, 452, 846, 859, 496, 506, 849, 855, 510, 509, 852, 513, 512, 171, 170, 516, 526, 167, 173, 570, 529, 155, 176, 573, 532, 151, 190, 576, 746, 107, 193, 590, 749, 103, 196, 593, 752, 91, 250, 596, 766, 87, 253, 940, 261, 422, 783, 930, 265, 419, 776, 924, 287, 437, 822, 914, 280, 441, 819, 876, 486, 463, 837, 866, 483, 456, 841, 860, 501, 502, 863, 850, 505, 499, 856, 527, 517, 172, 166, 520, 521, 162, 163, 566, 543, 156, 181, 563, 536, 146, 185, 581, 742, 108, 207, 585, 739, 98, 200, 607, 757, 92, 246, 600, 761, 82, 243, 945, 315, 410, 784, 948, 311, 413, 798, 906, 289, 395, 800, 909, 292, 391, 814, 881, 474, 464, 891, 884, 477, 478, 887, 842, 459, 480, 865, 845, 455, 494, 868, 528, 571, 177, 154, 542, 567, 180, 157, 544, 545, 138, 139, 558, 548, 141, 135, 635, 730, 113, 208, 631, 733, 116, 222, 609, 715, 74, 224, 612, 711, 77, 238, 959, 316, 406, 789, 952, 306, 403, 793, 902, 303, 396, 805, 899, 296, 386, 809, 895, 470, 469, 892, 888, 467, 473, 882, 838, 460, 485, 879, 835, 450, 489, 872, 533, 572, 191, 150, 537, 562, 184, 147, 549, 559, 134, 140, 553, 552, 131, 130, 636, 726, 127, 213, 626, 723, 120, 217, 623, 716, 70, 229, 616, 706, 67, 233, 960, 321, 362, 1003, 974, 324, 365, 999, 977, 378, 368, 987, 980, 381, 382, 983, 810, 385, 299, 896, 813, 388, 295, 910, 816, 442, 283, 913, 830, 445, 279, 916, 747, 577, 192, 106, 743, 580, 206, 109, 731, 634, 209, 112, 727, 637, 212, 126, 640, 641, 42, 43, 654, 644, 45, 39, 657, 698, 48, 27, 660, 701, 62, 23, 965, 335, 358, 1004, 969, 328, 355, 994, 991, 374, 373, 988, 984, 371, 377, 978, 806, 399, 300, 901, 803, 392, 290, 905, 821, 438, 284, 927, 825, 435, 274, 920, 748, 591, 197, 102, 738, 584, 201, 99, 732, 630, 223, 117, 722, 627, 216, 121, 645, 655, 38, 44, 649, 648, 35, 34, 671, 694, 53, 28, 664, 691, 57, 18, 1019, 336, 346, 1009, 1015, 350, 349, 1012, 993, 352, 331, 970, 996, 366, 327, 973, 794, 400, 305, 955, 797, 414, 308, 951, 779, 416, 266, 929, 775, 430, 269, 932, 753, 592, 251, 90, 756, 606, 247, 93, 714, 608, 225, 75, 717, 622, 228, 71, 699, 656, 26, 49, 695, 670, 29, 52, 673, 672, 11, 10, 676, 686, 7, 13, 1020, 341, 342, 1023, 1010, 345, 339, 1016, 1007, 357, 332, 966, 1000, 361, 322, 963, 790, 405, 319, 956, 787, 409, 312, 946, 780, 421, 262, 943, 770, 425, 259, 936, 767, 597, 252, 86, 760, 601, 242, 83, 710, 613, 239, 76, 707, 617, 232, 66, 700, 661, 22, 63, 690, 665, 19, 56, 687, 677, 12, 6, 680, 681, 2, 3]


getBits :: Word64 -> Word32 -> Word32 -> Word32 -> Word32 -> Word64
getBits n i j k bits =
  let
    mask = (1 `shiftL` lookupBits) - 1
    bits1 = lookupPosTable !
      ( bits
      + (((i `shiftR` fromIntegral (k * fromIntegral lookupBits)) .&. mask) `shiftL` (lookupBits + 2))
      + (((j `shiftR` fromIntegral (k * fromIntegral lookupBits)) .&. mask) `shiftL` 2)
      )

    op = fromIntegral $ (bits1 `shiftR` 2) `shiftL` fromIntegral ((k .&. 3) * 2 * fromIntegral lookupBits)
    n1 =
      case k `shiftR` 2 of
        0 -> n .|. op
        _ -> n .|. (op `shiftL` 32)

    bits2 = bits1 .&. (swapMask .|. invertMask)
  in

  case k of
    0 -> n1
    _ -> getBits n1 i j (k - 1) bits2


-- Return a leaf cell given its cube face (range 0..5) and i- and
-- j-coordinates (see s2.h).
fromFaceIJ :: Face -> Int -> Int -> S2CellId
fromFaceIJ (Face face') i' j' =
  let face = fromIntegral face' in
  let i = fromIntegral i' in
  let j = fromIntegral j' in

  let n0 = fromIntegral face `shiftL` (posBits - 1) in

  -- Alternating faces have opposite Hilbert curve orientations; this
  -- is necessary in order for all faces to have a right-handed
  -- coordinate system.
  let bits = face .&. swapMask in

  -- Each iteration maps 4 bits of "i" and "j" into 8 bits of the Hilbert
  -- curve position. The lookup table transforms a 10-bit key of the form
  -- "iiiijjjjoo" to a 10-bit value of the form "ppppppppoo", where the
  -- letters [ijpo] denote bits of "i", "j", Hilbert curve position, and
  -- Hilbert curve orientation respectively.

  let n8 = getBits n0 i j 7 bits in

  S2CellId $ (n8 `shiftL` 1) + 1


isLeaf :: S2CellId -> Bool
isLeaf S2CellId { value } = value .&. 1 /= 0


-- Return the subdivision level of the cell (range 0..kMaxLevel).
level :: S2CellId -> Int
level cid
  -- Fast path for leaf cells.
  | isLeaf cid = S2.maxCellLevel
  | otherwise =
      let
        level0 = -1
        level1 = if x0 /= 0 then level0 + 16 else level0
        x1 = if x0 /= 0 then x0 else fromIntegral $ value cid `shiftR` 32
        -- We only need to look at even-numbered bits to determine the
        -- level of a valid cell id.
        x2 = x1 .&. (-x1)  -- Get lowest bit.

        level2 = if x2 .&. 0x00005555 /= 0 then level1 + 8 else level1
        level3 = if x2 .&. 0x00550055 /= 0 then level2 + 4 else level2
        level4 = if x2 .&. 0x05050505 /= 0 then level3 + 2 else level3
        level5 = if x2 .&. 0x11111111 /= 0 then level4 + 1 else level4
      in
      level5
  where
    x0 :: Word32
    x0 = fromIntegral $ value cid


children :: S2CellId -> [S2CellId]
children = undefined


childrenForLevel :: Int -> S2CellId -> [S2CellId]
childrenForLevel = undefined
