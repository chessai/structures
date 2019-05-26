{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MagicHash #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

-- WIP
-- Impl: http://algo2.iti.kit.edu/documents/jacm05-revised.pdf
module Structures.SuffixArray
  ( --SuffixArray(..)
--  , Suffix(..)
--  , String8(..)
  ) where

import Control.Monad.ST (ST, runST)
import Data.Char (ord,chr)
import Data.Primitive.Array (Array, MutableArray, unsafeFreezeArray)
import Data.Primitive.PrimArray (PrimArray,MutablePrimArray)
import Data.String (IsString(..))
import Data.Word (Word8)
import qualified Data.Primitive.Contiguous as C
import qualified Data.Primitive.Sort as C
import Data.Primitive.Contiguous (Contiguous,Element,Mutable)
import GHC.Exts (Int(..))
import GHC.Classes (divInt#)

newtype String8 = String8 { getString8 :: PrimArray Word8 }
  deriving newtype (Eq,Ord)

instance Show String8 where
  show str = "pack " ++ show (unpack str)

unpack :: String8 -> [Char]
unpack = map w2c . C.toList . getString8

pack :: [Char] -> String8
pack = fromString

c2w :: Char -> Word8
c2w = fromIntegral . ord

w2c :: Word8 -> Char
w2c = chr . fromIntegral

primArrayToString :: PrimArray Word8 -> String
primArrayToString = map w2c . C.toList

instance IsString String8 where
  fromString = String8 . C.fromList . map c2w

{-
substring :: String8 -> Int -> Int -> String8
substring (String8 str) !from !to = String8 $ C.clone str from to

data Suffix = Suffix
  !Int -- ^ index
  !String8 -- ^ suffix
  deriving stock (Eq,Show)

instance Ord Suffix where
  compare (Suffix _ suf1) (Suffix _ suf2) = compare suf1 suf2

newtype SuffixArray = SuffixArray { getSuffixArray :: Array Suffix }
  deriving newtype (Eq,Ord,Show)
-}

leqPairs :: Int -> Int -> Int -> Int -> Bool
leqPairs a1 a2 b1 b2 = a1 < b1 || a1 == b1 && a2 <= b2

leqTriples :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
leqTriples a1 a2 a3 b1 b2 b3 = a1 < b1 || a1 == b1 && leqPairs a2 a3 b2 b3
-- | Stably sort a[0..n-1] to b[0..n-1] with keys in 0..K from r
radixPass :: forall arr s. (Contiguous arr, Element arr Int)
  => Mutable arr s Int -- ^ a[0..n-1]
  -> Mutable arr s Int -- ^ b[0..n-1]
  -> Mutable arr s Int -- ^ r
  -> Int -- ^ n
  -> Int -- ^ k
  -> ST s ()
radixPass a b r n k = do
  c :: Mutable arr s Int <- C.generateMutable (k + 1) $ (const 0) -- reset counters
  let go1 !ix = if ix < n
        then do
          atIx <- C.read c =<< C.read r =<< C.read a ix
          C.write c ix (atIx + 1)
          go1 (ix+1)
        else pure ()
  go1 0 -- count occurrences
  let go2 !ix !sum = if ix <= k
        then do
          t <- C.read c ix
          C.write c ix sum
          go2 (ix+1) (sum+t)
        else pure ()
  go2 0 0 -- exclusive prefix sums
  let go3 !ix = if ix < n
        then do
          atIxA <- C.read a ix
          atIxC <- C.read c =<< C.read r =<< C.read a ix
          C.write c ix (atIxC + 1)
          C.write b ix atIxA
        else pure ()
  go3 0 -- sort

suffixArray :: forall arr s. (Contiguous arr, Element arr Int)
  => Mutable arr s Int -- ^ T
  -> Mutable arr s Int -- ^ SA
  -> Int -- ^ n
  -> Int -- ^ k
  -> ST s ()
suffixArray t sa n k = do
  let n0 = (n + 2) `div` 3
      n1 = (n + 1) `div` 3
      n2 = n `div` 3
      n02 = n0 + n2
  r :: Mutable arr s Int <- C.new (n02 + 3)
  C.write r n02 0; C.write r (n02 + 1) 0; C.write r (n02 + 2) 0;
  sa12 :: Mutable arr s Int <- C.new (n02 + 3);
  C.write sa12 n02 0; C.write sa12 (n02 + 1) 0; C.write sa12 (n02+2) 0;
  r0 :: Mutable arr s Int <- C.new n0;
  sa0 :: Mutable arr s Int <- C.new n0;

  -- step 0: construct sample
  -- generate positions of mod 1 and mod 2 suffixes
  -- the "+(n0-n1) adds a dummy mod 1 suffix of n % 3 == 1
  let constructSample !i !j = if i < n + (n0 - n1)
        then if (mod i 3 /= 0)
          then do
            C.write r j i
            constructSample (i+1) (j+1)
          else constructSample (i+1) j
        else pure ()
  constructSample 0 0

  -- step 1: sort sample suffixes
  -- lsb radix sort the mod 1 and mod 2 triples
  radixPass r sa12 (
  -- find lexicographic names of triples and
  -- write them to correct places in R
  let name = 0; c0 = -1; c1 = -1; c2 = -1;

  pure ()

--unsafeDiv :: Int -> Int -> Int
--unsafeDiv (I# x) (I# y) = I# (x `divInt#` y)

