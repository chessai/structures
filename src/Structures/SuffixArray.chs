{-# language BangPatterns #-}
{-# language CPP #-}
{-# language ForeignFunctionInterface #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MagicHash #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

-- | Suffix Arrays, using Yuta Mori's SAIS algorithm
-- https://sites.google.com/site/yuta256/sais
module Structures.SuffixArray
  (
  ) where

import Control.Monad.ST (ST, runST)
import Data.Char (ord,chr)
import Data.Primitive.Array (Array, MutableArray, unsafeFreezeArray)
import Data.Primitive.Contiguous (Contiguous,Element,Mutable)
import Data.Primitive.PrimArray (PrimArray,MutablePrimArray)
import Data.String (IsString(..))
import Data.Word (Word8)
import GHC.Classes (divInt#)
import GHC.Exts (Int(..))
import qualified Data.Primitive.Contiguous as C
import qualified Data.Primitive.Sort as C

#include "sais.h"

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

