{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-float-in #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

#if __GLASGOW_HASKELL__ >= 705
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#define USE_TYPE_LITS 1
#endif

#if __GLASGOW_HASKELL__ >= 707
#define USE_NEW_TYPE_LITS 1
#endif

#if __GLASGOW_HASKELL__ >= 705 && __GLASGOW_HASKELL__ < 707
#define USE_OLD_TYPE_LITS 1
#endif

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013-2015
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.HyperLogLog.Config
  (
  -- * Config
    numBuckets
  , smallRange
  , interRange
  , rawFact
  , alpha
  , bucketMask
  -- * Rank
  , Rank
  , calcBucket
  , calcRank
  , lim32
  ) where

import Data.Binary
import Data.Bits
import Data.Bits.Extras
import Data.Vector.Serialize ()
import GHC.Int
#if __GLASGOW_HASKELL__ < 710
import GHC.Word
#endif

type Rank = Int8

------------------------------------------------------------------------------
-- Config
------------------------------------------------------------------------------

lim32 :: Double
lim32 = fromInteger (bit 32)
{-# INLINE lim32 #-}

numBuckets :: Integer -> Int
numBuckets b = unsafeShiftL 1 (fromIntegral b)
{-# INLINE numBuckets #-}

smallRange :: Integer -> Double
smallRange b = 5/2 * fromIntegral (numBuckets b)
{-# INLINE smallRange #-}

interRange :: Double
interRange = lim32 / 30
{-# INLINE interRange #-}

rawFact :: Integer -> Double
rawFact b = alpha b * m * m where
  m = fromIntegral (numBuckets b)
{-# INLINE rawFact #-}

alpha :: Integer -> Double
alpha b = 0.7213 / (1 + 1.079 / fromIntegral (numBuckets b))
{-# INLINE alpha #-}

bucketMask :: Integer -> Word32
bucketMask b = fromIntegral (numBuckets b) - 1
  
------------------------------------------------------------------------------
-- Util
------------------------------------------------------------------------------

calcBucket :: Integer -> Word32 -> Int
calcBucket t w = fromIntegral (w .&. bucketMask t)
{-# INLINE calcBucket #-}

calcRank :: Integer -> Word32 -> Int8
calcRank t w = fromIntegral $ rank $ shiftR w $ fromIntegral t
{-# INLINE calcRank #-}
