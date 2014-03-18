{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-float-in #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds                 #-}
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE RoleAnnotations #-}
#endif

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This package provides an approximate streaming (constant space)
-- unique object counter.
--
-- See the original paper for details:
-- <http://algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf>
--------------------------------------------------------------------
module Data.HyperLogLog.Type
  (
  -- * HyperLogLog
    HyperLogLog(..)
  , HasHyperLogLog(..)
  , size
  , insert
  , intersectionSize
  , cast
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Approximate.Type
import           Data.Bits
import           Data.Bits.Extras
import           Data.Hashable
import           Data.HyperLogLog.Config
import           Data.Proxy
import           Data.Semigroup
import           Data.Serialize
import qualified Data.Vector.Unboxed                           as V
import qualified Data.Vector.Unboxed.Mutable                   as MV
import           Generics.Deriving                             hiding (D, to)
import           GHC.Int

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :load Data.HyperLogLog
-- >>> import Control.Lens
-- >>> import Data.Reflection
-- >>> import Data.Monoid

------------------------------------------------------------------------------
-- HyperLogLog
------------------------------------------------------------------------------

-- |
--
-- Initialize a new counter:
--
-- >>> mempty :: HyperLogLog $(3)
-- HyperLogLog {runHyperLogLog = fromList [0,0,0,0,0,0,0,0]}
--
-- Please note how you specify a counter size with the @$(n)@
-- invocation. Sizes of up to 16 are valid, with 7 being a
-- likely good minimum for decent accuracy.
--
-- Let's count a list of unique items and get the latest estimate:
--
-- >>> size (foldr insert mempty [1..10] :: HyperLogLog $(4))
-- Approximate {_confidence = 0.9972, _lo = 2, _estimate = 11, _hi = 20}
--
-- Note how 'insert' can be used to add new observations to the
-- approximate counter.
newtype HyperLogLog p = HyperLogLog { runHyperLogLog :: V.Vector Rank }
    deriving (Eq, Show, Generic)

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
type role HyperLogLog nominal
#endif

instance Serialize (HyperLogLog p)

makeClassy ''HyperLogLog

_HyperLogLog :: Iso' (HyperLogLog p) (V.Vector Rank)
_HyperLogLog = iso runHyperLogLog HyperLogLog
{-# INLINE _HyperLogLog #-}

instance ReifiesConfig p => HasConfig (HyperLogLog p) where
  config = to reflectConfig
  {-# INLINE config #-}

instance Semigroup (HyperLogLog p) where
  HyperLogLog a <> HyperLogLog b = HyperLogLog (V.zipWith max a b)
  {-# INLINE (<>) #-}

-- The 'Monoid' instance \"should\" just work. Give me two estimators and I
-- can give you an estimator for the union set of the two.
instance ReifiesConfig p => Monoid (HyperLogLog p) where
  mempty = HyperLogLog $ V.replicate (reflectConfig (Proxy :: Proxy p) ^. numBuckets) 0
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

insert :: (ReifiesConfig s, Hashable a) => a -> HyperLogLog s -> HyperLogLog s
insert a m@(HyperLogLog v) = HyperLogLog $ V.modify (\x -> do
    old <- MV.read x bk
    when (rnk > old) $ MV.write x bk rnk
  ) v where
  !h = w32 (hash a)
  !bk = calcBucket m h
  !rnk = calcRank m h
{-# INLINE insert #-}

-- | Approximate size of our set
size :: ReifiesConfig p => HyperLogLog p -> Approximate Int64
size m@(HyperLogLog bs) = Approximate 0.9972 l expected h where
  m' = fromIntegral (m^.numBuckets)
  numZeros = fromIntegral . V.length . V.filter (== 0) $ bs
  res = case raw < m^.smallRange of
    True | numZeros > 0 -> m' * log (m' / numZeros)
         | otherwise -> raw
    False | raw <= m^.interRange -> raw
          | otherwise -> -1 * lim32 * log (1 - raw / lim32)
  raw = m^.rawFact * (1 / sm)
  sm = V.sum $ V.map (\x -> 1 / (2 ^^ x)) bs
  expected = round res
  sd = err (m^.numBits)
  err n = 1.04 / sqrt (fromInteger (bit n))
  l = floor $ max (res*(1-3*sd)) 0
  h = ceiling $ res*(1+3*sd)
{-# INLINE size #-}

intersectionSize :: ReifiesConfig p => [HyperLogLog p] -> Approximate Int64
intersectionSize [] = 0
intersectionSize (x:xs) = withMin 0 $ size x + intersectionSize xs - intersectionSize (mappend x <$> xs)
{-# INLINE intersectionSize #-}

cast :: forall p q. (ReifiesConfig p, ReifiesConfig q) => HyperLogLog p -> Maybe (HyperLogLog q)
cast old
  | newBuckets <= oldBuckets = Just $ over _HyperLogLog ?? mempty $ V.modify $ \m ->
    V.forM_ (V.indexed $ old^._HyperLogLog) $ \ (i,o) -> do
      let j = mod i newBuckets
      a <- MV.read m j
      MV.write m j (max o a)
  | otherwise = Nothing -- TODO?
  where
  newConfig = reflectConfig (Proxy :: Proxy q)
  newBuckets = newConfig^.numBuckets
  oldBuckets = old^.numBuckets
{-# INLINE cast #-}
