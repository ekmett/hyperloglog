{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE MonoLocalBinds            #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-float-in #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE RoleAnnotations #-}
#endif

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013-2015
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
  , insertHash
  , intersectionSize
  , cast
#if __GLASGOW_HASKELL__ >= 708
  , coerceConfig
#endif
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Control.DeepSeq (NFData (..))
import Control.Lens
import Control.Monad
import Crypto.MAC.SipHash
import Data.Approximate.Type
import Data.Bits.Extras
import qualified Data.Binary as Binary
import Data.Binary
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial
import Data.HyperLogLog.Config
import Data.Proxy
import Data.Reflection
import Data.Semigroup
import Data.Serialize as Serialize
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
#if __GLASGOW_HASKELL__ < 710
import Data.Word
#endif
import GHC.Generics hiding (D, to)
import GHC.Int
#if __GLASGOW_HASKELL__ >= 708
import Data.Type.Coercion (Coercion(..))
#endif

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XDataKinds
-- >>> :load Data.HyperLogLog
-- >>> import Control.Lens
-- >>> import Data.Reflection
-- >>> import Data.Monoid
-- >>> import qualified Data.Vector.Unboxed as V

------------------------------------------------------------------------------
-- HyperLogLog
------------------------------------------------------------------------------

-- |
--
-- Initialize a new counter:
--
-- >>> runHyperLogLog (mempty :: HyperLogLog 3) == V.fromList [0,0,0,0,0,0,0,0]
-- True
--
-- Please note how you specify a counter size with the @n@
-- invocation. Sizes of up to 16 are valid, with 7 being a
-- likely good minimum for decent accuracy.
--
-- Let's count a list of unique items and get the latest estimate:
--
-- >>> size (foldr insert mempty [1..10] :: HyperLogLog 4)
-- Approximate {_confidence = 0.9972, _lo = 2, _estimate = 9, _hi = 17}
--
-- Note how 'insert' can be used to add new observations to the
-- approximate counter.
newtype HyperLogLog p = HyperLogLog { runHyperLogLog :: V.Vector Rank }
    deriving (Eq, Show, Generic, NFData)

#if __GLASGOW_HASKELL__ >= 708
-- | If two types @p@ and @q@ reify the same configuration, then we can coerce
-- between @'HyperLogLog' p@ and @'HyperLogLog' q@. We do this by building
-- a hole in the @nominal@ role for the configuration parameter.
coerceConfig :: forall p q . (Reifies p Integer, Reifies q Integer) => Maybe (Coercion (HyperLogLog p) (HyperLogLog q))
coerceConfig | reflect (Proxy :: Proxy p) == reflect (Proxy :: Proxy q) = Just Coercion
             | otherwise = Nothing
#endif

#if __GLASGOW_HASKELL__ >= 707
type role HyperLogLog nominal
#endif

instance Serialize (HyperLogLog p)

instance Serial (HyperLogLog p) where
  serialize (HyperLogLog v) = serialize (V.toList v)
  deserialize = liftM (HyperLogLog . V.fromList) deserialize

instance Binary (HyperLogLog p) where
  put (HyperLogLog v) = Binary.put (V.toList v)
  get = fmap (HyperLogLog . V.fromList) Binary.get

class HasHyperLogLog a p | a -> p where
  hyperLogLog :: Lens' a (HyperLogLog p)

instance HasHyperLogLog (HyperLogLog p) p where
  hyperLogLog = id

-- TODO: prism to ensure the sizes are right
_HyperLogLog :: Iso' (HyperLogLog p) (V.Vector Rank)
_HyperLogLog = iso runHyperLogLog HyperLogLog
{-# INLINE _HyperLogLog #-}

instance Semigroup (HyperLogLog p) where
  HyperLogLog a <> HyperLogLog b = HyperLogLog (V.zipWith max a b)
  {-# INLINE (<>) #-}

-- The 'Monoid' instance \"should\" just work. Give me two estimators and I
-- can give you an estimator for the union set of the two.
instance Reifies p Integer => Monoid (HyperLogLog p) where
  mempty = HyperLogLog $ V.replicate (numBuckets (reflect (Proxy :: Proxy p))) 0
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

sipKey :: SipKey
sipKey = SipKey 4 7

siphash :: Serial a => a -> Word64
siphash a = h
  where !bs = runPutS (serialize a)
        (SipHash !h) = hash sipKey bs
{-# INLINE siphash #-}

insert :: (Reifies s Integer, Serial a) => a -> HyperLogLog s -> HyperLogLog s
insert = insertHash . w32 . siphash
{-# INLINE insert #-}

-- | Insert a value that has already been hashed by whatever user defined hash function you want.
insertHash :: Reifies s Integer => Word32 -> HyperLogLog s -> HyperLogLog s
insertHash h m@(HyperLogLog v) = HyperLogLog $ V.modify (\x -> do
    old <- MV.read x bk
    when (rnk > old) $ MV.write x bk rnk
  ) v where
  !n = reflect m
  !bk = calcBucket n h
  !rnk = calcRank n h
{-# INLINE insertHash #-}

-- | Approximate size of our set
size :: Reifies p Integer => HyperLogLog p -> Approximate Int64
size m@(HyperLogLog bs) = Approximate 0.9972 l expected h where
  n = reflect m
  m' = fromIntegral (numBuckets n)
  numZeros = fromIntegral . V.length . V.filter (== 0) $ bs
  res = case raw < smallRange n of
    True | numZeros > 0 -> m' * log (m' / numZeros) -- 13.47 bits max error
         -- numZeros > 0 -> m' / 1 / (log m' - log numZeros) -- 6.47 bits max error
         | otherwise -> raw
    False | raw <= interRange -> raw
          -- otherwise -> -1 * lim32 * log (1 - raw / lim32) -- 44 bits max error
          -- raw / lim32 < -1.7563532969399233e-6 -> - log (1 - (raw / lim32)) * lim32 -- 5.39 bits max error
          | otherwise -> raw + (raw / lim32) * raw

  raw = rawFact n * (1 / sm)
  sm = V.sum $ V.map (\x -> 1 / (2 ^^ x)) bs
  expected = round res
  sd = 1.04 / sqrt m'
  l = floor $ max (res*(1-3*sd)) 0
  h = ceiling $ res*(1+3*sd)
{-# INLINE size #-}
#ifdef HERBIE
{-# ANN size "NoHerbie" #-}
#endif

intersectionSize :: Reifies p Integer => [HyperLogLog p] -> Approximate Int64
intersectionSize [] = 0
intersectionSize (x:xs) = withMin 0 $ size x + intersectionSize xs - intersectionSize (mappend x <$> xs)
{-# INLINE intersectionSize #-}

cast :: forall p q. (Reifies p Integer, Reifies q Integer) => HyperLogLog p -> Maybe (HyperLogLog q)
cast old
  | newBuckets <= oldBuckets = Just $ over _HyperLogLog ?? mempty $ V.modify $ \m ->
    V.forM_ (V.indexed $ old^._HyperLogLog) $ \ (i,o) -> do
      let j = mod i newBuckets
      a <- MV.read m j
      MV.write m j (max o a)
  | otherwise = Nothing -- TODO?
  where
  newBuckets = numBuckets (reflect (Proxy :: Proxy q))
  oldBuckets = numBuckets (reflect old)
{-# INLINE cast #-}
