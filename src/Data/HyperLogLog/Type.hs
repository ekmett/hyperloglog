{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RoleAnnotations           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE MonoLocalBinds            #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-float-in #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013-2025
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
    DefaultSipKey
  , DefaultHyperLogLog
  , SipKey
  , reifySipKey
  , HyperLogLog(..)
  , generateHyperLogLog
  , HasHyperLogLog(..)
  , size
  , insert
  , insertHash
  , intersectionSize
  , cast
  , coerceConfig
  ) where

import Control.DeepSeq (NFData (..))
import Control.Lens
import Control.Monad
import Crypto.MAC.SipHash
import Data.Approximate.Type
import Data.Bits.Extras
import qualified Data.Binary as Binary
import Data.Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Bytes.Get as Bytes (getWord64host, runGetL)
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial
import Data.HyperLogLog.Config
import Data.Proxy
import Data.Reflection
import Data.Serialize as Serialize
import Data.Type.Coercion (Coercion(..))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import GHC.Generics hiding (D, to)
import GHC.Int
import GHC.Types (Type)
import System.Entropy (getEntropy)

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif

-- $setup
-- >>> :set -XScopedTypeVariables
-- >>> :set -XTemplateHaskell
-- >>> :set -XDataKinds
-- >>> import Data.HyperLogLog
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
-- >>> runHyperLogLog (mempty :: DefaultHyperLogLog 3) == V.fromList [0,0,0,0,0,0,0,0]
-- True
--
-- Please note how you specify a counter size with the @n@
-- invocation. Sizes of up to 16 are valid, with 7 being a
-- likely good minimum for decent accuracy.
--
-- Let's count a list of unique items and get the latest estimate:
--
-- >>> size (foldr insert mempty [1..10] :: DefaultHyperLogLog 4)
-- Approximate {_confidence = 0.9972, _lo = 2, _estimate = 9, _hi = 17}
--
-- Note how 'insert' can be used to add new observations to the
-- approximate counter.
--
-- The @s@ type parameter configures the 'SipKey' that is passed to the hash
-- function when 'insert'ing a new value. Note that if cryptographic security is
-- a primary consideration, it is recommended that you create 'HyperLogLog'
-- values using 'generateHyperLogLog' so that the 'SipKey' is randomly
-- generated using system entropy. In contrast, the 'HyperLogLog' data
-- constructor and the 'mempty' method allow constructing 'HyperLogLog' values
-- with fixed 'SipKey's, which can result in exponentially inaccurate estimates
-- if exploited by an adversary. (See <https://eprint.iacr.org/2021/1139>.)
newtype HyperLogLog s p =
  -- | Construct a 'HyperLogLog' value directly from a 'V.Vector'.
  --
  -- Note that using this data constructor directly permits the @s@ type
  -- parameter to be a fixed 'SipKey', which can have cryptographic security
  -- implications. See the Haddocks for 'HyperLogLog' for more details.
  HyperLogLog { runHyperLogLog :: V.Vector Rank }
    deriving (Eq, Show, Generic, NFData)
type role HyperLogLog nominal nominal

data DefaultSipKey = DefaultSipKey

instance Reifies DefaultSipKey SipKey where
  reflect _ = SipKey 4 7

type DefaultHyperLogLog = HyperLogLog DefaultSipKey

-- | Promote a 'SipKey' to the type level for use as part of a 'HyperLogLog'
-- type.
reifySipKey :: Word64 -> Word64 -> (forall (s :: Type). Reifies s SipKey => Proxy s -> r) -> r
reifySipKey m n k = reify (SipKey m n) k

-- | Generate a fresh 'HyperLogLog' value using a randomly generated 'SipKey':
--
-- >>> generateHyperLogLog $ \(m :: HyperLogLog s 3) -> pure (runHyperLogLog m == V.fromList [0,0,0,0,0,0,0,0])
-- True
--
--
-- The 'SipKey' is generated using system entropy, so if cryptographic security
-- is a primary consideration, use this function to create a 'HyperLogLog'
-- value instead of manually building one (e.g., by using the 'HyperLogLog'
-- data constructor or by using 'mempty').
generateHyperLogLog :: Reifies p Integer => (forall (s :: Type). HyperLogLog s p -> IO r) -> IO r
generateHyperLogLog k = do
  m <- generateWord64
  n <- generateWord64
  reifySipKey m n $ \(_ :: Proxy s) ->
    k @s mempty
  where
    generateWord64 :: IO Word64
    generateWord64 = do
      bs <- BSL.fromStrict <$> getEntropy 8
      pure $ Bytes.runGetL Bytes.getWord64host bs

-- | If the two types @p@ and @q@ reify the same configuration, and if the two
-- types @r@ and @s@ reify the same 'SipKey', then we can coerce between
-- @'HyperLogLog' r p@ and @'HyperLogLog' s q@. We do this by building a hole in
-- the @nominal@ role for the configuration parameter.
coerceConfig :: forall p q r s. (Reifies p Integer, Reifies q Integer, Reifies r SipKey, Reifies s SipKey) => Maybe (Coercion (HyperLogLog r p) (HyperLogLog s q))
coerceConfig | reflect (Proxy :: Proxy p) == reflect (Proxy :: Proxy q)
             , reflect (Proxy :: Proxy r) == reflect (Proxy :: Proxy s) = Just Coercion
             | otherwise = Nothing

instance Serialize (HyperLogLog s p)

instance Serial (HyperLogLog s p) where
  serialize (HyperLogLog v) = serialize (V.toList v)
  deserialize = liftM (HyperLogLog . V.fromList) deserialize

instance Binary (HyperLogLog s p) where
  put (HyperLogLog v) = Binary.put (V.toList v)
  get = fmap (HyperLogLog . V.fromList) Binary.get

class HasHyperLogLog a s p | a -> s p where
  hyperLogLog :: Lens' a (HyperLogLog s p)

instance HasHyperLogLog (HyperLogLog s p) s p where
  hyperLogLog = id

-- TODO: prism to ensure the sizes are right
_HyperLogLog :: Iso' (HyperLogLog s p) (V.Vector Rank)
_HyperLogLog = iso runHyperLogLog HyperLogLog
{-# INLINE _HyperLogLog #-}

instance Semigroup (HyperLogLog s p) where
  HyperLogLog a <> HyperLogLog b = HyperLogLog (V.zipWith max a b)
  {-# INLINE (<>) #-}

-- | The 'Monoid' instance \"should\" just work. Give me two estimators and I
-- can give you an estimator for the union set of the two.
--
-- Note that using 'mempty' permits the @s@ type parameter to be a fixed
-- 'SipKey', which can have cryptographic security implications. See the
-- Haddocks for 'HyperLogLog' for more details.
instance Reifies p Integer => Monoid (HyperLogLog s p) where
  mempty = HyperLogLog $ V.replicate (numBuckets (reflect (Proxy :: Proxy p))) 0
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

siphash :: Serial a => SipKey -> a -> Word64
siphash k a = h
  where !bs = runPutS (serialize a)
        (SipHash !h) = hash k bs
{-# INLINE siphash #-}

insert :: forall s p a. (Reifies s SipKey, Reifies p Integer, Serial a) => a -> HyperLogLog s p -> HyperLogLog s p
insert = insertHash . w32 . siphash (reflect (Proxy :: Proxy s))
{-# INLINE insert #-}

-- | Insert a value that has already been hashed by whatever user defined hash function you want.
insertHash :: Reifies p Integer => Word32 -> HyperLogLog s p -> HyperLogLog s p
insertHash h m@(HyperLogLog v) = HyperLogLog $ V.modify (\x -> do
    old <- MV.read x bk
    when (rnk > old) $ MV.write x bk rnk
  ) v where
  !n = reflect m
  !bk = calcBucket n h
  !rnk = calcRank n h
{-# INLINE insertHash #-}

-- | Approximate size of our set
size :: Reifies p Integer => HyperLogLog s p -> Approximate Int64
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

intersectionSize :: Reifies p Integer => [HyperLogLog s p] -> Approximate Int64
intersectionSize [] = 0
intersectionSize (x:xs) = withMin 0 $ size x + intersectionSize xs - intersectionSize (mappend x <$> xs)
{-# INLINE intersectionSize #-}

cast :: forall p q s. (Reifies p Integer, Reifies q Integer) => HyperLogLog s p -> Maybe (HyperLogLog s q)
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
