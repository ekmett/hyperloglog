--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013-2015
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- See the original paper for details:
-- <http://algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf>
--
-- __This library should not be used for cryptographic or security
-- applications__, as this implementation of HyperLogLog does not
-- generate keys from a cryptographically secure source of randomness.
--------------------------------------------------------------------
module Data.HyperLogLog
  (
  -- * HyperLogLog
    HyperLogLog
  , HasHyperLogLog(..)
  , size
  , intersectionSize
  , insert
  , insertHash
  , cast
  , coerceConfig
  ) where

import Data.HyperLogLog.Type
