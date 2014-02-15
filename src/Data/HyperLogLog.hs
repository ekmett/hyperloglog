--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- See the original paper for details:
-- <http://algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf>
--------------------------------------------------------------------
module Data.HyperLogLog
  (
  -- * HyperLogLog
    HyperLogLog
  , HasHyperLogLog(..)
  , size
  , intersectionSize
  , insert
  , cast
  -- * Config
  , Config
  , hll
  -- * ReifiesConfig
  , ReifiesConfig
  , reifyConfig
  ) where

import Data.HyperLogLog.Config
import Data.HyperLogLog.Type
