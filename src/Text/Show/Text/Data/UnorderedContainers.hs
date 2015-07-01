{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.UnorderedContainers
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for 'HashMap's and 'HashSet's.

/Since: 0.1/
-}
module Text.Show.Text.Data.UnorderedContainers (
      showbHashMapPrecWith2
    , showbHashSetPrec
    , showbHashSetPrecWith
    ) where

import qualified Data.HashMap.Lazy as HM (toList)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashSet as HS (toList)
import           Data.HashSet (HashSet)

import           Prelude hiding (Show)

import           Text.Show.Text (Show(showbPrec), Show1(..), Show2(..),
                                 Builder, showbPrec1)
import           Text.Show.Text.Data.Tuple (showb2TupleWith2)
import           Text.Show.Text.Utils (showbUnaryList, showbUnaryListWith)

#include "inline.h"

-- | Convert a 'HashMap' to a 'Builder' with the given show functions and precedence.
--
-- /Since: 1/
showbHashMapPrecWith2 :: (k -> Builder) -> (v -> Builder)
                      -> Int -> HashMap k v -> Builder
showbHashMapPrecWith2 sp1 sp2 p =
    showbUnaryListWith (showb2TupleWith2 sp1 sp2) p . HM.toList
{-# INLINE showbHashMapPrecWith2 #-}

-- | Convert a 'HashSet' to a 'Builder' with the given precedence.
--
-- /Since: 0.1/
showbHashSetPrec :: Show a => Int -> HashSet a -> Builder
showbHashSetPrec p = showbUnaryList p . HS.toList
{-# INLINE showbHashSetPrec #-}

-- | Convert a 'HashSet' to a 'Builder' with the given show function and precedence.
--
-- /Since: 1/
showbHashSetPrecWith :: (a -> Builder) -> Int -> HashSet a -> Builder
showbHashSetPrecWith sp p = showbUnaryListWith sp p . HS.toList
{-# INLINE showbHashSetPrecWith #-}

instance (Show k, Show v) => Show (HashMap k v) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance Show k => Show1 (HashMap k) where
    showbPrecWith = showbPrecWith2 showbPrec
    INLINE_INST_FUN(showbPrecWith)

instance Show2 HashMap where
    showbPrecWith2 sp1 sp2 = showbHashMapPrecWith2 (sp1 0) (sp2 0)
    INLINE_INST_FUN(showbPrecWith2)

instance Show a => Show (HashSet a) where
    showbPrec = showbHashSetPrec
    INLINE_INST_FUN(showbPrec)

instance Show1 HashSet where
    showbPrecWith sp = showbHashSetPrecWith $ sp 0
    INLINE_INST_FUN(showbPrecWith)
