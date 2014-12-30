{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.UnorderedContainers
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for 'HashMap's and 'HashSet's.

/Since: 0.1/
-}
module Text.Show.Text.Data.UnorderedContainers (showbHashMapPrec, showbHashSetPrec) where

import qualified Data.HashMap.Lazy as HM (toList)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashSet as HS (toList)
import           Data.HashSet (HashSet)

import           Prelude hiding (Show)

import           Text.Show.Text (Show(showbPrec), Show1(showbPrec1), Builder)
import           Text.Show.Text.Utils (showbUnaryList)

#include "inline.h"

-- | Convert a 'HashMap' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbHashMapPrec :: (Show k, Show v) => Int -> HashMap k v -> Builder
showbHashMapPrec p = showbUnaryList p . HM.toList
{-# INLINE showbHashMapPrec #-}

-- | Convert a 'HashSet' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbHashSetPrec :: Show a => Int -> HashSet a -> Builder
showbHashSetPrec p = showbUnaryList p . HS.toList
{-# INLINE showbHashSetPrec #-}

instance (Show k, Show v) => Show (HashMap k v) where
    showbPrec = showbHashMapPrec
    INLINE_INST_FUN(showbPrec)

instance Show a => Show (HashSet a) where
    showbPrec = showbHashSetPrec
    INLINE_INST_FUN(showbPrec)

instance Show k => Show1 (HashMap k) where
    showbPrec1 = showbHashMapPrec
    INLINE_INST_FUN(showbPrec1)

instance Show1 HashSet where
    showbPrec1 = showbHashSetPrec
    INLINE_INST_FUN(showbPrec1)