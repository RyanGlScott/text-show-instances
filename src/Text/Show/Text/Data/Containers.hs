{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Containers
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @containers@ library.

/Since: 0.1/
-}
module Text.Show.Text.Data.Containers (
      showbIntMapPrec
    , showbIntSetPrec
    , showbMapPrec
    , showbSequencePrec
    , showbViewLPrec
    , showbViewRPrec
    , showbSetPrec
    , showbTreePrec
    ) where

import qualified Data.Foldable as F

import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)
import qualified Data.IntSet as IS
import           Data.IntSet (IntSet)
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Sequence (Seq, ViewL, ViewR)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Tree (Tree)

import           Prelude hiding (Show)

import           Text.Show.Text (Builder, Show(showbPrec), Show1(showbPrec1))
import           Text.Show.Text.Data.Integral ()
import           Text.Show.Text.Data.List ()
import           Text.Show.Text.Data.Tuple ()
import           Text.Show.Text.TH (deriveShowPragmas, defaultInlineShowbPrec)
import           Text.Show.Text.Utils (showbUnaryList)

#include "inline.h"

-- | Convert an 'IntMap' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbIntMapPrec :: Show v => Int -> IntMap v -> Builder
showbIntMapPrec p = showbUnaryList p . IM.toList
{-# INLINE showbIntMapPrec #-}

-- | Convert an 'IntSet' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbIntSetPrec :: Int -> IntSet -> Builder
showbIntSetPrec p = showbUnaryList p . IS.toList
{-# INLINE showbIntSetPrec #-}

-- | Convert a 'Map' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbMapPrec :: (Show k, Show v) => Int -> Map k v -> Builder
showbMapPrec p = showbUnaryList p . M.toList
{-# INLINE showbMapPrec #-}

-- | Convert a 'Sequence' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbSequencePrec :: Show a => Int -> Seq a -> Builder
showbSequencePrec p = showbUnaryList p . F.toList
{-# INLINE showbSequencePrec #-}

-- | Convert a 'ViewL' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbViewLPrec :: Show a => Int -> ViewL a -> Builder
showbViewLPrec = showbPrec
{-# INLINE showbViewLPrec #-}

-- | Convert a 'ViewR' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbViewRPrec :: Show a => Int -> ViewR a -> Builder
showbViewRPrec = showbPrec
{-# INLINE showbViewRPrec #-}

-- | Convert a 'Set' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbSetPrec :: Show a => Int -> Set a -> Builder
showbSetPrec p = showbUnaryList p . Set.toList
{-# INLINE showbSetPrec #-}

-- | Convert a 'Tree' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbTreePrec :: Show a => Int -> Tree a -> Builder
showbTreePrec = showbPrec
{-# INLINE showbTreePrec #-}

instance Show v => Show (IntMap v) where
    showbPrec = showbIntMapPrec
    INLINE_INST_FUN(showbPrec)

instance Show IntSet where
    showbPrec = showbIntSetPrec
    INLINE_INST_FUN(showbPrec)

instance (Show k, Show v) => Show (Map k v) where
    showbPrec = showbMapPrec
    INLINE_INST_FUN(showbPrec)

instance Show a => Show (Seq a) where
    showbPrec = showbSequencePrec
    INLINE_INST_FUN(showbPrec)

instance Show a => Show (Set a) where
    showbPrec = showbSetPrec
    INLINE_INST_FUN(showbPrec)

$(deriveShowPragmas defaultInlineShowbPrec ''ViewL)
$(deriveShowPragmas defaultInlineShowbPrec ''ViewR)
$(deriveShowPragmas defaultInlineShowbPrec ''Tree)

instance Show1 IntMap where
    showbPrec1 = showbIntMapPrec
    INLINE_INST_FUN(showbPrec1)

instance Show k => Show1 (Map k) where
    showbPrec1 = showbMapPrec
    INLINE_INST_FUN(showbPrec1)

instance Show1 Seq where
    showbPrec1 = showbSequencePrec
    INLINE_INST_FUN(showbPrec1)

instance Show1 ViewL where
    showbPrec1 = showbViewLPrec
    INLINE_INST_FUN(showbPrec1)

instance Show1 ViewR where
    showbPrec1 = showbViewRPrec
    INLINE_INST_FUN(showbPrec1)

instance Show1 Set where
    showbPrec1 = showbSetPrec
    INLINE_INST_FUN(showbPrec1)

instance Show1 Tree where
    showbPrec1 = showbTreePrec
    INLINE_INST_FUN(showbPrec1)
