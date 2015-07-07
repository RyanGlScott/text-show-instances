{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Containers
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the @containers@ library.

/Since: 2/
-}
module TextShow.Data.Containers (
      showbIntMapPrecWith
    , showbIntSetPrec
    , showbMapPrecWith2
    , showbSequencePrec
    , showbSequencePrecWith
    , showbViewLPrec
    , showbViewLPrecWith
    , showbViewRPrec
    , showbViewRPrecWith
    , showbSetPrec
    , showbSetPrecWith
    , showbTreePrec
    , showbTreePrecWith
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

import           TextShow (TextShow(showb, showbPrec), TextShow1(..), TextShow2(..),
                           Builder, showbPrec1)
import           TextShow.Data.Integral ()
import           TextShow.Data.Tuple (showb2TupleWith2)
import           TextShow.TH (deriveTextShow, deriveTextShow1)
import           TextShow.Utils (showbUnaryList, showbUnaryListWith)

#include "inline.h"

-- | Convert an 'IntMap' to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbIntMapPrecWith :: (v -> Builder) -> Int -> IntMap v -> Builder
showbIntMapPrecWith sp p = showbUnaryListWith (showb2TupleWith2 showb sp) p . IM.toList
{-# INLINE showbIntMapPrecWith #-}

-- | Convert an 'IntSet' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbIntSetPrec :: Int -> IntSet -> Builder
showbIntSetPrec p = showbUnaryListWith showb p . IS.toList
{-# INLINE showbIntSetPrec #-}

-- | Convert a 'Map' to a 'Builder' with the given show functions and precedence.
--
-- /Since: 2/
showbMapPrecWith2 :: (k -> Builder) -> (v -> Builder) -> Int -> Map k v -> Builder
showbMapPrecWith2 sp1 sp2 p = showbUnaryListWith (showb2TupleWith2 sp1 sp2) p . M.toList
{-# INLINE showbMapPrecWith2 #-}

-- | Convert a 'Sequence' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbSequencePrec :: TextShow a => Int -> Seq a -> Builder
showbSequencePrec p = showbUnaryList p . F.toList
{-# INLINE showbSequencePrec #-}

-- | Convert a 'Sequence' to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbSequencePrecWith :: (a -> Builder) -> Int -> Seq a -> Builder
showbSequencePrecWith sp p = showbUnaryListWith sp p . F.toList
{-# INLINE showbSequencePrecWith #-}

-- | Convert a 'ViewL' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbViewLPrec :: TextShow a => Int -> ViewL a -> Builder
showbViewLPrec = showbPrec
{-# INLINE showbViewLPrec #-}

-- | Convert a 'ViewL' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbViewLPrecWith :: (a -> Builder) -> Int -> ViewL a -> Builder
showbViewLPrecWith sp = showbPrecWith $ const sp
{-# INLINE showbViewLPrecWith #-}

-- | Convert a 'ViewR' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbViewRPrec :: TextShow a => Int -> ViewR a -> Builder
showbViewRPrec = showbPrec
{-# INLINE showbViewRPrec #-}

-- | Convert a 'ViewR' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbViewRPrecWith :: (a -> Builder) -> Int -> ViewR a -> Builder
showbViewRPrecWith sp = showbPrecWith $ const sp
{-# INLINE showbViewRPrecWith #-}

-- | Convert a 'Set' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbSetPrec :: TextShow a => Int -> Set a -> Builder
showbSetPrec p = showbUnaryList p . Set.toList
{-# INLINE showbSetPrec #-}

-- | Convert a 'Set' to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbSetPrecWith :: (a -> Builder) -> Int -> Set a -> Builder
showbSetPrecWith sp p = showbUnaryListWith sp p . Set.toList
{-# INLINE showbSetPrecWith #-}

-- | Convert a 'Tree' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbTreePrec :: TextShow a => Int -> Tree a -> Builder
showbTreePrec = showbPrec
{-# INLINE showbTreePrec #-}

-- | Convert a 'Tree' to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbTreePrecWith :: (a -> Builder) -> Int -> Tree a -> Builder
showbTreePrecWith sp = showbPrecWith $ const sp
{-# INLINE showbTreePrecWith #-}

instance TextShow v => TextShow (IntMap v) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow1 IntMap where
    showbPrecWith sp = showbIntMapPrecWith $ sp 0
    INLINE_INST_FUN(showbPrecWith)

instance TextShow IntSet where
    showbPrec = showbIntSetPrec
    INLINE_INST_FUN(showbPrec)

instance (TextShow k, TextShow v) => TextShow (Map k v) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow k => TextShow1 (Map k) where
    showbPrecWith = showbPrecWith2 showbPrec
    INLINE_INST_FUN(showbPrecWith)

instance TextShow2 Map where
    showbPrecWith2 sp1 sp2 = showbMapPrecWith2 (sp1 0) (sp2 0)
    INLINE_INST_FUN(showbPrecWith2)

instance TextShow a => TextShow (Seq a) where
    showbPrec = showbSequencePrec
    INLINE_INST_FUN(showbPrec)

instance TextShow1 Seq where
    showbPrecWith sp = showbSequencePrecWith $ sp 0
    INLINE_INST_FUN(showbPrecWith)

$(deriveTextShow  ''ViewL)
$(deriveTextShow1 ''ViewL)

$(deriveTextShow  ''ViewR)
$(deriveTextShow1 ''ViewR)

instance TextShow a => TextShow (Set a) where
    showbPrec = showbSetPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow1 Set where
    showbPrecWith sp = showbSetPrecWith $ sp 0
    INLINE_INST_FUN(showbPrecWith)

$(deriveTextShow  ''Tree)
$(deriveTextShow1 ''Tree)
