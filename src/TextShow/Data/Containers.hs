{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Containers
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the @containers@ library.

/Since: 2/
-}
module TextShow.Data.Containers (
      liftShowbIntMapPrec
    , showbIntSetPrec
    , liftShowbMapPrec2
    , liftShowbSequencePrec
    , liftShowbViewLPrec
    , liftShowbViewRPrec
    , liftShowbSetPrec
    , liftShowbTreePrec
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

import           TextShow (TextShow(..), TextShow1(..), TextShow2(..),
                           Builder, showbPrec1)
import           TextShow.Data.Integral ()
import           TextShow.TH (deriveTextShow, deriveTextShow1)
import           TextShow.Utils (showbUnaryListWith)

#include "inline.h"

-- | Convert an 'IntMap' to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbIntMapPrec :: (v -> Builder) -> Int -> IntMap v -> Builder
liftShowbIntMapPrec sp p =
    showbUnaryListWith (liftShowbList2 showbPrec undefined
                                      (const sp) undefined) p . IM.toList
{-# INLINE liftShowbIntMapPrec #-}

-- | Convert an 'IntSet' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbIntSetPrec :: Int -> IntSet -> Builder
showbIntSetPrec p = showbUnaryListWith showbList p . IS.toList
{-# INLINE showbIntSetPrec #-}

-- | Convert a 'Map' to a 'Builder' with the given show functions and precedence.
--
-- /Since: 3/
liftShowbMapPrec2 :: (k -> Builder) -> (v -> Builder) -> Int -> Map k v -> Builder
liftShowbMapPrec2 sp1 sp2 p =
    showbUnaryListWith (liftShowbList2 (const sp1) undefined
                                       (const sp2) undefined) p . M.toList
{-# INLINE liftShowbMapPrec2 #-}

-- | Convert a 'Sequence' to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbSequencePrec :: ([a] -> Builder) -> Int -> Seq a -> Builder
liftShowbSequencePrec sl p = showbUnaryListWith sl p . F.toList
{-# INLINE liftShowbSequencePrec #-}

-- | Convert a 'ViewL' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbViewLPrec :: (Int -> a -> Builder) -> ([a] -> Builder)
                   -> Int -> ViewL a -> Builder
liftShowbViewLPrec = liftShowbPrec
{-# INLINE liftShowbViewLPrec #-}

-- | Convert a 'ViewR' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbViewRPrec :: (Int -> a -> Builder) -> ([a] -> Builder)
                   -> Int -> ViewR a -> Builder
liftShowbViewRPrec = liftShowbPrec
{-# INLINE liftShowbViewRPrec #-}

-- | Convert a 'Set' to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbSetPrec :: ([a] -> Builder) -> Int -> Set a -> Builder
liftShowbSetPrec sl p = showbUnaryListWith sl p . Set.toList
{-# INLINE liftShowbSetPrec #-}

-- | Convert a 'Tree' to a 'Builder' with the given show functions and precedence.
--
-- /Since: 3/
liftShowbTreePrec :: (Int -> a -> Builder) -> ([a] -> Builder)
                  -> Int -> Tree a -> Builder
liftShowbTreePrec = liftShowbPrec
{-# INLINE liftShowbTreePrec #-}

instance TextShow v => TextShow (IntMap v) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow1 IntMap where
    liftShowbPrec sp _ = liftShowbIntMapPrec (sp 0)
    INLINE_INST_FUN(liftShowbPrec)

instance TextShow IntSet where
    showbPrec = showbIntSetPrec
    INLINE_INST_FUN(showbPrec)

instance (TextShow k, TextShow v) => TextShow (Map k v) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow k => TextShow1 (Map k) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList
    INLINE_INST_FUN(liftShowbPrec)

instance TextShow2 Map where
    liftShowbPrec2 sp1 _ sp2 _ = liftShowbMapPrec2 (sp1 0) (sp2 0)
    INLINE_INST_FUN(liftShowbPrec2)

instance TextShow a => TextShow (Seq a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow1 Seq where
    liftShowbPrec _ = liftShowbSequencePrec
    INLINE_INST_FUN(liftShowbPrec)

$(deriveTextShow  ''ViewL)
$(deriveTextShow1 ''ViewL)

$(deriveTextShow  ''ViewR)
$(deriveTextShow1 ''ViewR)

instance TextShow a => TextShow (Set a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow1 Set where
    liftShowbPrec _ = liftShowbSetPrec
    INLINE_INST_FUN(liftShowbPrec)

$(deriveTextShow  ''Tree)
$(deriveTextShow1 ''Tree)
