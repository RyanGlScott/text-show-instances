{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Semigroup
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for @Semigroup@ data types.

/Since: 0.1/
-}
module Text.Show.Text.Data.Semigroup (
      showbMinPrec
    , showbMaxPrec
    , showbFirstPrec
    , showbLastPrec
    , showbWrappedMonoidPrec
    , showbOptionPrec
    , showbArgPrec
    ) where

import Data.Semigroup (Min, Max, First, Last, WrappedMonoid, Option, Arg)

import Prelude hiding (Show)

import Text.Show.Text (Show(showbPrec), Show1(showbPrec1), Builder)
import Text.Show.Text.TH (deriveShowPragmas, defaultInlineShowbPrec)

#include "inline.h"

-- | Convert a 'Min' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbMinPrec :: Show a => Int -> Min a -> Builder
showbMinPrec = showbPrec
{-# INLINE showbMinPrec #-}

-- | Convert a 'Max' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbMaxPrec :: Show a => Int -> Max a -> Builder
showbMaxPrec = showbPrec
{-# INLINE showbMaxPrec #-}

-- | Convert a 'First' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbFirstPrec :: Show a => Int -> First a -> Builder
showbFirstPrec = showbPrec
{-# INLINE showbFirstPrec #-}

-- | Convert a 'Last' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbLastPrec :: Show a => Int -> Last a -> Builder
showbLastPrec = showbPrec
{-# INLINE showbLastPrec #-}

-- | Convert a 'WrappedMonoid' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbWrappedMonoidPrec :: Show m => Int -> WrappedMonoid m -> Builder
showbWrappedMonoidPrec = showbPrec
{-# INLINE showbWrappedMonoidPrec #-}

-- | Convert an 'Option' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbOptionPrec :: Show a => Int -> Option a -> Builder
showbOptionPrec = showbPrec
{-# INLINE showbOptionPrec #-}

-- | Convert an 'Arg' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbArgPrec :: (Show a, Show b) => Int -> Arg a b -> Builder
showbArgPrec = showbPrec
{-# INLINE showbArgPrec #-}

$(deriveShowPragmas defaultInlineShowbPrec ''Min)
$(deriveShowPragmas defaultInlineShowbPrec ''Max)
$(deriveShowPragmas defaultInlineShowbPrec ''First)
$(deriveShowPragmas defaultInlineShowbPrec ''Last)
$(deriveShowPragmas defaultInlineShowbPrec ''WrappedMonoid)
$(deriveShowPragmas defaultInlineShowbPrec ''Option)
$(deriveShowPragmas defaultInlineShowbPrec ''Arg)

instance Show1 Min where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)

instance Show1 Max where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)

instance Show1 First where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)

instance Show1 Last where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)

instance Show1 WrappedMonoid where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)

instance Show1 Option where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)

instance Show a => Show1 (Arg a) where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)