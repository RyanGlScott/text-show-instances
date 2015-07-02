{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Semigroup
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'Show' functions for @Semigroup@ data types.

/Since: 0.1/
-}
module Text.Show.Text.Data.Semigroup (
      showbMinPrecWith
    , showbMaxPrecWith
    , showbFirstPrecWith
    , showbLastPrecWith
    , showbWrappedMonoidPrecWith
    , showbOptionPrecWith
    , showbArgPrecWith2
    ) where

import Data.Semigroup (Min, Max, First, Last, WrappedMonoid, Option, Arg)

import Prelude hiding (Show)

import Text.Show.Text (Builder, showbPrecWith, showbPrecWith2)
import Text.Show.Text.TH (deriveShow, deriveShow1, deriveShow2)

#include "inline.h"

-- | Convert a 'Min' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 1/
showbMinPrecWith :: (Int -> a -> Builder) -> Int -> Min a -> Builder
showbMinPrecWith = showbPrecWith
{-# INLINE showbMinPrecWith #-}

-- | Convert a 'Max' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 1/
showbMaxPrecWith :: (Int -> a -> Builder) -> Int -> Max a -> Builder
showbMaxPrecWith = showbPrecWith
{-# INLINE showbMaxPrecWith #-}

-- | Convert a 'First' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 1/
showbFirstPrecWith :: (Int -> a -> Builder) -> Int -> First a -> Builder
showbFirstPrecWith = showbPrecWith
{-# INLINE showbFirstPrecWith #-}

-- | Convert a 'Last' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 1/
showbLastPrecWith :: (Int -> a -> Builder) -> Int -> Last a -> Builder
showbLastPrecWith = showbPrecWith
{-# INLINE showbLastPrecWith #-}

-- | Convert a 'WrappedMonoid' to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 1/
showbWrappedMonoidPrecWith :: (Int -> m -> Builder) -> Int -> WrappedMonoid m -> Builder
showbWrappedMonoidPrecWith = showbPrecWith
{-# INLINE showbWrappedMonoidPrecWith #-}

-- | Convert an 'Option' value to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 1/
showbOptionPrecWith :: (Int -> a -> Builder) -> Int -> Option a -> Builder
showbOptionPrecWith = showbPrecWith
{-# INLINE showbOptionPrecWith #-}

-- | Convert an 'Arg' value to a 'Builder' with the given show functions and precedence.
--
-- /Since: 1/
showbArgPrecWith2 :: (Int -> a -> Builder) -> (Int -> b -> Builder)
                  -> Int -> Arg a b -> Builder
showbArgPrecWith2 = showbPrecWith2
{-# INLINE showbArgPrecWith2 #-}

$(deriveShow  ''Min)
$(deriveShow1 ''Min)

$(deriveShow  ''Max)
$(deriveShow1 ''Max)

$(deriveShow  ''First)
$(deriveShow1 ''First)

$(deriveShow  ''Last)
$(deriveShow1 ''Last)

$(deriveShow  ''WrappedMonoid)
$(deriveShow1 ''WrappedMonoid)

$(deriveShow  ''Option)
$(deriveShow1 ''Option)

$(deriveShow  ''Arg)
$(deriveShow1 ''Arg)
$(deriveShow2 ''Arg)
