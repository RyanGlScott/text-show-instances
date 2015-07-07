{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Semigroup
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for @Semigroup@ data types.

/Since: 2/
-}
module TextShow.Data.Semigroup (
      showbMinPrecWith
    , showbMaxPrecWith
    , showbFirstPrecWith
    , showbLastPrecWith
    , showbWrappedMonoidPrecWith
    , showbOptionPrecWith
    , showbArgPrecWith2
    ) where

import Data.Semigroup (Min, Max, First, Last, WrappedMonoid, Option, Arg)

import TextShow (Builder, showbPrecWith, showbPrecWith2)
import TextShow.TH (deriveTextShow, deriveTextShow1, deriveTextShow2)

#include "inline.h"

-- | Convert a 'Min' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbMinPrecWith :: (Int -> a -> Builder) -> Int -> Min a -> Builder
showbMinPrecWith = showbPrecWith
{-# INLINE showbMinPrecWith #-}

-- | Convert a 'Max' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbMaxPrecWith :: (Int -> a -> Builder) -> Int -> Max a -> Builder
showbMaxPrecWith = showbPrecWith
{-# INLINE showbMaxPrecWith #-}

-- | Convert a 'First' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbFirstPrecWith :: (Int -> a -> Builder) -> Int -> First a -> Builder
showbFirstPrecWith = showbPrecWith
{-# INLINE showbFirstPrecWith #-}

-- | Convert a 'Last' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbLastPrecWith :: (Int -> a -> Builder) -> Int -> Last a -> Builder
showbLastPrecWith = showbPrecWith
{-# INLINE showbLastPrecWith #-}

-- | Convert a 'WrappedMonoid' to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 2/
showbWrappedMonoidPrecWith :: (Int -> m -> Builder) -> Int -> WrappedMonoid m -> Builder
showbWrappedMonoidPrecWith = showbPrecWith
{-# INLINE showbWrappedMonoidPrecWith #-}

-- | Convert an 'Option' value to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 2/
showbOptionPrecWith :: (Int -> a -> Builder) -> Int -> Option a -> Builder
showbOptionPrecWith = showbPrecWith
{-# INLINE showbOptionPrecWith #-}

-- | Convert an 'Arg' value to a 'Builder' with the given show functions and precedence.
--
-- /Since: 2/
showbArgPrecWith2 :: (Int -> a -> Builder) -> (Int -> b -> Builder)
                  -> Int -> Arg a b -> Builder
showbArgPrecWith2 = showbPrecWith2
{-# INLINE showbArgPrecWith2 #-}

$(deriveTextShow  ''Min)
$(deriveTextShow1 ''Min)

$(deriveTextShow  ''Max)
$(deriveTextShow1 ''Max)

$(deriveTextShow  ''First)
$(deriveTextShow1 ''First)

$(deriveTextShow  ''Last)
$(deriveTextShow1 ''Last)

$(deriveTextShow  ''WrappedMonoid)
$(deriveTextShow1 ''WrappedMonoid)

$(deriveTextShow  ''Option)
$(deriveTextShow1 ''Option)

$(deriveTextShow  ''Arg)
$(deriveTextShow1 ''Arg)
$(deriveTextShow2 ''Arg)
