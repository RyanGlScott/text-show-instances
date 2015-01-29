{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.Utils
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for 'NubList's and 'NubListR's.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.Utils (showbNubList, showbNubListR) where

import Distribution.Utils.NubList (NubList, fromNubList, NubListR, fromNubListR)
import Prelude hiding (Show)
import Text.Show.Text (Show(showb, showbPrec), Show1(showbPrec1), Builder)

#include "inline.h"

-- | Convert a 'NubList' to a Builder.
-- 
-- /Since: 0.2/
showbNubList :: Show a => NubList a -> Builder
showbNubList = showb . fromNubList
{-# INLINE showbNubList #-}

-- | Convert a 'NubListR' to a Builder.
-- 
-- /Since: 0.2/
showbNubListR :: Show a => NubListR a -> Builder
showbNubListR = showb . fromNubListR
{-# INLINE showbNubListR #-}

instance Show a => Show (NubList a) where
    showb = showbNubList
    INLINE_INST_FUN(showb)

instance Show1 NubList where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)

instance Show a => Show (NubListR a) where
    showb = showbNubListR
    INLINE_INST_FUN(showb)

instance Show1 NubListR where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)