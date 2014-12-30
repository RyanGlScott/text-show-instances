{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Tagged
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Tagged' values.

/Since: 0.1/
-}
module Text.Show.Text.Data.Tagged (showbTaggedPrec) where

import Data.Tagged (Tagged(..))
import Prelude hiding (Show)
import Text.Show.Text (Show(showbPrec), Show1(showbPrec1),
                       Builder, showbUnary)

#include "inline.h"

-- | Convert a 'Tagged' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbTaggedPrec :: Show b => Int -> Tagged s b -> Builder
showbTaggedPrec p (Tagged b) = showbUnary "Tagged" p b
{-# INLINE showbTaggedPrec #-}

instance Show b => Show (Tagged s b) where
    showbPrec = showbTaggedPrec
    INLINE_INST_FUN(showbPrec)

instance Show1 (Tagged s) where
    showbPrec1 = showbTaggedPrec
    INLINE_INST_FUN(showbPrec1)