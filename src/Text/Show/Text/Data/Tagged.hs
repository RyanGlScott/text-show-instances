{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Tagged
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'Show' function for 'Tagged' values.

/Since: 0.1/
-}
module Text.Show.Text.Data.Tagged (showbTaggedPrecWith) where

import Data.Tagged (Tagged(..))
import Prelude hiding (Show)
import Text.Show.Text (Show(showbPrec), Show1(..), Show2(..), Builder,
                       showbPrec1, showbUnaryWith)

#include "inline.h"

-- | Convert a 'Tagged' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 1/
showbTaggedPrecWith :: (Int -> b -> Builder) -> Int -> Tagged s b -> Builder
showbTaggedPrecWith sp p (Tagged b) = showbUnaryWith sp "Tagged" p b
{-# INLINE showbTaggedPrecWith #-}

instance Show b => Show (Tagged s b) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance Show1 (Tagged s) where
    showbPrecWith = showbTaggedPrecWith
    INLINE_INST_FUN(showbPrecWith)

instance Show2 Tagged where
    showbPrecWith2 _ = showbTaggedPrecWith
    INLINE_INST_FUN(showbPrecWith2)
