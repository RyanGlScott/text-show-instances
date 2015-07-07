{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Tagged
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Tagged' values.

/Since: 2/
-}
module TextShow.Data.Tagged (showbTaggedPrecWith) where

import Data.Tagged (Tagged(..))
import TextShow (TextShow(showbPrec), TextShow1(..), TextShow2(..),
                 Builder, showbPrec1, showbUnaryWith)

#include "inline.h"

-- | Convert a 'Tagged' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbTaggedPrecWith :: (Int -> b -> Builder) -> Int -> Tagged s b -> Builder
showbTaggedPrecWith sp p (Tagged b) = showbUnaryWith sp "Tagged" p b
{-# INLINE showbTaggedPrecWith #-}

instance TextShow b => TextShow (Tagged s b) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance TextShow1 (Tagged s) where
    showbPrecWith = showbTaggedPrecWith
    INLINE_INST_FUN(showbPrecWith)

instance TextShow2 Tagged where
    showbPrecWith2 _ = showbTaggedPrecWith
    INLINE_INST_FUN(showbPrecWith2)
