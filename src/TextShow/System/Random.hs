{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.System.Random
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'StdGen'.

/Since: 2/
-}
module TextShow.System.Random (showbStdGenPrec) where

import System.Random (StdGen)
import TextShow (TextShow(..), Builder, FromStringShow(..))

#include "inline.h"

-- | Convert a 'StdGen' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbStdGenPrec :: Int -> StdGen -> Builder
showbStdGenPrec p = showbPrec p . FromStringShow
{-# INLINE showbStdGenPrec #-}

instance TextShow StdGen where
    showbPrec = showbStdGenPrec
    INLINE_INST_FUN(showbPrec)
