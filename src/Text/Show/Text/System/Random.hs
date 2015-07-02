{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.System.Random
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'Show' function for 'StdGen'.

/Since: 0.1/
-}
module Text.Show.Text.System.Random (showbStdGenPrec) where

import Prelude hiding (Show)
import System.Random (StdGen)
import Text.Show.Text (Show(showbPrec), Builder, FromStringShow(..))

#include "inline.h"

-- | Convert a 'StdGen' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbStdGenPrec :: Int -> StdGen -> Builder
showbStdGenPrec p = showbPrec p . FromStringShow
{-# INLINE showbStdGenPrec #-}

instance Show StdGen where
    showbPrec = showbStdGenPrec
    INLINE_INST_FUN(showbPrec)