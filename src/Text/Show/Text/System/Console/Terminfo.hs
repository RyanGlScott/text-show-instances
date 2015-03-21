{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.System.Console.Terminfo
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @terminfo@ library.

/Since: 0.2/
-}
module Text.Show.Text.System.Console.Terminfo (showbColorPrec, showbSetupTermError) where

import Prelude hiding (Show)

import System.Console.Terminfo.Base (SetupTermError)
import System.Console.Terminfo.Color (Color)

import Text.Show.Text (Show(showb, showbPrec), Builder, FromStringShow(..))
import Text.Show.Text.TH (deriveShow)

#include "inline.h"

-- | Convert a 'Color' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbColorPrec :: Int -> Color -> Builder
showbColorPrec = showbPrec
{-# INLINE showbColorPrec #-}

-- | Convert a 'SetupTermError' to a 'Builder'.
-- 
-- /Since: 0.2/
showbSetupTermError :: SetupTermError -> Builder
showbSetupTermError = showb . FromStringShow
{-# INLINE showbSetupTermError #-}

$(deriveShow ''Color)

instance Show SetupTermError where
    showb = showbSetupTermError
    INLINE_INST_FUN(showb)
