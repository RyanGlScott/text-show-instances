{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.System.Locale
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'Show' function for old 'TimeLocale's.

/Since: 0.1/
-}
module Text.Show.Text.System.Locale (showbTimeLocalePrec) where

import System.Locale (TimeLocale)

import Prelude hiding (Show)

import Text.Show.Text (Show(showbPrec), Builder)
import Text.Show.Text.TH (deriveShow)

-- | Convert a 'TimeLocale' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbTimeLocalePrec :: Int -> TimeLocale -> Builder
showbTimeLocalePrec = showbPrec
{-# INLINE showbTimeLocalePrec #-}

$(deriveShow ''TimeLocale)