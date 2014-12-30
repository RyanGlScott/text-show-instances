{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.System.Locale
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'TimeLocale'.

/Since: 0.1/
-}
module Text.Show.Text.System.Locale (showbTimeLocalePrec) where

import System.Locale (TimeLocale)

import Prelude hiding (Show)

import Text.Show.Text (Show(showbPrec), Builder)
import Text.Show.Text.TH (deriveShowPragmas, defaultInlineShowbPrec)

-- | Convert a 'TimeLocale' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbTimeLocalePrec :: Int -> TimeLocale -> Builder
showbTimeLocalePrec = showbPrec
{-# INLINE showbTimeLocalePrec #-}

$(deriveShowPragmas defaultInlineShowbPrec ''TimeLocale)