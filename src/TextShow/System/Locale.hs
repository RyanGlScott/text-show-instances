{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.System.Locale
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for old 'TimeLocale's.

/Since: 2/
-}
module TextShow.System.Locale (showbTimeLocalePrec) where

import System.Locale (TimeLocale)

import TextShow (TextShow(..), Builder)
import TextShow.TH (deriveTextShow)

-- | Convert a 'TimeLocale' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbTimeLocalePrec :: Int -> TimeLocale -> Builder
showbTimeLocalePrec = showbPrec
{-# INLINE showbTimeLocalePrec #-}

$(deriveTextShow ''TimeLocale)
