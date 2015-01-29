{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.License
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'License's.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.License (showbLicensePrec) where

import Distribution.License (License)

import Text.Show.Text (Builder, showbPrec)
import Text.Show.Text.TH (deriveShow)

-- | Convert a 'License' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbLicensePrec :: Int -> License -> Builder
showbLicensePrec = showbPrec
{-# INLINE showbLicensePrec #-}

$(deriveShow ''License)