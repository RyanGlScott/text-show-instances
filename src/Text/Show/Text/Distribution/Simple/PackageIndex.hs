{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.Simple.PackageIndex
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.Simple.PackageIndex@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.Simple.PackageIndex (showbPackageIndexPrec) where

import Distribution.Simple.PackageIndex (PackageIndex)

import Prelude hiding (Show)

import Text.Show.Text (Show(showbPrec), Show1(showbPrec1), Builder)
import Text.Show.Text.Data.Containers ()
import Text.Show.Text.Distribution.InstalledPackageInfo ()
import Text.Show.Text.Distribution.Simple.Compiler      ()
import Text.Show.Text.TH (deriveShow)

-- | Convert a 'PackageIndex' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbPackageIndexPrec :: Show a => Int -> PackageIndex a -> Builder
showbPackageIndexPrec = showbPrec
{-# INLINE showbPackageIndexPrec #-}

$(deriveShow ''PackageIndex)

instance Show1 PackageIndex where
    showbPrec1 = showbPrec