{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.InstalledPackageInfo
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.InstalledPackageInfo@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.InstalledPackageInfo (
      showbExposedModulePrec
    , showbInstalledPackageInfo_Prec
    , showbOriginalModulePrec
    ) where

import Distribution.InstalledPackageInfo
    (ExposedModule, InstalledPackageInfo_, OriginalModule)

import Prelude hiding (Show)

import Text.Show.Text (Show(showbPrec), Show1(showbPrec1), Builder)
import Text.Show.Text.Distribution.License ()
import Text.Show.Text.Distribution.ModuleName ()
import Text.Show.Text.Distribution.Package ()
import Text.Show.Text.TH (deriveShow, deriveShowPragmas, defaultInlineShowbPrec)

-- | Convert an 'ExposedModule' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbExposedModulePrec :: Int -> ExposedModule -> Builder
showbExposedModulePrec = showbPrec
{-# INLINE showbExposedModulePrec #-}

-- | Convert an 'InstalledPackageInfo_' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbInstalledPackageInfo_Prec :: Show m => Int -> InstalledPackageInfo_ m -> Builder
showbInstalledPackageInfo_Prec = showbPrec
{-# INLINE showbInstalledPackageInfo_Prec #-}

-- | Convert an 'OriginalModule' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbOriginalModulePrec :: Int -> OriginalModule -> Builder
showbOriginalModulePrec = showbPrec
{-# INLINE showbOriginalModulePrec #-}

$(deriveShow                               ''ExposedModule)
$(deriveShow                               ''InstalledPackageInfo_)
$(deriveShowPragmas defaultInlineShowbPrec ''OriginalModule)

instance Show1 InstalledPackageInfo_ where
    showbPrec1 = showbPrec