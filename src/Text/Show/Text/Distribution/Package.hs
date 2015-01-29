{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.Package
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.Package@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.Package (
      showbDependencyPrec
    , showbInstalledPackageIdPrec
    , showbPackageIdentifierPrec
    , showbPackageKeyPrec
    , showbPackageNamePrec
    ) where

import Distribution.Package (Dependency, InstalledPackageId, PackageIdentifier,
                             PackageKey, PackageName)

import Text.Show.Text (Builder, showbPrec)
import Text.Show.Text.Distribution.Version ()
import Text.Show.Text.TH (deriveShow, deriveShowPragmas, defaultInlineShowbPrec)

-- | Convert a 'Dependency' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbDependencyPrec :: Int -> Dependency -> Builder
showbDependencyPrec = showbPrec
{-# INLINE showbDependencyPrec #-}

-- | Convert an 'InstalledPackageId' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbInstalledPackageIdPrec :: Int -> InstalledPackageId -> Builder
showbInstalledPackageIdPrec = showbPrec
{-# INLINE showbInstalledPackageIdPrec #-}

-- | Convert a 'PackageIdentifier' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbPackageIdentifierPrec :: Int -> PackageIdentifier -> Builder
showbPackageIdentifierPrec = showbPrec
{-# INLINE showbPackageIdentifierPrec #-}

-- | Convert a 'PackageKey' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbPackageKeyPrec :: Int -> PackageKey -> Builder
showbPackageKeyPrec = showbPrec
{-# INLINE showbPackageKeyPrec #-}

-- | Convert a 'PackageName' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbPackageNamePrec :: Int -> PackageName -> Builder
showbPackageNamePrec = showbPrec
{-# INLINE showbPackageNamePrec #-}

$(deriveShowPragmas defaultInlineShowbPrec ''Dependency)
$(deriveShowPragmas defaultInlineShowbPrec ''InstalledPackageId)
$(deriveShowPragmas defaultInlineShowbPrec ''PackageIdentifier)
$(deriveShow                               ''PackageKey)
$(deriveShowPragmas defaultInlineShowbPrec ''PackageName)