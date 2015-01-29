{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.Simple.LocalBuildInfo
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.Simple.LocalBuildInfo@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.Simple.LocalBuildInfo (
      showbComponentPrec
    , showbComponentLocalBuildInfoPrec
    , showbComponentNamePrec
    , showbLibraryNamePrec
    , showbLocalBuildInfoPrec
    ) where

import Distribution.Simple.LocalBuildInfo
    (Component, ComponentLocalBuildInfo, ComponentName, LibraryName, LocalBuildInfo)

import Text.Show.Text (Builder, showbPrec)
import Text.Show.Text.Distribution.InstalledPackageInfo ()
import Text.Show.Text.Distribution.Package              ()
import Text.Show.Text.Distribution.PackageDescription   ()
import Text.Show.Text.Distribution.Simple.PackageIndex  ()
import Text.Show.Text.Distribution.Simple.Program.Db    ()
import Text.Show.Text.Distribution.Simple.Setup         ()
import Text.Show.Text.TH (deriveShow)

-- | Convert a 'Component' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbComponentPrec :: Int -> Component -> Builder
showbComponentPrec = showbPrec
{-# INLINE showbComponentPrec #-}

-- | Convert a 'ComponentLocalBuildInfo' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbComponentLocalBuildInfoPrec :: Int -> ComponentLocalBuildInfo -> Builder
showbComponentLocalBuildInfoPrec = showbPrec
{-# INLINE showbComponentLocalBuildInfoPrec #-}

-- | Convert a 'ComponentName' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbComponentNamePrec :: Int -> ComponentName -> Builder
showbComponentNamePrec = showbPrec
{-# INLINE showbComponentNamePrec #-}

-- | Convert a 'LibraryName' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbLibraryNamePrec :: Int -> LibraryName -> Builder
showbLibraryNamePrec = showbPrec
{-# INLINE showbLibraryNamePrec #-}

-- | Convert a 'LocalBuildInfo' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbLocalBuildInfoPrec :: Int -> LocalBuildInfo -> Builder
showbLocalBuildInfoPrec = showbPrec
{-# INLINE showbLocalBuildInfoPrec #-}

$(deriveShow ''Component)
$(deriveShow ''ComponentLocalBuildInfo)
$(deriveShow ''ComponentName)
$(deriveShow ''LibraryName)
$(deriveShow ''LocalBuildInfo)