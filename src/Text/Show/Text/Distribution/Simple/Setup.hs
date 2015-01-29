{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.Simple.Setup
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.Simple.Setup@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.Simple.Setup (
      showbBuildFlagsPrec
    , showbCleanFlagsPrec
    , showbConfigFlagsPrec
    , showbCopyFlagsPrec
    , showbFlagPrec
    , showbHaddockFlagsPrec
    , showbHscolourFlagsPrec
    , showbInstallFlagsPrec
    , showbRegisterFlagsPrec
    , showbReplFlagsPrec
    , showbSDistFlagsPrec
    , showbTestShowDetails
    ) where

import Distribution.Simple.Setup
    (BuildFlags, CleanFlags, ConfigFlags, CopyFlags, Flag, HaddockFlags, HscolourFlags,
     InstallFlags, RegisterFlags, ReplFlags, SDistFlags, TestShowDetails)

import Prelude hiding (Show)

import Text.Show.Text (Show(showb, showbPrec), Show1(showbPrec1), Builder)
import Text.Show.Text.Distribution.Compiler           ()
import Text.Show.Text.Distribution.Package            ()
import Text.Show.Text.Distribution.PackageDescription ()
import Text.Show.Text.Distribution.Simple.Compiler    ()
import Text.Show.Text.Distribution.Simple.InstallDirs ()
import Text.Show.Text.Distribution.Simple.Program.Db  ()
import Text.Show.Text.Distribution.Utils              ()
import Text.Show.Text.Distribution.Verbosity          ()
import Text.Show.Text.TH (deriveShow)

-- | Convert a 'BuildFlags' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbBuildFlagsPrec :: Int -> BuildFlags -> Builder
showbBuildFlagsPrec = showbPrec
{-# INLINE showbBuildFlagsPrec #-}

-- | Convert a 'CleanFlags' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbCleanFlagsPrec :: Int -> CleanFlags -> Builder
showbCleanFlagsPrec = showbPrec
{-# INLINE showbCleanFlagsPrec #-}

-- | Convert a 'ConfigFlags' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbConfigFlagsPrec :: Int -> ConfigFlags -> Builder
showbConfigFlagsPrec = showbPrec
{-# INLINE showbConfigFlagsPrec #-}

-- | Convert a 'CopyFlags' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbCopyFlagsPrec :: Int -> CopyFlags -> Builder
showbCopyFlagsPrec = showbPrec
{-# INLINE showbCopyFlagsPrec #-}

-- | Convert a 'Flag' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbFlagPrec :: Show a => Int -> Flag a -> Builder
showbFlagPrec = showbPrec
{-# INLINE showbFlagPrec #-}

-- | Convert a 'HaddockFlags' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbHaddockFlagsPrec :: Int -> HaddockFlags -> Builder
showbHaddockFlagsPrec = showbPrec
{-# INLINE showbHaddockFlagsPrec #-}

-- | Convert a 'HscolourFlags' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbHscolourFlagsPrec :: Int -> HscolourFlags -> Builder
showbHscolourFlagsPrec = showbPrec
{-# INLINE showbHscolourFlagsPrec #-}

-- | Convert a 'InstallFlags' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbInstallFlagsPrec :: Int -> InstallFlags -> Builder
showbInstallFlagsPrec = showbPrec
{-# INLINE showbInstallFlagsPrec #-}

-- | Convert a 'RegisterFlags' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbRegisterFlagsPrec :: Int -> RegisterFlags -> Builder
showbRegisterFlagsPrec = showbPrec
{-# INLINE showbRegisterFlagsPrec #-}

-- | Convert a 'ReplFlags' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbReplFlagsPrec :: Int -> ReplFlags -> Builder
showbReplFlagsPrec = showbPrec
{-# INLINE showbReplFlagsPrec #-}

-- | Convert a 'SDistFlags' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbSDistFlagsPrec :: Int -> SDistFlags -> Builder
showbSDistFlagsPrec = showbPrec
{-# INLINE showbSDistFlagsPrec #-}

-- | Convert a 'TestShowDetails' value to a 'Builder'.
-- 
-- /Since: 0.2/
showbTestShowDetails :: TestShowDetails -> Builder
showbTestShowDetails = showb
{-# INLINE showbTestShowDetails #-}

$(deriveShow ''BuildFlags)
$(deriveShow ''CleanFlags)
$(deriveShow ''ConfigFlags)
$(deriveShow ''CopyFlags)
$(deriveShow ''Flag)
$(deriveShow ''HaddockFlags)
$(deriveShow ''HscolourFlags)
$(deriveShow ''InstallFlags)
$(deriveShow ''RegisterFlags)
$(deriveShow ''ReplFlags)
$(deriveShow ''SDistFlags)
$(deriveShow ''TestShowDetails)

instance Show1 Flag where
    showbPrec1 = showbPrec