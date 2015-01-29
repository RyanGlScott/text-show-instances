{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.Simple.InstallDirs
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.Simple.InstallDirs@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.Simple.InstallDirs (
      showbCopyDestPrec
    , showbInstallDirsPrec
    , showbPathTemplate
    , showbPathTemplateVariable
    ) where

import Distribution.Simple.InstallDirs
    (CopyDest, InstallDirs, PathTemplate, PathTemplateVariable(..), fromPathTemplate)

import Prelude hiding (Show)

import Text.Show.Text (Show(showb, showbPrec), Show1(showbPrec1), Builder)
import Text.Show.Text.TH (deriveShow)

-- | Convert a 'CopyDest' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbCopyDestPrec :: Int -> CopyDest -> Builder
showbCopyDestPrec = showbPrec
{-# INLINE showbCopyDestPrec #-}

-- | Convert an 'InstallDirs' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbInstallDirsPrec :: Show a => Int -> InstallDirs a -> Builder
showbInstallDirsPrec = showbPrec
{-# INLINE showbInstallDirsPrec #-}

-- | Convert a 'PathTemplate' to a 'Builder'.
-- 
-- /Since: 0.2/
showbPathTemplate :: PathTemplate -> Builder
showbPathTemplate = showb . fromPathTemplate
{-# INLINE showbPathTemplate #-}

-- | Convert a 'PathTemplateVariable' to a 'Builder'.
-- 
-- /Since: 0.2/
showbPathTemplateVariable :: PathTemplateVariable -> Builder
showbPathTemplateVariable PrefixVar          = "prefix"
showbPathTemplateVariable PkgKeyVar          = "pkgkey"
showbPathTemplateVariable BindirVar          = "bindir"
showbPathTemplateVariable LibdirVar          = "libdir"
showbPathTemplateVariable LibsubdirVar       = "libsubdir"
showbPathTemplateVariable DatadirVar         = "datadir"
showbPathTemplateVariable DatasubdirVar      = "datasubdir"
showbPathTemplateVariable DocdirVar          = "docdir"
showbPathTemplateVariable HtmldirVar         = "htmldir"
showbPathTemplateVariable PkgNameVar         = "pkg"
showbPathTemplateVariable PkgVerVar          = "version"
showbPathTemplateVariable PkgIdVar           = "pkgid"
showbPathTemplateVariable CompilerVar        = "compiler"
showbPathTemplateVariable OSVar              = "os"
showbPathTemplateVariable ArchVar            = "arch"
showbPathTemplateVariable AbiTagVar          = "abitag"
showbPathTemplateVariable AbiVar             = "abi"
showbPathTemplateVariable ExecutableNameVar  = "executablename"
showbPathTemplateVariable TestSuiteNameVar   = "test-suite"
showbPathTemplateVariable TestSuiteResultVar = "result"
showbPathTemplateVariable BenchmarkNameVar   = "benchmark"

$(deriveShow ''CopyDest)
$(deriveShow ''InstallDirs)

instance Show1 InstallDirs where
    showbPrec1 = showbPrec

instance Show PathTemplate where
    showb = showbPathTemplate

instance Show PathTemplateVariable where
    showb = showbPathTemplateVariable