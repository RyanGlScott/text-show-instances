{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.PackageDescription
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.PackageDescription@
modules of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.PackageDescription (
      -- * @Distribution.PackageDescription@
      showbBenchmarkPrec
    , showbBenchmarkInterfacePrec
    , showbBenchmarkTypePrec
    , showbBuildInfoPrec
    , showbBuildTypePrec
    , showbConditionPrec
    , showbCondTreePrec
    , showbConfVarPrec
    , showbExecutablePrec
    , showbFlagPrec
    , showbFlagNamePrec
    , showbGenericPackageDescriptionPrec
    , showbLibraryPrec
    , showbModuleReexportPrec
    , showbModuleRenamingPrec
    , showbPackageDescriptionPrec
    , showbRepoKindPrec
    , showbRepoTypePrec
    , showbSourceRepoPrec
    , showbTestSuitePrec
    , showbTestSuiteInterfacePrec
    , showbTestTypePrec
    -- * @Distribution.PackageDescription.Check@
    , showbPackageCheck
    ) where

import Distribution.PackageDescription
import Distribution.PackageDescription.Check (PackageCheck(..))

import Prelude hiding (Show)

import Text.Show.Text (Show(showb, showbPrec), Show1(showbPrec1),
                            Builder, fromString)
import Text.Show.Text.Data.Containers            ()
import Text.Show.Text.Distribution.Compiler      ()
import Text.Show.Text.Distribution.License       ()
import Text.Show.Text.Distribution.ModuleName    ()
import Text.Show.Text.Distribution.Package       ()
import Text.Show.Text.Distribution.System        ()
import Text.Show.Text.Distribution.Version       ()
import Text.Show.Text.Language.Haskell.Extension ()
import Text.Show.Text.TH (deriveShow)

-- | Convert a 'Benchmark' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbBenchmarkPrec :: Int -> Benchmark -> Builder
showbBenchmarkPrec = showbPrec
{-# INLINE showbBenchmarkPrec #-}

-- | Convert a 'BenchmarkInterface' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbBenchmarkInterfacePrec :: Int -> BenchmarkInterface -> Builder
showbBenchmarkInterfacePrec = showbPrec
{-# INLINE showbBenchmarkInterfacePrec #-}

-- | Convert a 'BenchmarkType' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbBenchmarkTypePrec :: Int -> BenchmarkType -> Builder
showbBenchmarkTypePrec = showbPrec
{-# INLINE showbBenchmarkTypePrec #-}

-- | Convert a 'BuildInfo' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbBuildInfoPrec :: Int -> BuildInfo -> Builder
showbBuildInfoPrec = showbPrec
{-# INLINE showbBuildInfoPrec #-}

-- | Convert a 'BuildType' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbBuildTypePrec :: Int -> BuildType -> Builder
showbBuildTypePrec = showbPrec
{-# INLINE showbBuildTypePrec #-}

-- | Convert a 'Condition' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbConditionPrec :: Show c => Int -> Condition c -> Builder
showbConditionPrec = showbPrec
{-# INLINE showbConditionPrec #-}

-- | Convert a 'CondTree' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbCondTreePrec :: (Show v, Show c, Show a) => Int -> CondTree v c a -> Builder
showbCondTreePrec = showbPrec
{-# INLINE showbCondTreePrec #-}

-- | Convert a 'ConfVar' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbConfVarPrec :: Int -> ConfVar -> Builder
showbConfVarPrec = showbPrec
{-# INLINE showbConfVarPrec #-}

-- | Convert a 'Executable' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbExecutablePrec :: Int -> Executable -> Builder
showbExecutablePrec = showbPrec
{-# INLINE showbExecutablePrec #-}

-- | Convert a 'Flag' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbFlagPrec :: Int -> Flag -> Builder
showbFlagPrec = showbPrec
{-# INLINE showbFlagPrec #-}

-- | Convert a 'FlagName' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbFlagNamePrec :: Int -> FlagName -> Builder
showbFlagNamePrec = showbPrec
{-# INLINE showbFlagNamePrec #-}

-- | Convert a 'GenericPackageDescription' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbGenericPackageDescriptionPrec :: Int -> GenericPackageDescription -> Builder
showbGenericPackageDescriptionPrec = showbPrec
{-# INLINE showbGenericPackageDescriptionPrec #-}

-- | Convert a 'Library' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbLibraryPrec :: Int -> Library -> Builder
showbLibraryPrec = showbPrec
{-# INLINE showbLibraryPrec #-}

-- | Convert a 'ModuleReexport' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbModuleReexportPrec :: Int -> ModuleReexport -> Builder
showbModuleReexportPrec = showbPrec
{-# INLINE showbModuleReexportPrec #-}

-- | Convert a 'ModuleRenaming' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbModuleRenamingPrec :: Int -> ModuleRenaming -> Builder
showbModuleRenamingPrec = showbPrec
{-# INLINE showbModuleRenamingPrec #-}

-- | Convert a 'PackageDescription' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbPackageDescriptionPrec :: Int -> PackageDescription -> Builder
showbPackageDescriptionPrec = showbPrec
{-# INLINE showbPackageDescriptionPrec #-}

-- | Convert a 'RepoKind' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbRepoKindPrec :: Int -> RepoKind -> Builder
showbRepoKindPrec = showbPrec
{-# INLINE showbRepoKindPrec #-}

-- | Convert a 'RepoType' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbRepoTypePrec :: Int -> RepoType -> Builder
showbRepoTypePrec = showbPrec
{-# INLINE showbRepoTypePrec #-}

-- | Convert a 'SoureRepo' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbSourceRepoPrec :: Int -> SourceRepo -> Builder
showbSourceRepoPrec = showbPrec
{-# INLINE showbSourceRepoPrec #-}

-- | Convert a 'TestSuite' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbTestSuitePrec :: Int -> TestSuite -> Builder
showbTestSuitePrec = showbPrec
{-# INLINE showbTestSuitePrec #-}

-- | Convert a 'TestSuiteInterface' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbTestSuiteInterfacePrec :: Int -> TestSuiteInterface -> Builder
showbTestSuiteInterfacePrec = showbPrec
{-# INLINE showbTestSuiteInterfacePrec #-}

-- | Convert a 'TestType' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbTestTypePrec :: Int -> TestType -> Builder
showbTestTypePrec = showbPrec
{-# INLINE showbTestTypePrec #-}

-- | Convert a 'PackageCheck' to a 'Builder'.
-- 
-- /Since: 0.2/
showbPackageCheck :: PackageCheck -> Builder
showbPackageCheck = fromString . explanation
{-# INLINE showbPackageCheck #-}

$(deriveShow  ''Benchmark)
$(deriveShow  ''BenchmarkInterface)
$(deriveShow  ''BenchmarkType)
$(deriveShow  ''BuildInfo)
$(deriveShow  ''BuildType)
$(deriveShow  ''Condition)
$(deriveShow  ''CondTree)
$(deriveShow  ''ConfVar)
$(deriveShow  ''Executable)
$(deriveShow  ''Flag)
$(deriveShow  ''FlagName)
$(deriveShow  ''GenericPackageDescription)
$(deriveShow  ''Library)
$(deriveShow  ''ModuleReexport)
$(deriveShow  ''ModuleRenaming)
$(deriveShow  ''PackageDescription)
$(deriveShow  ''RepoKind)
$(deriveShow  ''RepoType)
$(deriveShow  ''SourceRepo)
$(deriveShow  ''TestSuite)
$(deriveShow  ''TestSuiteInterface)
$(deriveShow  ''TestType)

instance Show PackageCheck where
    showb = showbPackageCheck

instance Show1 Condition where
    showbPrec1 = showbPrec

instance (Show v, Show c) => Show1 (CondTree v c) where
    showbPrec1 = showbPrec