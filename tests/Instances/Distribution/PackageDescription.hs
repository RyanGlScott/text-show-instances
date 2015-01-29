{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.PackageDescription
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Distribution.PackageDescription@
module of the @Cabal@ library.
-}
module Instances.Distribution.PackageDescription () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif

import Data.Functor ((<$>))

import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.PackageDescription
import Distribution.PackageDescription.Check (PackageCheck(..))

import Instances.Distribution.Compiler      ()
import Instances.Distribution.License       ()
import Instances.Distribution.ModuleName    (fModuleName)
import Instances.Distribution.Package       ()
import Instances.Distribution.System        ()
import Instances.Distribution.Version       ()
import Instances.Language.Haskell.Extension ()
import Instances.Utils ((<@>))

import Test.QuickCheck.Instances ()
import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary Benchmark where
    arbitrary = Benchmark <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary BenchmarkInterface where
    arbitrary = oneof [ BenchmarkExeV10      <$> arbitrary <*> arbitrary
                      , BenchmarkUnsupported <$> arbitrary
                      ]

instance Arbitrary BenchmarkType where
    arbitrary = oneof [ BenchmarkTypeExe     <$> arbitrary
                      , BenchmarkTypeUnknown <$> arbitrary <*> arbitrary
                      ]

instance Arbitrary BuildInfo where
    arbitrary = do
        string <- arbitrary
        dep    <- arbitrary
        let opts :: [(CompilerFlavor, [String])]
            opts = [(GHC, [string])]
            
            cfbi :: [(String, String)]
            cfbi = [(string, string)]
        BuildInfo <$> arbitrary <@> [dep]     <@> [string]      <@> [string]
                  <@> [string]  <@> [dep]     <@> [string]      <@> [string]
                  <@> [string]  <@> [string]  <@> [fModuleName] <*> arbitrary
                  <*> arbitrary <*> arbitrary <*> arbitrary     <*> arbitrary
                  <@> [string]  <@> [string]  <@> [string]      <@> [string]
                  <@> [string]  <@> [string]  <@> opts          <@> opts
                  <@> opts      <@> cfbi      <@> [dep]         <*> arbitrary
--     arbitrary = BuildInfo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--                           <*> arbitrary <*> arbitrary <@> arbitrary <*> arbitrary
--                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary BuildType where
    arbitrary = oneof [ pure Simple
                      , pure Configure
                      , pure Make
                      , pure Custom
                      , UnknownBuildType <$> arbitrary
                      ]

instance Arbitrary c => Arbitrary (Condition c) where
    arbitrary = oneof [ Var <$> arbitrary
                      , Lit <$> arbitrary
                      , pure $ CNot fCondition
                      , pure $ COr  fCondition fCondition
                      , pure $ CAnd fCondition fCondition
                      ]
--     arbitrary = oneof [ Var  <$> arbitrary
--                       , Lit  <$> arbitrary
--                       , CNot <$> arbitrary
--                       , COr  <$> arbitrary <*> arbitrary
--                       , CAnd <$> arbitrary <*> arbitrary
--                       ]

instance (Arbitrary v, Arbitrary c, Arbitrary a) => Arbitrary (CondTree v c a) where
    arbitrary = CondNode <$> arbitrary <*> arbitrary <@> []
--     arbitrary = CondNode <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ConfVar where
    arbitrary = oneof [ OS   <$> arbitrary
                      , Arch <$> arbitrary
                      , Flag <$> arbitrary
                      , Impl <$> arbitrary <*> arbitrary
                      ]

instance Arbitrary Executable where
    arbitrary = Executable <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Flag where
    arbitrary = MkFlag <$> arbitrary <@> "" <*> arbitrary <*> arbitrary
--     arbitrary = MkFlag <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary FlagName where
    arbitrary = pure $ FlagName ""
-- deriving instance Arbitrary FlagName

instance Arbitrary GenericPackageDescription where
    arbitrary = GenericPackageDescription <$> arbitrary <*> arbitrary <@> Nothing
                                          <@> []        <@> []        <@> []
--     arbitrary = GenericPackageDescription <$> arbitrary <*> arbitrary <*> arbitrary
--                                           <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Library where
    arbitrary = Library [fModuleName] <$> arbitrary <@> [fModuleName]
                    <@> [fModuleName] <*> arbitrary <*> arbitrary
--     arbitrary = Library <$> arbitrary <*> arbitrary <*> arbitrary
--                         <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ModuleReexport where
    arbitrary = ModuleReexport <$> arbitrary <@> fModuleName <@> fModuleName
--     arbitrary = ModuleReexport <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ModuleRenaming where
    arbitrary = ModuleRenaming <$> arbitrary <@> [(fModuleName, fModuleName)]
--     arbitrary = ModuleRenaming <$> arbitrary <*> arbitrary

instance Arbitrary PackageDescription where
    arbitrary = do
        string <- arbitrary
        let cfpd :: [(String, String)]
            cfpd = [(string, string)]
        PackageDescription <$> arbitrary <*> arbitrary <@> [string]  <@> string
                           <@> string    <@> string    <@> string    <*> arbitrary
                           <@> string    <@> string    <@> string    <@> []
                           <@> string    <@> string    <@> string    <@> cfpd
                           <*> arbitrary <*> arbitrary <*> arbitrary <@> Nothing
                           <@> []        <@> []        <@> []        <@> [string]
                           <@> string    <@> [string]  <@> [string]  <@> [string]
--     arbitrary = PackageDescription <$> arbitrary <*> arbitrary <*> arbitrary
--                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--                      <*> arbitrary

instance Arbitrary RepoKind where
    arbitrary = oneof [pure RepoHead, pure RepoThis, RepoKindUnknown <$> arbitrary]

instance Arbitrary RepoType where
    arbitrary = oneof [ pure Darcs
                      , pure Git
                      , pure SVN
                      , pure CVS
                      , pure Mercurial
                      , pure GnuArch
                      , pure Bazaar
                      , pure Monotone
                      , OtherRepoType <$> arbitrary
                      ]

instance Arbitrary SourceRepo where
    arbitrary = SourceRepo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TestSuite where
    arbitrary = TestSuite <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TestSuiteInterface where
    arbitrary = oneof [ TestSuiteExeV10      <$> arbitrary <*> arbitrary
                      , TestSuiteLibV09      <$> arbitrary <@> fModuleName
                      , TestSuiteUnsupported <$> arbitrary
                      ]
--     arbitrary = oneof [ TestSuiteExeV10      <$> arbitrary <*> arbitrary
--                       , TestSuiteLibV09      <$> arbitrary <*> arbitrary
--                       , TestSuiteUnsupported <$> arbitrary
--                       ]

instance Arbitrary TestType where
    arbitrary = oneof [ TestTypeExe     <$> arbitrary
                      , TestTypeLib     <$> arbitrary
                      , TestTypeUnknown <$> arbitrary <*> arbitrary
                      ]

instance Arbitrary PackageCheck where
    arbitrary = oneof $ map (<$> arbitrary) [ PackageBuildImpossible
                                            , PackageBuildWarning
                                            , PackageDistSuspicious
                                            , PackageDistInexcusable
                                            ]

-------------------------------------------------------------------------------
-- Workarounds to make Arbitrary instances faster
-------------------------------------------------------------------------------

fCondition :: Condition c
fCondition = Lit True