{-# LANGUAGE CPP #-}
{-|
Module:      Properties
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ tests for @text-show-instances@.
-}
module Main (main) where

import Properties.Compiler.Hoopl                     (hooplTests)
import Properties.Control.Applicative.Trans          (applicativeFunctorTransformerTests)
import Properties.Control.Monad.Trans                (monadTransformerTests)
import Properties.Data.Binary                        (binaryTests)
import Properties.Data.Containers                    (containersTests)
import Properties.Data.Functor.Trans                 (functorTransformerTests)
import Properties.Data.List.NonEmpty                 (nonEmptyListTests)
import Properties.Data.Semigroup                     (semigroupTests)
import Properties.Data.String.UTF8                   (utf8StringTests)
import Properties.Data.Tagged                        (taggedTests)
import Properties.Data.Time                          (timeTests)
import Properties.Data.UnorderedContainers           (unorderedContainersTests)
import Properties.Data.Vector                        (vectorTests)
import Properties.Distribution.Compiler              (cabalDistributionCompilerTests)
import Properties.Distribution.InstalledPackageInfo  (cabalDistributionInstalledPackageInfoTests)
import Properties.Distribution.License               (cabalDistributionLicenseTests)
import Properties.Distribution.ModuleName            (cabalDistributionModuleNameTests)
import Properties.Distribution.Package               (cabalDistributionPackageTests)
import Properties.Distribution.PackageDescription    (cabalDistributionPackageDescriptionTests)
import Properties.Distribution.ParseUtils            (cabalDistributionParseUtilsTests)
import Properties.Distribution.Simple.BuildTarget    (cabalDistributionSimpleBuildTargetTests)
import Properties.Distribution.Simple.CCompiler      (cabalDistributionSimpleCCompilerTests)
import Properties.Distribution.Simple.Compiler       (cabalDistributionSimpleCompilerTests)
import Properties.Distribution.Simple.Configure      (cabalDistributionSimpleConfigureTests)
import Properties.Distribution.Simple.Hpc            (cabalDistributionSimpleHpcTests)
import Properties.Distribution.Simple.InstallDirs    (cabalDistributionSimpleInstallDirsTests)
import Properties.Distribution.Simple.LocalBuildInfo (cabalDistributionSimpleLocalBuildInfoTests)
import Properties.Distribution.Simple.PackageIndex   (cabalDistributionSimplePackageIndexTests)
import Properties.Distribution.Simple.Program.Db     (cabalDistributionSimpleProgramDbTests)
import Properties.Distribution.Simple.Program.GHC    (cabalDistributionSimpleProgramGHCTests)
import Properties.Distribution.Simple.Program.Types  (cabalDistributionSimpleProgramTypesTests)
import Properties.Distribution.Simple.Setup          (cabalDistributionSimpleSetupTests)
import Properties.Distribution.Simple.Test.Log       (cabalDistributionSimpleTestLogTests)
import Properties.Distribution.System                (cabalDistributionSystemTests)
import Properties.Distribution.TestSuite             (cabalDistributionTestSuiteTests)
import Properties.Distribution.Text                  (cabalDistributionTextTests)
import Properties.Distribution.Utils                 (cabalDistributionUtilsTests)
import Properties.Distribution.Verbosity             (cabalDistributionVerbosityTests)
import Properties.Distribution.Version               (cabalDistributionVersionTests)
import Properties.Language.Haskell.Extension         (cabalLanguageHaskellExtensionTests)
import Properties.Language.Haskell.TH                (templateHaskellTests)
import Properties.System.Console.Haskeline           (haskelineTests)
import Properties.System.Directory                   (directoryTests)
import Properties.System.Locale                      (oldLocaleTests)
#if !defined(mingw32_HOST_OS)
import Properties.System.Console.Terminfo            (terminfoTests)
import Properties.System.Posix                       (unixTests)
#endif
import Properties.System.Random                      (randomTests)
import Properties.System.Time                        (oldTimeTests)
#if defined(mingw32_HOST_OS)
import Properties.System.Win32                       (win32Tests)
#endif
import Properties.Text.PrettyPrint                   (prettyTests)
import Properties.Text.XHtml                         (xhtmlTests)
import Properties.Trace.Hpc                          (hpcTests)

import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree = testGroup "QuickCheck properties" allTests

allTests :: [TestTree]
allTests = concat [ hooplTests
                  , applicativeFunctorTransformerTests
                  , monadTransformerTests
                  , binaryTests
                  , containersTests
                  , functorTransformerTests
                  , nonEmptyListTests
                  , semigroupTests
                  , utf8StringTests
                  , taggedTests
                  , timeTests
                  , unorderedContainersTests
                  , vectorTests
                  , cabalDistributionCompilerTests
                  , cabalDistributionInstalledPackageInfoTests
                  , cabalDistributionLicenseTests
                  , cabalDistributionModuleNameTests
                  , cabalDistributionPackageTests
                  , cabalDistributionPackageDescriptionTests
                  , cabalDistributionParseUtilsTests
                  , cabalDistributionSimpleBuildTargetTests
                  , cabalDistributionSimpleCCompilerTests
                  , cabalDistributionSimpleCompilerTests
                  , cabalDistributionSimpleConfigureTests
                  , cabalDistributionSimpleHpcTests
                  , cabalDistributionSimpleInstallDirsTests
                  , cabalDistributionSimpleLocalBuildInfoTests
                  , cabalDistributionSimplePackageIndexTests
                  , cabalDistributionSimpleProgramDbTests
                  , cabalDistributionSimpleProgramGHCTests
                  , cabalDistributionSimpleProgramTypesTests
                  , cabalDistributionSimpleSetupTests
                  , cabalDistributionSimpleTestLogTests
                  , cabalDistributionSystemTests
                  , cabalDistributionTestSuiteTests
                  , cabalDistributionTextTests
                  , cabalDistributionUtilsTests
                  , cabalDistributionVerbosityTests
                  , cabalDistributionVersionTests
                  , cabalLanguageHaskellExtensionTests
                  , templateHaskellTests
                  , haskelineTests
                  , directoryTests
                  , oldLocaleTests
#if !defined(mingw32_HOST_OS)
                  , terminfoTests
                  , unixTests
#endif
                  , randomTests
                  , oldTimeTests
#if defined(mingw32_HOST_OS)
                  , win32Tests
#endif
                  , prettyTests
                  , xhtmlTests
                  , hpcTests
                  ]