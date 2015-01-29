{-|
Module:      Properties.Distribution.Simple.Compiler
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.Simple.Compiler@
module of the @Cabal@ library.
-}
module Properties.Distribution.Simple.Compiler (cabalDistributionSimpleCompilerTests) where

import Distribution.Simple.Compiler (Compiler, DebugInfoLevel,
                                     OptimisationLevel, PackageDB)

import Instances.Distribution.Simple.Compiler ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.Simple.Compiler ()

cabalDistributionSimpleCompilerTests :: [TestTree]
cabalDistributionSimpleCompilerTests =
    [ testGroup "Text.Show.Text.Distribution.Simple.Compiler"
        [ testProperty "Compiler instance"          (prop_matchesShow :: Int -> Compiler -> Bool)
        , testProperty "DebugInfoLevel instance"    (prop_matchesShow :: Int -> DebugInfoLevel -> Bool)
        , testProperty "OptimisationLevel instance" (prop_matchesShow :: Int -> OptimisationLevel -> Bool)
        , testProperty "PackageDB instance"         (prop_matchesShow :: Int -> PackageDB -> Bool)
        ]
    ]