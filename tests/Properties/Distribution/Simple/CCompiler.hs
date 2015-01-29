{-|
Module:      Properties.Distribution.Simple.CCompiler
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.Simple.CCompiler@
module of the @Cabal@ library.
-}
module Properties.Distribution.Simple.CCompiler (cabalDistributionSimpleCCompilerTests) where

import Distribution.Simple.CCompiler (CDialect)

import Instances.Distribution.Simple.CCompiler ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.Simple.CCompiler ()

cabalDistributionSimpleCCompilerTests :: [TestTree]
cabalDistributionSimpleCCompilerTests =
    [ testGroup "Text.Show.Text.Distribution.Simple.CCompiler"
        [ testProperty "CDialect instance" (prop_matchesShow :: Int -> CDialect -> Bool)
        ]
    ]