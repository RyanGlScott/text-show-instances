{-|
Module:      Properties.Distribution.Compiler
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.Compiler@
module of the @Cabal@ library.
-}
module Properties.Distribution.Compiler (cabalDistributionCompilerTests) where

import Distribution.Compiler (AbiTag, CompilerFlavor, CompilerId, CompilerInfo)

import Instances.Distribution.Compiler ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.Compiler ()

cabalDistributionCompilerTests :: [TestTree]
cabalDistributionCompilerTests =
    [ testGroup "Text.Show.Text.Distribution.Compiler"
        [ testProperty "AbiTag instance"         (prop_matchesShow :: Int -> AbiTag -> Bool)
        , testProperty "CompilerFlavor instance" (prop_matchesShow :: Int -> CompilerFlavor -> Bool)
        , testProperty "CompilerId instance"     (prop_matchesShow :: Int -> CompilerId -> Bool)
        , testProperty "CompilerInfo instance"   (prop_matchesShow :: Int -> CompilerInfo -> Bool)
        ]
    ]