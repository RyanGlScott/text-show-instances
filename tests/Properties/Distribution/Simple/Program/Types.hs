{-|
Module:      Properties.Distribution.Simple.Program.Types
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.Simple.Program.Types@
module of the @Cabal@ library.
-}
module Properties.Distribution.Simple.Program.Types (cabalDistributionSimpleProgramTypesTests) where

import Distribution.Simple.Program.Types (ConfiguredProgram, ProgramLocation)

import Instances.Distribution.Simple.Program.Types ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.Simple.Program.Types ()

cabalDistributionSimpleProgramTypesTests :: [TestTree]
cabalDistributionSimpleProgramTypesTests =
    [ testGroup "Text.Show.Text.Distribution.Simple.Program.Types"
        [ testProperty "ConfiguredProgram instance" (prop_matchesShow :: Int -> ConfiguredProgram -> Bool)
        , testProperty "ProgramLocation instance"   (prop_matchesShow :: Int -> ProgramLocation -> Bool)
        ]
    ]