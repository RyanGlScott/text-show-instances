{-|
Module:      Properties.Distribution.Simple.Program.Db
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.Simple.Program.Db@
module of the @Cabal@ library.
-}
module Properties.Distribution.Simple.Program.Db (cabalDistributionSimpleProgramDbTests) where

import Distribution.Simple.Program.Db (ProgramDb)

import Instances.Distribution.Simple.Program.Db ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.Simple.Program.Db ()

cabalDistributionSimpleProgramDbTests :: [TestTree]
cabalDistributionSimpleProgramDbTests =
    [ testGroup "Text.Show.Text.Distribution.Simple.Program.Db"
        [ testProperty "ProgramDb instance" (prop_matchesShow :: Int -> ProgramDb -> Bool)
        ]
    ]