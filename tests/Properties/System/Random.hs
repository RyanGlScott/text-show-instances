{-|
Module:      Properties.System.Random
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for 'StdGen' values.
-}
module Properties.System.Random (randomTests) where

import Instances.System.Random ()

import Properties.Utils (prop_matchesShow)

import System.Random (StdGen)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.System.Random ()

randomTests :: [TestTree]
randomTests =
    [ testGroup "Text.Show.Text.System.Random"
        [ testProperty "StdGen instance" (prop_matchesShow :: Int -> StdGen -> Bool)
        ]
    ]