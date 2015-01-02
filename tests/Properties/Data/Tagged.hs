{-|
Module:      Properties.Data.Tagged
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for 'Tagged' values.
-}
module Properties.Data.Tagged (taggedTests) where

import Data.Tagged (Tagged)

import Instances.Data.Tagged ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Data.Tagged ()

taggedTests :: [TestTree]
taggedTests =
    [ testGroup "Text.Show.Text.Data.Tagged"
        [ testProperty "Tagged Char Int instance" (prop_matchesShow :: Int -> Tagged Char Int -> Bool)
        ]
    ]