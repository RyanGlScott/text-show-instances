{-|
Module:      Properties.Data.UnorderedContainers
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for 'HashMap's and 'HashSet's.
-}
module Properties.Data.UnorderedContainers (unorderedContainersTests) where

import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)

import Properties.Utils (prop_matchesShow)

import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Data.UnorderedContainers ()

unorderedContainersTests :: [TestTree]
unorderedContainersTests =
    [ testGroup "Text.Show.Text.Data.UnorderedContainers"
        [ testProperty "HashMap Char Char instance" (prop_matchesShow :: Int -> HashMap Char Char -> Bool)
        , testProperty "HashSet Char instance"      (prop_matchesShow :: Int -> HashSet Char -> Bool)
        ]
    ]