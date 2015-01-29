{-|
Module:      Properties.Distribution.Text
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for the 'displayB' function.
-}
module Properties.Distribution.Text (cabalDistributionTextTests) where

import Distribution.Compiler (CompilerFlavor)
import Distribution.Text (Text, display)

import Instances.Distribution.Compiler ()

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

import Text.Show.Text (fromString)
import Text.Show.Text.Distribution.Text (displayB)

-- | Verifies that the output of 'display' and 'displayB' coincides.
prop_display :: (Arbitrary a, Text a) => a -> Bool
prop_display a = fromString (display a) == displayB a

cabalDistributionTextTests :: [TestTree]
cabalDistributionTextTests =
    [ testGroup "Text.Show.Text.Distribution.Text"
        [ testProperty "displayB output" (prop_display :: CompilerFlavor -> Bool)
        ]
    ]