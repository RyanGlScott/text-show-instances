{-|
Module:      Properties.Data.Binary
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for 'Decoder's.
-}
module Properties.Data.Binary (binaryTests) where

import Data.Binary.Get.Internal (Decoder)

import Instances.Data.Binary ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Data.Binary ()

binaryTests :: [TestTree]
binaryTests =
    [ testGroup "Text.Show.Text.Data.Binary"
        [ testProperty "Decoder Int instance" (prop_matchesShow :: Int -> Decoder Int -> Bool)
        ]
    ]