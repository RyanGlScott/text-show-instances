{-|
Module:      Properties.System.Console.Terminfo
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @terminfo@ library.
-}
module Properties.System.Console.Terminfo (terminfoTests) where

import Instances.System.Console.Terminfo ()

import Properties.Utils (prop_matchesShow)

import System.Console.Terminfo.Color (Color)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.System.Console.Terminfo ()

terminfoTests :: [TestTree]
terminfoTests =
    [ testGroup "Text.Show.Text.System.Console.Terminfo"
        [ testProperty "Color instance"          (prop_matchesShow :: Int -> Color -> Bool)
--         , testProperty "SetupTermError instance" (prop_matchesShow :: Int -> SetupTermError -> Bool)
        ]
    ]