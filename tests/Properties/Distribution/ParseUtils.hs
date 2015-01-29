{-|
Module:      Properties.Distribution.ParseUtils
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.ParseUtils@
module of the @Cabal@ library.
-}
module Properties.Distribution.ParseUtils (cabalDistributionParseUtilsTests) where

import Distribution.ParseUtils (Field, ParseResult, PError, PWarning)

import Instances.Distribution.ParseUtils ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.ParseUtils ()

cabalDistributionParseUtilsTests :: [TestTree]
cabalDistributionParseUtilsTests =
    [ testGroup "Text.Show.Text.Distribution.ParseUtils"
        [ testProperty "Field instance"           (prop_matchesShow :: Int -> Field -> Bool)
        , testProperty "ParseResult Int instance" (prop_matchesShow :: Int -> ParseResult Int -> Bool)
        , testProperty "PError instance"          (prop_matchesShow :: Int -> PError -> Bool)
        , testProperty "PWarning instance"        (prop_matchesShow :: Int -> PWarning -> Bool)
        ]
    ]