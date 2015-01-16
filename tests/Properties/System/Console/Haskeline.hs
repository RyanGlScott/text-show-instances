{-|
Module:      Properties.System.Console.Haskeline
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @haskeline@ library.
-}
module Properties.System.Console.Haskeline (haskelineTests) where

import Instances.System.Console.Haskeline ()

import Properties.Utils (prop_matchesShow)

import System.Console.Haskeline (Interrupt, defaultPrefs)
import System.Console.Haskeline.Completion (Completion)
import System.Console.Haskeline.History (History)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?), testCase)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text (FromStringShow(..), showb)
import Text.Show.Text.System.Console.Haskeline ()

haskelineTests :: [TestTree]
haskelineTests =
    [ testGroup "Text.Show.Text.System.Console.Haskeline"
        [ testProperty "Interrupt instance"   (prop_matchesShow :: Int -> Interrupt -> Bool)
--         , testProperty "Prefs instance"       (prop_matchesShow :: Int -> Prefs -> Bool)
        , testCase "defaultPrefs Show output" $ showb (FromStringShow defaultPrefs) @=? showb defaultPrefs
        , testProperty "Completion instance"  (prop_matchesShow :: Int -> Completion -> Bool)
        , testProperty "History instance"     (prop_matchesShow :: Int -> History -> Bool)
        ]
    ]