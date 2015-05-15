{-|
Module:      Spec.System.Console.HaskelineSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for data types in the @haskeline@ library.
-}
module Spec.System.Console.HaskelineSpec (main, spec) where

import Instances.System.Console.Haskeline ()

import Spec.Utils (prop_matchesShow)

import System.Console.Haskeline (Interrupt, defaultPrefs)
import System.Console.Haskeline.Completion (Completion)
import System.Console.Haskeline.History (History)

import Test.Hspec (Spec, describe, hspec, it, parallel, shouldBe)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text (FromStringShow(..), showb)
import Text.Show.Text.System.Console.Haskeline ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.System.Console.Haskeline" $ do
    prop "Interrupt instance"       (prop_matchesShow :: Int -> Interrupt -> Bool)
--     prop "Prefs instance"           (prop_matchesShow :: Int -> Prefs -> Bool)
    it "defaultPrefs Show output" $ showb (FromStringShow defaultPrefs) `shouldBe` showb defaultPrefs
    prop "Completion instance"      (prop_matchesShow :: Int -> Completion -> Bool)
    prop "History instance"         (prop_matchesShow :: Int -> History -> Bool)
