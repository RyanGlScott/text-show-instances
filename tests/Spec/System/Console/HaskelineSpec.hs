{-|
Module:      Spec.System.Console.HaskelineSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @haskeline@ library.
-}
module Spec.System.Console.HaskelineSpec (main, spec) where

import Instances.System.Console.Haskeline ()

import Spec.Utils (prop_matchesTextShow)

import System.Console.Haskeline (Interrupt, defaultPrefs)
import System.Console.Haskeline.Completion (Completion)
import System.Console.Haskeline.History (History)

import Test.Hspec (Spec, describe, hspec, it, parallel, shouldBe)
import Test.Hspec.QuickCheck (prop)

import TextShow (FromStringShow(..), showb)
import TextShow.System.Console.Haskeline ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Interrupt" $
        prop "TextShow instance"                                  (prop_matchesTextShow :: Int -> Interrupt -> Bool)
--     describe "Prefs" $
--         prop "TextShow instance"                                  (prop_matchesTextShow :: Int -> Prefs -> Bool)
    describe "defaultPrefs" $
        it "should have coinciding string and text Show output" $ showb (FromStringShow defaultPrefs) `shouldBe` showb defaultPrefs
    describe "Completion" $
        prop "TextShow instance"                                  (prop_matchesTextShow :: Int -> Completion -> Bool)
    describe "History" $
        prop "TextShow instance"                                  (prop_matchesTextShow :: Int -> History -> Bool)
