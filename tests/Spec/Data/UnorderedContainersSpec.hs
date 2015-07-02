{-|
Module:      Spec.Data.UnorderedContainersSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'HashMap's and 'HashSet's.
-}
module Spec.Data.UnorderedContainersSpec (main, spec) where

import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances ()

import Text.Show.Text.Data.UnorderedContainers ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "HashMap Char Char" $
        prop "Show instance" (prop_matchesShow :: Int -> HashMap Char Char -> Bool)
    describe "HashSet Char" $
        prop "Show instance" (prop_matchesShow :: Int -> HashSet Char -> Bool)
