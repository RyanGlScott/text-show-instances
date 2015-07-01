{-|
Module:      Spec.Trace.HpcSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for data types in the @hpc@ library.
-}
module Spec.Trace.HpcSpec (main, spec) where

import Instances.Trace.Hpc ()

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text.Trace.Hpc ()

import Trace.Hpc.Mix (Mix, BoxLabel, CondBox)
import Trace.Hpc.Tix (Tix, TixModule)
import Trace.Hpc.Util (HpcPos, Hash)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Mix" $
        prop "Show instance" (prop_matchesShow :: Int -> Mix -> Bool)
    describe "BoxLabel" $
        prop "Show instance" (prop_matchesShow :: Int -> BoxLabel -> Bool)
    describe "CondBox" $
        prop "Show instance" (prop_matchesShow :: Int -> CondBox -> Bool)
    describe "Tix" $
        prop "Show instance" (prop_matchesShow :: Int -> Tix -> Bool)
    describe "TixModule" $
        prop "Show instance" (prop_matchesShow :: Int -> TixModule -> Bool)
    describe "HpcPos" $
        prop "Show instance" (prop_matchesShow :: Int -> HpcPos -> Bool)
    describe "Hash" $
        prop "Show instance" (prop_matchesShow :: Int -> Hash -> Bool)
