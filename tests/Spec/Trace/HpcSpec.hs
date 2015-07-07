{-|
Module:      Spec.Trace.HpcSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @hpc@ library.
-}
module Spec.Trace.HpcSpec (main, spec) where

import Instances.Trace.Hpc ()

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import TextShow.Trace.Hpc ()

import Trace.Hpc.Mix (Mix, BoxLabel, CondBox)
import Trace.Hpc.Tix (Tix, TixModule)
import Trace.Hpc.Util (HpcPos, Hash)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Mix" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Mix -> Bool)
    describe "BoxLabel" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> BoxLabel -> Bool)
    describe "CondBox" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CondBox -> Bool)
    describe "Tix" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Tix -> Bool)
    describe "TixModule" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> TixModule -> Bool)
    describe "HpcPos" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> HpcPos -> Bool)
    describe "Hash" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Hash -> Bool)
