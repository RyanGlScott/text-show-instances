{-|
Module:      Properties.Trace.Hpc
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @hpc@ library.
-}
module Properties.Trace.Hpc (hpcTests) where

import Instances.Trace.Hpc ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Trace.Hpc ()

import Trace.Hpc.Mix (Mix, BoxLabel, CondBox)
import Trace.Hpc.Tix (Tix, TixModule)
import Trace.Hpc.Util (HpcPos, Hash)

hpcTests :: [TestTree]
hpcTests =
    [ testGroup "Text.Show.Text.Trace.Hpc"
        [ testProperty "Mix instance"       (prop_matchesShow :: Int -> Mix -> Bool)
        , testProperty "BoxLabel instance"  (prop_matchesShow :: Int -> BoxLabel -> Bool)
        , testProperty "CondBox instance"   (prop_matchesShow :: Int -> CondBox -> Bool)
        , testProperty "Tix instance"       (prop_matchesShow :: Int -> Tix -> Bool)
        , testProperty "TixModule instance" (prop_matchesShow :: Int -> TixModule -> Bool)
        , testProperty "HpcPos instance"    (prop_matchesShow :: Int -> HpcPos -> Bool)
        , testProperty "Hash instance"      (prop_matchesShow :: Int -> Hash -> Bool)
        ]
    ]