{-|
Module:      Properties
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ tests for @text-show-instances@.
-}
module Main (main) where

import Properties.Containers (containersTests)
import Properties.Time       (timeTests)

import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain testTree

allTests :: [TestTree]
allTests = concat [ containersTests
                  , timeTests
                  ]

testTree :: TestTree
testTree = testGroup "QuickCheck properties" allTests