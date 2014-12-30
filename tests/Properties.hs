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

import Properties.Data.Containers          (containersTests)
import Properties.Data.Tagged              (taggedTests)
import Properties.Data.Time                (timeTests)
import Properties.Data.UnorderedContainers (unorderedContainersTests)
import Properties.System.Random            (randomTests)
import Properties.System.Time              (oldTimeTests)
import Properties.Text.PrettyPrint         (prettyTests)

import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain testTree

allTests :: [TestTree]
allTests = concat [ containersTests
                  , taggedTests
                  , timeTests
                  , unorderedContainersTests
                  , randomTests
                  , oldTimeTests
                  , prettyTests
                  ]

testTree :: TestTree
testTree = testGroup "QuickCheck properties" allTests