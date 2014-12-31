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

import Properties.Control.Applicative.Trans (applicativeFunctorTransformerTests)
import Properties.Control.Monad.Trans       (monadTransformerTests)
import Properties.Data.Containers           (containersTests)
import Properties.Data.Functor.Trans        (functorTransformerTests)
import Properties.Data.Tagged               (taggedTests)
import Properties.Data.Time                 (timeTests)
import Properties.Data.UnorderedContainers  (unorderedContainersTests)
import Properties.Data.Vector               (vectorTests)
import Properties.System.Directory          (directoryTests)
import Properties.System.Locale             (oldLocaleTests)
import Properties.System.Random             (randomTests)
import Properties.System.Time               (oldTimeTests)
import Properties.Text.PrettyPrint          (prettyTests)
import Properties.Text.XHtml                (xhtmlTests)
import Properties.Trace.Hpc                 (hpcTests)

import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain testTree

allTests :: [TestTree]
allTests = concat [ applicativeFunctorTransformerTests
                  , monadTransformerTests
                  , containersTests
                  , functorTransformerTests
                  , taggedTests
                  , timeTests
                  , unorderedContainersTests
                  , vectorTests
                  , directoryTests
                  , oldLocaleTests
                  , randomTests
                  , oldTimeTests
                  , prettyTests
                  , xhtmlTests
                  , hpcTests
                  ]

testTree :: TestTree
testTree = testGroup "QuickCheck properties" allTests