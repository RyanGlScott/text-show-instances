{-# LANGUAGE CPP #-}
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
import Properties.Data.List.NonEmpty        (nonEmptyListTests)
import Properties.Data.Semigroup            (semigroupTests)
import Properties.Data.Tagged               (taggedTests)
import Properties.Data.Time                 (timeTests)
import Properties.Data.UnorderedContainers  (unorderedContainersTests)
import Properties.Data.Vector               (vectorTests)
#if defined(mingw32_HOST_OS)
import Properties.Graphics.Win32            (win32GraphicsTests)
#endif
import Properties.Language.Haskell.TH       (templateHaskellTests)
import Properties.System.Directory          (directoryTests)
import Properties.System.Locale             (oldLocaleTests)
#if !defined(mingw32_HOST_OS)
import Properties.System.Posix              (unixTests)
#endif
import Properties.System.Random             (randomTests)
import Properties.System.Time               (oldTimeTests)
#if defined(mingw32_HOST_OS)
import Properties.System.Win32              (win32SystemTests)
#endif
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
                  , nonEmptyListTests
                  , semigroupTests
                  , taggedTests
                  , timeTests
                  , unorderedContainersTests
                  , vectorTests
#if defined(mingw32_HOST_OS)
                  , win32GraphicsTests
#endif
                  , templateHaskellTests
                  , directoryTests
                  , oldLocaleTests
#if !defined(mingw32_HOST_OS)
                  , unixTests
#endif
                  , randomTests
                  , oldTimeTests
#if defined(mingw32_HOST_OS)
                  , win32SystemTests
#endif
                  , prettyTests
                  , xhtmlTests
                  , hpcTests
                  ]

testTree :: TestTree
testTree = testGroup "QuickCheck properties" allTests