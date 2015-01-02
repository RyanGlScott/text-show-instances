{-|
Module:      Properties.System.Posix
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @unix@ library.
-}
module Properties.System.Posix (unixTests) where

import Instances.System.Posix ()

import Properties.Utils (prop_matchesShow)

import System.Posix.DynamicLinker (RTLDFlags, DL)
import System.Posix.Process (ProcessStatus)
import System.Posix.User (GroupEntry, UserEntry)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.System.Posix ()

unixTests :: [TestTree]
unixTests =
    [ testGroup "Text.Show.Text.System.Posix"
        [ testProperty "RTLDFlags instance"     (prop_matchesShow :: Int -> RTLDFlags -> Bool)
        , testProperty "DL instance"            (prop_matchesShow :: Int -> DL -> Bool)
        , testProperty "ProcessStatus instance" (prop_matchesShow :: Int -> ProcessStatus -> Bool)
        , testProperty "GroupEntry instance"    (prop_matchesShow :: Int -> GroupEntry -> Bool)
        , testProperty "UserEntry instance"     (prop_matchesShow :: Int -> UserEntry -> Bool)
        ]
    ]