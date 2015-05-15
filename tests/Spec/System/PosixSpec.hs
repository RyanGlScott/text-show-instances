{-# LANGUAGE CPP #-}
{-|
Module:      Spec.System.PosixSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for data types in the @unix@ library.
-}
module Spec.System.PosixSpec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if !defined(mingw32_HOST_OS)
import Instances.System.Posix ()

import Spec.Utils (prop_matchesShow)

import System.Posix.DynamicLinker (RTLDFlags, DL)
import System.Posix.Process (ProcessStatus)
import System.Posix.User (GroupEntry, UserEntry)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text.System.Posix ()
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
#if !defined(mingw32_HOST_OS)
    describe "Text.Show.Text.System.Posix" $ do
        prop "RTLDFlags instance"     (prop_matchesShow :: Int -> RTLDFlags -> Bool)
        prop "DL instance"            (prop_matchesShow :: Int -> DL -> Bool)
        prop "ProcessStatus instance" (prop_matchesShow :: Int -> ProcessStatus -> Bool)
        prop "GroupEntry instance"    (prop_matchesShow :: Int -> GroupEntry -> Bool)
        prop "UserEntry instance"     (prop_matchesShow :: Int -> UserEntry -> Bool)
#else
    pure ()
#endif
