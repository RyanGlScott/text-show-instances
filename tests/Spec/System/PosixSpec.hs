{-# LANGUAGE CPP #-}
{-|
Module:      Spec.System.PosixSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @unix@ library.
-}
module Spec.System.PosixSpec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if !defined(mingw32_HOST_OS)
import Instances.System.Posix ()

import Spec.Utils (prop_matchesTextShow)

import System.Posix.DynamicLinker (RTLDFlags, DL)
import System.Posix.Process (ProcessStatus)
import System.Posix.User (GroupEntry, UserEntry)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)

import TextShow.System.Posix ()
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if !defined(mingw32_HOST_OS)
    describe "RTLDFlags" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> RTLDFlags -> Bool)
    describe "DL" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> DL -> Bool)
    describe "ProcessStatus" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ProcessStatus -> Bool)
    describe "GroupEntry" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> GroupEntry -> Bool)
    describe "UserEntry" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> UserEntry -> Bool)
#else
    pure ()
#endif
