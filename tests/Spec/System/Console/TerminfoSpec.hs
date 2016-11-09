{-# LANGUAGE CPP #-}
{-|
Module:      Spec.System.Console.TerminfoSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @terminfo@ library.
-}
module Spec.System.Console.TerminfoSpec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if !defined(mingw32_HOST_OS)
import Instances.System.Console.Terminfo ()

import Spec.Utils (prop_matchesTextShow)

import System.Console.Terminfo (Color, SetupTermError)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)

import TextShow.System.Console.Terminfo ()
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if !defined(mingw32_HOST_OS)
    describe "Color" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Color -> Bool)
    describe "SetupTermError" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> SetupTermError -> Bool)
#else
    pure ()
#endif
