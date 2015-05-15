{-# LANGUAGE CPP #-}
{-|
Module:      Spec.System.Console.TerminfoSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for data types in the @terminfo@ library.
-}
module Spec.System.Console.TerminfoSpec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if !defined(mingw32_HOST_OS)
import Instances.System.Console.Terminfo ()

import Spec.Utils (prop_matchesShow)

import System.Console.Terminfo.Color (Color)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text.System.Console.Terminfo ()
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
#if !defined(mingw32_HOST_OS)
    describe "Text.Show.Text.System.Console.Terminfo" $ do
        prop "Color instance"          (prop_matchesShow :: Int -> Color -> Bool)
--         prop "SetupTermError instance" (prop_matchesShow :: Int -> SetupTermError -> Bool)
#else
    pure ()
#endif
