{-# LANGUAGE CPP #-}
{-|
Module:      Spec.GHC.LanguageExtensions.TypeSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for the 'Extension' data type.
-}
module Spec.GHC.LanguageExtensions.TypeSpec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if defined(MIN_VERSION_ghc_boot)
import GHC.LanguageExtensions.Type (Extension)

import Instances.GHC.LanguageExtensions.Type ()

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)

import TextShow.GHC.LanguageExtensions.Type ()
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if defined(MIN_VERSION_ghc_boot)
    describe "Extension" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Extension -> Bool)
#else
    pure ()
#endif
