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

#if defined(MIN_VERSION_ghc_boot_th)
import Data.Proxy (Proxy(..))
import GHC.LanguageExtensions.Type (Extension)
import Instances.GHC.LanguageExtensions.Type ()
import Spec.Utils (matchesTextShowSpec)
import Test.Hspec (describe)
import TextShow.GHC.LanguageExtensions.Type ()
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if defined(MIN_VERSION_ghc_boot_th)
    describe "Extension" $
        matchesTextShowSpec (Proxy :: Proxy Extension)
#else
    pure ()
#endif
