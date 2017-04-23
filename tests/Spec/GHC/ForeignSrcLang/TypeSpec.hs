{-# LANGUAGE CPP #-}

{-|
Module:      Spec.GHC.ForeignSrcLang.TypeSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for the 'ForeignSrcLang' data type.
-}
module Spec.GHC.ForeignSrcLang.TypeSpec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if defined(MIN_VERSION_ghc_boot_th)
# if MIN_VERSION_ghc_boot_th(8,2,0)
import Data.Proxy (Proxy(..))
import GHC.ForeignSrcLang.Type (ForeignSrcLang)
import Instances.GHC.ForeignSrcLang.Type ()
import Spec.Utils (matchesTextShowSpec)
import Test.Hspec (describe)
import TextShow.GHC.ForeignSrcLang.Type ()
# endif
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if defined(MIN_VERSION_ghc_boot_th) && MIN_VERSION_ghc_boot_th(8,2,0)
    describe "ForeignSrcLang" $
        matchesTextShowSpec (Proxy :: Proxy ForeignSrcLang)
#else
    pure ()
#endif
