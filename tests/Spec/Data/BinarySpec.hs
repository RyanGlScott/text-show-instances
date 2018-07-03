{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.BinarySpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Decoder's.
-}
module Spec.Data.BinarySpec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_binary(0,6,0)
import Data.Binary.Get.Internal (Decoder)
import Data.Proxy (Proxy(..))

import Instances.Data.Binary ()

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (describe)

import TextShow.Data.Binary ()
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_binary(0,6,0)
    describe "Decoder Int" $
        matchesTextShowSpec (Proxy :: Proxy (Decoder Int))
#else
    pure ()
#endif
