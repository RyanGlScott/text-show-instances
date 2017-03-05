{-# LANGUAGE CPP #-}
{-|
Module:      Spec.System.ProcessSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for @process@.
-}
module Spec.System.ProcessSpec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_process(1,4,3)
import Data.Proxy (Proxy(..))
import Instances.System.Process ()
import Spec.Utils (matchesTextShowSpec)
import System.Process (CmdSpec, CreateProcess, StdStream)
import Test.Hspec (describe)
import TextShow.System.Process ()
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_process(1,4,3)
    describe "CmdSpec" $
        matchesTextShowSpec (Proxy :: Proxy CmdSpec)
    describe "CreateProcess" $
        matchesTextShowSpec (Proxy :: Proxy CreateProcess)
    describe "StdStream" $
        matchesTextShowSpec (Proxy :: Proxy StdStream)
#else
    pure ()
#endif
