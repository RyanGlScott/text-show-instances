{-# LANGUAGE CPP #-}
{-|
Module:      Spec.System.DirectorySpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Permissions'.
-}
module Spec.System.DirectorySpec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_directory(1,1,0)
import Data.Proxy (Proxy(..))

import Instances.System.Directory ()

import Spec.Utils (matchesTextShowSpec)

import System.Directory (Permissions)
# if MIN_VERSION_directory(1,2,3)
import System.Directory (XdgDirectory)
# endif

import Test.Hspec (describe)

import TextShow.System.Directory ()
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_directory(1,1,0)
    describe "Permissions" $
        matchesTextShowSpec (Proxy :: Proxy Permissions)
# if MIN_VERSION_directory(1,2,3)
    describe "XdgDirectory" $
        matchesTextShowSpec (Proxy :: Proxy XdgDirectory)
# endif
#else
    pure ()
#endif
