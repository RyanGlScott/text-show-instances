{-# LANGUAGE CPP #-}
{-|
Module:      Spec.GHC.PackageDbSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "GHC.PackageDb" module.
-}
module Spec.GHC.PackageDbSpec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if defined(MIN_VERSION_ghc_boot)
import Data.Proxy (Proxy(..))
import GHC.PackageDb
import Instances.GHC.PackageDb ()
import Spec.Utils (matchesTextShowSpec)
import Test.Hspec (describe)
import TextShow.GHC.PackageDb ()

ipiString :: String
# if MIN_VERSION_ghc_boot(8,1,0)
type IPI = InstalledPackageInfo Int Int Int Int Int Int Int
ipiString = "InstalledPackageInfo Int Int Int Int Int Int Int"
# else
type IPI = InstalledPackageInfo Int Int Int Int
ipiString = "InstalledPackageInfo Int Int Int Int"
# endif
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if defined(MIN_VERSION_ghc_boot)
    describe ipiString $
        matchesTextShowSpec (Proxy :: Proxy IPI)
# if MIN_VERSION_ghc_boot(8,1,0)
    describe "DbModule Int Int Int Int Int" $
        matchesTextShowSpec (Proxy :: Proxy (DbModule Int Int Int Int Int))
# else
    describe "OriginalModule Int Int" $
        matchesTextShowSpec (Proxy :: Proxy (OriginalModule Int Int))
    describe "ExposedModule Int Int" $
        matchesTextShowSpec (Proxy :: Proxy (ExposedModule Int Int))
# endif
#else
    pure ()
#endif
