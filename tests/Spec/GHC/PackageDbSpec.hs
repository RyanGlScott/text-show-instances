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
import GHC.PackageDb

import Instances.GHC.PackageDb ()

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)

import TextShow.GHC.PackageDb ()

ipiString :: String
# if __GLASGOW_HASKELL__ >= 801
type IPI = InstalledPackageInfo Int Int Int Int Int
ipiString = "InstalledPackageInfo Int Int Int Int Int"
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
        prop "TextShow instance" (prop_matchesTextShow :: Int -> IPI -> Bool)
# if __GLASGOW_HASKELL__ >= 801
    describe "DbModule Int Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> DbModule Int Int -> Bool)
# else
    describe "OriginalModule Int Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> OriginalModule Int Int -> Bool)
    describe "ExposedModule Int Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ExposedModule Int Int -> Bool)
# endif
#else
    pure ()
#endif
