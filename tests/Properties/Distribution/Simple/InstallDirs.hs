{-|
Module:      Properties.Distribution.Simple.InstallDirs
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Distribution.Simple.InstallDirs@
module of the @Cabal@ library.
-}
module Properties.Distribution.Simple.InstallDirs (cabalDistributionSimpleInstallDirsTests) where

import Distribution.Simple.InstallDirs (CopyDest, InstallDirs,
                                        PathTemplate, PathTemplateVariable)

import Instances.Distribution.Simple.InstallDirs ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Distribution.Simple.InstallDirs ()

cabalDistributionSimpleInstallDirsTests :: [TestTree]
cabalDistributionSimpleInstallDirsTests =
    [ testGroup "Text.Show.Text.Distribution.Simple.InstallDirs"
        [ testProperty "CopyDest instance"             (prop_matchesShow :: Int -> CopyDest -> Bool)
        , testProperty "InstallDirs Int instance"      (prop_matchesShow :: Int -> InstallDirs Int -> Bool)
        , testProperty "PathTemplate instance"         (prop_matchesShow :: Int -> PathTemplate -> Bool)
        , testProperty "PathTemplateVariable instance" (prop_matchesShow :: Int -> PathTemplateVariable -> Bool)
        ]
    ]