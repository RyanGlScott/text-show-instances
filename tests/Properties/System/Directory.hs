{-# LANGUAGE CPP #-}
{-|
Module:      Properties.System.Directory
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for 'Permissions'.
-}
module Properties.System.Directory (directoryTests) where

#if MIN_VERSION_directory(1,1,0)
import Instances.System.Directory ()

import Properties.Utils (prop_matchesShow)

import System.Directory (Permissions)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.System.Directory ()
#endif

directoryTests :: [TestTree]
directoryTests =
    [
#if MIN_VERSION_directory(1,1,0)
      testGroup "Text.Show.Text.System.Directory"
        [ testProperty "Permissions instance" (prop_matchesShow :: Int -> Permissions -> Bool)
        ]
#endif
    ]