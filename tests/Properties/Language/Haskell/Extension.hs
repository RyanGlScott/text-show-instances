{-|
Module:      Properties.Language.Haskell.Extension
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @Language.Haskell.Extension@
module of the @Cabal@ library.
-}
module Properties.Language.Haskell.Extension (cabalLanguageHaskellExtensionTests) where

import Instances.Language.Haskell.Extension ()

import Language.Haskell.Extension (Extension, KnownExtension, Language)

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Language.Haskell.Extension ()

cabalLanguageHaskellExtensionTests :: [TestTree]
cabalLanguageHaskellExtensionTests =
    [ testGroup "Text.Show.Text.Language.Haskell.Extension"
        [ testProperty "Extension instance"      (prop_matchesShow :: Int -> Extension -> Bool)
        , testProperty "KnownExtension instance" (prop_matchesShow :: Int -> KnownExtension -> Bool)
        , testProperty "Language instance"       (prop_matchesShow :: Int -> Language -> Bool)
        ]
    ]