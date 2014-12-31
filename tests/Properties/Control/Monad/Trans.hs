{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-|
Module:      Properties.Control.Monad.Trans
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for monad transformers.
-}
module Properties.Control.Monad.Trans (monadTransformerTests) where

import           Control.Monad.Trans.Error               (ErrorT)
import           Control.Monad.Trans.Except              (ExceptT)
import           Control.Monad.Trans.Identity            (IdentityT)
import           Control.Monad.Trans.List                (ListT)
import           Control.Monad.Trans.Maybe               (MaybeT)
import qualified Control.Monad.Trans.Writer.Lazy   as WL (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as WS (WriterT)

import           Instances.Control.Monad.Trans ()

import           Properties.Utils (prop_matchesShow)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Text.Show.Text.Control.Monad.Trans ()

monadTransformerTests :: [TestTree]
monadTransformerTests =
    [ testGroup "Text.Show.Text.Control.Monad.Trans"
        [ testProperty "ErrorT Char Maybe Int instance"           (prop_matchesShow :: Int -> ErrorT Char Maybe Int -> Bool)
        , testProperty "ExceptT Char Maybe Int instance"          (prop_matchesShow :: Int -> ExceptT Char Maybe Int -> Bool)
        , testProperty "IdentityT Maybe Int instance"             (prop_matchesShow :: Int -> IdentityT Maybe Int -> Bool)
        , testProperty "ListT Maybe Char instance"                (prop_matchesShow :: Int -> ListT Maybe Char -> Bool)
        , testProperty "Maybe [] Int instance"                    (prop_matchesShow :: Int -> MaybeT [] Int -> Bool)
        , testProperty "lazy WriterT String Maybe Int instance"   (prop_matchesShow :: Int -> WL.WriterT String Maybe Int -> Bool)
        , testProperty "strict WriterT String Maybe Int instance" (prop_matchesShow :: Int -> WS.WriterT String Maybe Int -> Bool)
        ]
    ]