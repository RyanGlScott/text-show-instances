{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-|
Module:      Spec.Control.Monad.TransSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for monad transformers.
-}
module Spec.Control.Monad.TransSpec (main, spec) where

import           Control.Monad.Trans.Error               (ErrorT)
import           Control.Monad.Trans.Except              (ExceptT)
import           Control.Monad.Trans.Identity            (IdentityT)
import           Control.Monad.Trans.List                (ListT)
import           Control.Monad.Trans.Maybe               (MaybeT)
import qualified Control.Monad.Trans.Writer.Lazy   as WL (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as WS (WriterT)

import           Instances.Control.Monad.Trans ()

import           Spec.Utils (prop_matchesTextShow)

import           Test.Hspec (Spec, describe, hspec, parallel)
import           Test.Hspec.QuickCheck (prop)

import           TextShow.Control.Monad.Trans ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "ErrorT Char Maybe Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ErrorT Char Maybe Int -> Bool)
    describe "ExceptT Char Maybe Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ExceptT Char Maybe Int -> Bool)
    describe "IdentityT Maybe Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> IdentityT Maybe Int -> Bool)
    describe "ListT Maybe Char" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ListT Maybe Char -> Bool)
    describe "Maybe [] Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> MaybeT [] Int -> Bool)
    describe "lazy WriterT String Maybe Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> WL.WriterT String Maybe Int -> Bool)
    describe "strict WriterT String Maybe Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> WS.WriterT String Maybe Int -> Bool)
