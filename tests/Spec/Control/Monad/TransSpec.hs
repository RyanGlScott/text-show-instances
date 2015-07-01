{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-|
Module:      Spec.Control.Monad.TransSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
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

import           Spec.Utils (prop_matchesShow)

import           Test.Hspec (Spec, describe, hspec, parallel)
import           Test.Hspec.QuickCheck (prop)

import           Text.Show.Text.Control.Monad.Trans ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "ErrorT Char Maybe Int" $
        prop "Show instance" (prop_matchesShow :: Int -> ErrorT Char Maybe Int -> Bool)
    describe "ExceptT Char Maybe Int" $
        prop "Show instance" (prop_matchesShow :: Int -> ExceptT Char Maybe Int -> Bool)
    describe "IdentityT Maybe Int" $
        prop "Show instance" (prop_matchesShow :: Int -> IdentityT Maybe Int -> Bool)
    describe "ListT Maybe Char" $
        prop "Show instance" (prop_matchesShow :: Int -> ListT Maybe Char -> Bool)
    describe "Maybe [] Int" $
        prop "Show instance" (prop_matchesShow :: Int -> MaybeT [] Int -> Bool)
    describe "lazy WriterT String Maybe Int" $
        prop "Show instance" (prop_matchesShow :: Int -> WL.WriterT String Maybe Int -> Bool)
    describe "strict WriterT String Maybe Int" $
        prop "Show instance" (prop_matchesShow :: Int -> WS.WriterT String Maybe Int -> Bool)
