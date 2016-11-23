{-|
Module:      Spec.Data.BifunctorSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @bifunctors@ library.
-}
module Spec.Data.BifunctorSpec (main, spec) where

import Data.Bifunctor.Biff (Biff)
import Data.Bifunctor.Clown (Clown)
import Data.Bifunctor.Fix (Fix)
import Data.Bifunctor.Flip (Flip)
import Data.Bifunctor.Join (Join)
import Data.Bifunctor.Joker (Joker)
import Data.Bifunctor.Product (Product)
import Data.Bifunctor.Sum (Sum)
import Data.Bifunctor.Tannen (Tannen)
import Data.Bifunctor.Wrapped (WrappedBifunctor)
import Data.Proxy (Proxy(..))

import Instances.Data.Bifunctor ()

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

import TextShow.Data.Bifunctor ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Biff Either [] Maybe Char Int" $
        matchesTextShowSpec (Proxy :: Proxy (Biff Either [] Maybe Char Int))
    describe "Clown [] Char Int" $
        matchesTextShowSpec (Proxy :: Proxy (Clown [] Char Int))
    describe "Fix Either Int" $
        matchesTextShowSpec (Proxy :: Proxy (Fix Either Int))
    describe "Flip Either Int Char" $
        matchesTextShowSpec (Proxy :: Proxy (Flip Either Int Char))
    describe "Join Either Int" $
        matchesTextShowSpec (Proxy :: Proxy (Join Either Int))
    describe "Joker [] Char Int" $
        matchesTextShowSpec (Proxy :: Proxy (Joker [] Char Int))
    describe "Product Either ((,,) Bool) Int Char" $
        matchesTextShowSpec (Proxy :: Proxy (Product Either ((,,) Bool) Int Char))
    describe "Sum Either ((,,) Bool) Int Char" $
        matchesTextShowSpec (Proxy :: Proxy (Sum Either ((,,) Bool) Int Char))
    describe "Tannen Maybe Either Int Char" $
        matchesTextShowSpec (Proxy :: Proxy (Tannen Maybe Either Int Char))
    describe "WrappedBifunctor Either Int Char" $
        matchesTextShowSpec (Proxy :: Proxy (WrappedBifunctor Either Int Char))
