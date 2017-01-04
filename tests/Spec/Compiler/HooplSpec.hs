{-|
Module:      Spec.Compiler.HooplSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @hoopl@ library.
-}
module Spec.Compiler.HooplSpec (main, spec) where

import Compiler.Hoopl (Label, LabelMap, LabelSet, Pointed,
                       Unique, UniqueMap, UniqueSet, C)
import Compiler.Hoopl.Passes.Dominator (DominatorNode, DominatorTree, DPath)

import Data.Proxy (Proxy(..))

import Instances.Compiler.Hoopl ()

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

import TextShow.Compiler.Hoopl ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Label" $
        matchesTextShowSpec (Proxy :: Proxy Label)
    describe "LabelMap Char" $
        matchesTextShowSpec (Proxy :: Proxy (LabelMap Char))
    describe "LabelSet" $
        matchesTextShowSpec (Proxy :: Proxy LabelSet)
    describe "Pointed C C Int" $
        matchesTextShowSpec (Proxy :: Proxy (Pointed C C Int))
    describe "Unique" $
        matchesTextShowSpec (Proxy :: Proxy Unique)
    describe "UniqueMap Char" $
        matchesTextShowSpec (Proxy :: Proxy (UniqueMap Char))
    describe "UniqueSet" $
        matchesTextShowSpec (Proxy :: Proxy UniqueSet)
    describe "DominatorNode" $
        matchesTextShowSpec (Proxy :: Proxy DominatorNode)
    describe "DominatorTree" $
        matchesTextShowSpec (Proxy :: Proxy DominatorTree)
    describe "DPath" $
        matchesTextShowSpec (Proxy :: Proxy DPath)
