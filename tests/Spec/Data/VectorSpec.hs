{-|
Module:      Spec.Data.VectorSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for 'Vector' types.
-}
module Spec.Data.VectorSpec (main, spec) where

import qualified Data.Vector as B (Vector)
import           Data.Vector.Fusion.Stream.Size (Size)
import qualified Data.Vector.Primitive as P (Vector)
import qualified Data.Vector.Storable as S (Vector)
import qualified Data.Vector.Unboxed as U (Vector)

import           Instances.Data.Vector ()

import           Spec.Utils (prop_matchesShow)

import           Test.Hspec (Spec, describe, hspec, parallel)
import           Test.Hspec.QuickCheck (prop)

import           Text.Show.Text.Data.Vector ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "(boxed) Vector Char" $
        prop "Show instance" (prop_matchesShow :: Int -> B.Vector Char -> Bool)
    describe "(primitive) Vector Char" $
        prop "Show instance" (prop_matchesShow :: Int -> P.Vector Char -> Bool)
    describe "(storable) Vector Char" $
        prop "Show instance" (prop_matchesShow :: Int -> S.Vector Char -> Bool)
    describe "(unboxed) Vector Char" $
        prop "Show instance" (prop_matchesShow :: Int -> U.Vector Char -> Bool)
    describe "Size" $
        prop "Show instance" (prop_matchesShow :: Int -> Size -> Bool)
