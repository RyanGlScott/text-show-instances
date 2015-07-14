{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.VectorSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Vector' types.
-}
module Spec.Data.VectorSpec (main, spec) where

import qualified Data.Vector as B (Vector)
#if MIN_VERSION_vector(0,11,0)
import           Data.Vector.Fusion.Bundle.Size (Size)
#else
import           Data.Vector.Fusion.Stream.Size (Size)
#endif
import qualified Data.Vector.Primitive as P (Vector)
import qualified Data.Vector.Storable as S (Vector)
import qualified Data.Vector.Unboxed as U (Vector)

import           Instances.Data.Vector ()

import           Spec.Utils (prop_matchesTextShow)

import           Test.Hspec (Spec, describe, hspec, parallel)
import           Test.Hspec.QuickCheck (prop)

import           TextShow.Data.Vector ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "(boxed) Vector Char" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> B.Vector Char -> Bool)
    describe "(primitive) Vector Char" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> P.Vector Char -> Bool)
    describe "(storable) Vector Char" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> S.Vector Char -> Bool)
    describe "(unboxed) Vector Char" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> U.Vector Char -> Bool)
    describe "Size" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Size -> Bool)
