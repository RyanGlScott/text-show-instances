{-|
Module:      Properties.Data.Vector
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for 'Vector' types.
-}
module Properties.Data.Vector (vectorTests) where

import qualified Data.Vector as B (Vector)
import           Data.Vector.Fusion.Stream.Size (Size)
import qualified Data.Vector.Primitive as P (Vector)
import qualified Data.Vector.Storable as S (Vector)
import qualified Data.Vector.Unboxed as U (Vector)

import Instances.Data.Vector ()

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text.Data.Vector ()

vectorTests :: [TestTree]
vectorTests =
    [ testGroup "Text.Show.Text.Data.Vector"
        [ testProperty "(boxed)     Vector Char instance" (prop_matchesShow :: Int -> B.Vector Char -> Bool)
        , testProperty "(primitive) Vector Char instance" (prop_matchesShow :: Int -> P.Vector Char -> Bool)
        , testProperty "(storable)  Vector Char instance" (prop_matchesShow :: Int -> S.Vector Char -> Bool)
        , testProperty "(unboxed)   Vector Char instance" (prop_matchesShow :: Int -> U.Vector Char -> Bool)
        , testProperty "Size instance"                    (prop_matchesShow :: Int -> Size -> Bool)
        ]
    ]