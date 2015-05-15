{-|
Module:      Spec.Data.String.UTF8Spec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for 'UTF8' strings.
-}
module Spec.Data.String.UTF8Spec (main, spec) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.String.UTF8 (UTF8)
import           Data.Word (Word8)

import           Instances.Data.String.UTF8 ()

import           Spec.Utils (prop_matchesShow)

import           Test.Hspec (Spec, describe, hspec, parallel)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck.Instances ()

import           Text.Show.Text.Data.String.UTF8 ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Data.String.UTF8" $ do
    prop "UTF8 ByteString (strict) instance" (prop_matchesShow :: Int -> UTF8 BS.ByteString -> Bool)
    prop "UTF8 ByteString (lazy) instance"   (prop_matchesShow :: Int -> UTF8 BL.ByteString -> Bool)
    prop "UTF8 [Word8] instance"             (prop_matchesShow :: Int -> UTF8 [Word8] -> Bool)
