{-|
Module:      Properties.Data.String.UTF8
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for 'UTF8' strings.
-}
module Properties.Data.String.UTF8 (utf8StringTests) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.String.UTF8 (UTF8)
import           Data.Word (Word8)

import           Instances.Data.String.UTF8 ()

import           Properties.Utils (prop_matchesShow)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Text.Show.Text.Data.String.UTF8 ()

utf8StringTests :: [TestTree]
utf8StringTests =
    [ testGroup "Text.Show.Text.Data.String.UTF8"
        [ testProperty "UTF8 ByteString (strict) instance" (prop_matchesShow :: Int -> UTF8 BS.ByteString -> Bool)
        , testProperty "UTF8 ByteString (lazy) instance"   (prop_matchesShow :: Int -> UTF8 BL.ByteString -> Bool)
        , testProperty "UTF8 [Word8] instance"             (prop_matchesShow :: Int -> UTF8 [Word8] -> Bool)
        ]
    ]