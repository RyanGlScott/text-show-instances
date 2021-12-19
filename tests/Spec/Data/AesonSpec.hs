{-|
Module:      Spec.Data.AesonSpec
Copyright:   (C) 2014-2018 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Aeson' (from the @aeson@ package).
-}
module Spec.Data.AesonSpec (main, spec) where

import           Data.Proxy                (Proxy (..))
import           Data.Aeson

import           Spec.Utils                (matchesTextShowSpec)

import           Test.Hspec                (Spec, describe, hspec, parallel)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           TextShow.Data.Aeson       ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Aeson" $
        matchesTextShowSpec (Proxy :: Proxy Value)
