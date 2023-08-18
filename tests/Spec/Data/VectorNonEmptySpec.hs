{-|
Module:      Spec.Data.Vector.NonEmptySpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Vector' types.
-}
module Spec.Data.VectorNonEmptySpec (main, spec) where

import           Data.Proxy (Proxy(..))
import qualified Data.Vector.NonEmpty as B (NonEmptyVector)

import           Instances.Data.VectorNonEmpty ()

import           Spec.Utils (matchesTextShowSpec, matchesTextShow1Spec)

import           Test.Hspec (Spec, describe, hspec, parallel)

import           TextShow.Data.VectorNonEmpty ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "(boxed) Vector Char" $ do
        let p :: Proxy (B.NonEmptyVector Char)
            p = Proxy
        matchesTextShowSpec  p
        matchesTextShow1Spec p
