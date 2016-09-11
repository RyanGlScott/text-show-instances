{-# LANGUAGE CPP                #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric      #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Data.Binary
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides an 'Arbitrary' instance for 'Decoder's.
-}
module Instances.Data.Binary () where

import           Data.Binary.Get.Internal (Decoder(..))

#if __GLASGOW_HASKELL__ >= 702
import           GHC.Generics (Generic)
#else
import qualified Generics.Deriving.TH as Generics (deriveAll0)
#endif

import           Test.QuickCheck (Arbitrary(..), genericArbitrary)
import           Test.QuickCheck.Instances ()

instance Arbitrary a => Arbitrary (Decoder a) where
    arbitrary = genericArbitrary

#if __GLASGOW_HASKELL__ >= 702
deriving instance Generic (Decoder a)
#else
$(Generics.deriveAll0 ''Decoder)
#endif
