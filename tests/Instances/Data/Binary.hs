{-# LANGUAGE CPP                #-}
#if MIN_VERSION_binary(0,6,0)
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      Instances.Data.Binary
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides an 'Arbitrary' instance for 'Decoder's.
-}
module Instances.Data.Binary () where

#if MIN_VERSION_binary(0,6,0)
import           Data.Binary.Get.Internal (Decoder(..))

# if __GLASGOW_HASKELL__ >= 706
import           GHC.Generics (Generic)
# else
import qualified Generics.Deriving.TH as Generics (deriveAll0)
# endif

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Test.QuickCheck (Arbitrary(..))
import           Test.QuickCheck.Instances ()

instance Arbitrary a => Arbitrary (Decoder a) where
    arbitrary = genericArbitrary

# if __GLASGOW_HASKELL__ >= 706
deriving instance Generic (Decoder a)
# else
$(Generics.deriveAll0 ''Decoder)
# endif
#endif
