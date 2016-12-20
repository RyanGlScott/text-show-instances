{-# LANGUAGE CPP                #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric      #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Data.Containers
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for data types located in @containers@.
-}
module Instances.Data.Containers () where

import           Data.Graph (SCC(..))
import           Data.Sequence (ViewL(..), ViewR(..))

#if !(MIN_VERSION_containers(0,5,9))
# if __GLASGOW_HASKELL__ >= 706
import           GHC.Generics (Generic)
# else
import qualified Generics.Deriving.TH as Generics (deriveAll0)
# endif
#endif

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Test.QuickCheck (Arbitrary(..))
import           Test.QuickCheck.Instances ()

instance Arbitrary vertex => Arbitrary (SCC vertex) where
    arbitrary = genericArbitrary

instance Arbitrary a => Arbitrary (ViewL a) where
    arbitrary = genericArbitrary

instance Arbitrary a => Arbitrary (ViewR a) where
    arbitrary = genericArbitrary

#if !(MIN_VERSION_containers(0,5,8))
# if __GLASGOW_HASKELL__ >= 706
deriving instance Generic (ViewL a)
deriving instance Generic (ViewR a)
# else
$(Generics.deriveAll0 ''ViewL)
$(Generics.deriveAll0 ''ViewR)
# endif
#endif

#if !(MIN_VERSION_containers(0,5,9))
deriving instance Show vertex => Show (SCC vertex)

# if __GLASGOW_HASKELL__ >= 706
deriving instance Generic (SCC vertex)
# else
$(Generics.deriveAll0 ''SCC)
# endif
#endif
