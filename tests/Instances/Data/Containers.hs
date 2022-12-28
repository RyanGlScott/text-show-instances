{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Data.Containers
Copyright:   (C) 2014-2017 Ryan Scott
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
import           GHC.Generics (Generic)
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
deriving instance Generic (ViewL a)
deriving instance Generic (ViewR a)
#endif

#if !(MIN_VERSION_containers(0,5,9))
deriving instance Show vertex => Show (SCC vertex)
deriving instance Generic (SCC vertex)
#endif
