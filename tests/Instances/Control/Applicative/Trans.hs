{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Control.Applicative.Trans
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for applicative functor transformers.
-}
module Instances.Control.Applicative.Trans () where

import           Control.Applicative.Backwards (Backwards(..))
import           Control.Applicative.Lift      (Lift(..))
import           Control.Monad.Trans.Instances ()

-- TODO: Remove the MIN_VERSION_transformers_compat(0,7,0) by unconditionally
-- depending on transformers-0.7 or later
#if !(MIN_VERSION_transformers(0,6,0)) && !(MIN_VERSION_transformers_compat(0,7,0))
import           GHC.Generics (Generic)
#endif

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Test.QuickCheck (Arbitrary(..))

deriving instance Arbitrary (f a) => Arbitrary (Backwards f a)

instance (Arbitrary a, Arbitrary (f a)) => Arbitrary (Lift f a) where
    arbitrary = genericArbitrary

-- TODO: Remove the MIN_VERSION_transformers_compat(0,7,0) by unconditionally
-- depending on transformers-0.7 or later
#if !(MIN_VERSION_transformers(0,6,0)) && !(MIN_VERSION_transformers_compat(0,7,0))
deriving instance Generic (Lift f a)
#endif
