{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric              #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Control.Applicative.Trans
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for applicative functor transformers.
-}
module Instances.Control.Applicative.Trans () where

import           Control.Applicative.Backwards (Backwards(..))
import           Control.Applicative.Lift      (Lift(..))

#if __GLASGOW_HASKELL__ >= 706
import           GHC.Generics (Generic)
#else
import qualified Generics.Deriving.TH as Generics (deriveAll0)
#endif

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Test.QuickCheck (Arbitrary(..))

deriving instance Arbitrary (f a) => Arbitrary (Backwards f a)

instance (Arbitrary a, Arbitrary (f a)) => Arbitrary (Lift f a) where
    arbitrary = genericArbitrary

#if __GLASGOW_HASKELL__ >= 706
deriving instance Generic (Lift f a)
#else
$(Generics.deriveAll0 ''Lift)
#endif
