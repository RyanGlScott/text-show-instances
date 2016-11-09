{-# LANGUAGE CPP                #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric      #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.System.Locale
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides an 'Arbitrary' instance for old 'TimeLocale' values.
-}
module Instances.System.Locale () where

#if __GLASGOW_HASKELL__ >= 704
import           GHC.Generics (Generic)
#else
import qualified Generics.Deriving.TH as Generics (deriveAll0)
#endif

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           System.Locale (TimeLocale(..))

import           Test.QuickCheck (Arbitrary(..))

instance Arbitrary TimeLocale where
    arbitrary = genericArbitrary

#if __GLASGOW_HASKELL__ >= 704
deriving instance Generic TimeLocale
#else
$(Generics.deriveAll0 ''TimeLocale)
#endif
