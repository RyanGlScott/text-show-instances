{-# LANGUAGE CPP                #-}

#if !defined(mingw32_HOST_OS)
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

# if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric      #-}
# endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      Instances.System.Posix
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for data types in the @unix@ library.
-}
module Instances.System.Posix () where

#if !defined(mingw32_HOST_OS)
# if __GLASGOW_HASKELL__ >= 704
import           GHC.Generics (Generic)
# else
import qualified Generics.Deriving.TH as Generics (deriveAll0)
# endif

import           Instances.Miscellaneous ()
import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Prelude ()
import           Prelude.Compat

import           System.Posix.DynamicLinker (RTLDFlags(..), DL(..))
import           System.Posix.Process (ProcessStatus(..))
import           System.Posix.User (GroupEntry(..), UserEntry(..))

import           Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

deriving instance Bounded RTLDFlags
deriving instance Enum RTLDFlags
instance Arbitrary RTLDFlags where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary DL where
    arbitrary = genericArbitrary

instance Arbitrary ProcessStatus where
    arbitrary = genericArbitrary

instance Arbitrary GroupEntry where
    arbitrary = genericArbitrary

instance Arbitrary UserEntry where
    arbitrary = genericArbitrary

# if __GLASGOW_HASKELL__ >= 704
deriving instance Generic DL
deriving instance Generic ProcessStatus
deriving instance Generic GroupEntry
deriving instance Generic UserEntry
# else
$(Generics.deriveAll0 ''DL)
$(Generics.deriveAll0 ''ProcessStatus)
$(Generics.deriveAll0 ''GroupEntry)
$(Generics.deriveAll0 ''UserEntry)
# endif
#endif
