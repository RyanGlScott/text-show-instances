{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds       #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.System.Directory
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides an 'Arbitrary' instance for 'Permissions'.
-}
module Instances.System.Directory () where

#if MIN_VERSION_directory(1,1,0)
import qualified Generics.Deriving.TH as Generics (deriveAll0)
import           Instances.Utils.GenericArbitrary (genericArbitrary)
import           System.Directory (Permissions)
import           Test.QuickCheck (Arbitrary(..))

# if MIN_VERSION_directory(1,2,3)
import           System.Directory (XdgDirectory(..))
import           Test.QuickCheck (arbitraryBoundedEnum)
# endif

instance Arbitrary Permissions where
    arbitrary = genericArbitrary
$(Generics.deriveAll0 ''Permissions)

# if MIN_VERSION_directory(1,2,3)
instance Arbitrary XdgDirectory where
    arbitrary = arbitraryBoundedEnum
# endif
#endif
