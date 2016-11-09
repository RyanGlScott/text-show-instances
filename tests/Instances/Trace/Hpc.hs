{-# LANGUAGE CPP                #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds          #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Trace.Hpc
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for data types in the @hpc@ library.
-}
module Instances.Trace.Hpc () where

import qualified Generics.Deriving.TH as Generics (deriveAll0)

import           Instances.Utils ((<@>))
import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)
import           Test.QuickCheck.Instances ()

import           Trace.Hpc.Mix (Mix(..), MixEntry, BoxLabel(..), CondBox(..))
import           Trace.Hpc.Tix (Tix(..), TixModule(..))
import           Trace.Hpc.Util (HpcPos, Hash, toHpcPos)

instance Arbitrary Mix where
    arbitrary = Mix <$> arbitrary <*> arbitrary <*> arbitrary
                    <*> arbitrary <@> [fMixEntry]
--     arbitrary = Mix <$> arbitrary <*> arbitrary <*> arbitrary
--                     <*> arbitrary <*> arbitrary

instance Arbitrary BoxLabel where
    arbitrary = genericArbitrary

deriving instance Bounded CondBox
deriving instance Enum CondBox
instance Arbitrary CondBox where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Tix where
    arbitrary = genericArbitrary

instance Arbitrary TixModule where
    arbitrary = genericArbitrary

instance Arbitrary HpcPos where
    arbitrary = genericArbitrary

instance Arbitrary Hash where
    arbitrary = genericArbitrary

-------------------------------------------------------------------------------
-- Workarounds to make Arbitrary instances faster
-------------------------------------------------------------------------------

fMixEntry :: MixEntry
fMixEntry = (toHpcPos (0, 1, 2, 3), ExpBox True)

$(Generics.deriveAll0 ''BoxLabel)
$(Generics.deriveAll0 ''Tix)
$(Generics.deriveAll0 ''TixModule)
$(Generics.deriveAll0 ''HpcPos)
$(Generics.deriveAll0 ''Hash)
