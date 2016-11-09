{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric              #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Text.PrettyPrint
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for data types in the @pretty@ library
(as well as 'Show' instances if using an old version of @pretty@).
-}
module Instances.Text.PrettyPrint () where

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

import           Text.PrettyPrint.HughesPJ (Doc, Mode(..), Style(..),
                                            TextDetails(..), text)
#if MIN_VERSION_pretty(1,1,2)
import           Text.PrettyPrint.HughesPJClass (PrettyLevel(..))
#endif

#if MIN_VERSION_pretty(1,1,3)
import qualified Text.PrettyPrint.Annotated.HughesPJ as Annot (Doc, text)
import           Text.PrettyPrint.Annotated.HughesPJ (AnnotDetails(..), Span(..))
import qualified Text.PrettyPrint.Annotated.HughesPJClass as Annot (PrettyLevel(..))
#endif

#if !(MIN_VERSION_pretty(1,1,2)) || MIN_VERSION_pretty(1,1,3)
# if __GLASGOW_HASKELL__ >= 704
import           GHC.Generics (Generic)
# else
import qualified Generics.Deriving.TH as Generics (deriveAll0)
# endif
#endif

instance Arbitrary Doc where
    arbitrary = text <$> arbitrary

deriving instance Bounded Mode
deriving instance Enum Mode
instance Arbitrary Mode where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Style where
    arbitrary = genericArbitrary

instance Arbitrary TextDetails where
    arbitrary = genericArbitrary

#if MIN_VERSION_pretty(1,1,2)
deriving instance Arbitrary PrettyLevel
#else
deriving instance Show Mode
deriving instance Show Style
deriving instance Show TextDetails

# if __GLASGOW_HASKELL__ >= 704
deriving instance Generic Style
deriving instance Generic TextDetails
# else
$(Generics.deriveAll0 ''Style)
$(Generics.deriveAll0 ''TextDetails)
# endif
#endif

#if MIN_VERSION_pretty(1,1,3)
instance Arbitrary a => Arbitrary (AnnotDetails a) where
    arbitrary = genericArbitrary

instance Arbitrary (Annot.Doc a) where
    arbitrary = Annot.text <$> arbitrary

deriving instance Arbitrary Annot.PrettyLevel

instance Arbitrary a => Arbitrary (Span a) where
    arbitrary = genericArbitrary

# if __GLASGOW_HASKELL__ >= 704
deriving instance Generic (AnnotDetails a)
deriving instance Generic (Span a)
# else
$(Generics.deriveAll0 ''AnnotDetails)
$(Generics.deriveAll0 ''Span)
# endif
#endif
