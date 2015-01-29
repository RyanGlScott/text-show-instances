{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.Version
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Distribution.Version@
module of the @Cabal@ library.
-}
module Instances.Distribution.Version () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif
import Data.Functor ((<$>))
import Distribution.Version
    ( Bound(..), LowerBound(..), UpperBound(..),
      VersionIntervals, toVersionIntervals,
#if MIN_VERSION_Cabal(1,10,0)
      VersionRange(VersionRangeParens),
#else
      VersionRange,
#endif
      anyVersion, thisVersion, laterVersion, earlierVersion,
      withinVersion, unionVersionRanges, intersectVersionRanges
    )
import Instances.Miscellaneous ()
import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary Bound where
    arbitrary = oneof $ map pure [ExclusiveBound, InclusiveBound]

instance Arbitrary LowerBound where
    arbitrary = LowerBound <$> arbitrary <*> arbitrary

instance Arbitrary UpperBound where
    arbitrary = oneof [pure NoUpperBound, UpperBound <$> arbitrary <*> arbitrary]

instance Arbitrary VersionIntervals where
    arbitrary = toVersionIntervals <$> arbitrary

instance Arbitrary VersionRange where
    arbitrary = oneof [ pure anyVersion
                      , thisVersion    <$> arbitrary
                      , laterVersion   <$> arbitrary
                      , earlierVersion <$> arbitrary
                      , withinVersion  <$> arbitrary
                      , pure $ unionVersionRanges     fVersionRange fVersionRange
                      , pure $ intersectVersionRanges fVersionRange fVersionRange
#if MIN_VERSION_Cabal(1,10,0)
                      , pure $ VersionRangeParens     fVersionRange
#endif
                      ]
--     arbitrary = oneof [ pure anyVersion
--                       , thisVersion            <$> arbitrary
--                       , laterVersion           <$> arbitrary
--                       , earlierVersion         <$> arbitrary
--                       , withinVersion          <$> arbitrary
--                       , unionVersionRanges     <$> arbitrary <*> arbitrary
--                       , intersectVersionRanges <$> arbitrary <*> arbitrary
-- #if MIN_VERSION_Cabal(1,10,0)
--                       , VersionRangeParens     <$> arbitrary
-- #endif
--                       ]

-------------------------------------------------------------------------------
-- Workarounds to make Arbitrary instances faster
-------------------------------------------------------------------------------

fVersionRange :: VersionRange
fVersionRange = anyVersion