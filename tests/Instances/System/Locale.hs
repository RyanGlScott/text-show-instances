{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances.System.Locale () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>))
#endif
import Data.Functor ((<$>))
import System.Locale (TimeLocale(..))
import Test.Tasty.QuickCheck (Arbitrary(..))

instance Arbitrary TimeLocale where
    arbitrary = TimeLocale <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary