{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.System.Directory
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides an 'Arbitrary' instance for 'Permissions'.
-}
module Instances.System.Directory () where

#if MIN_VERSION_directory(1,1,0)
import Control.Applicative ((<**>))
# if !(MIN_VERSION_base(4,8,0))
import Data.Functor ((<$>))
# endif
import System.Directory (Permissions, emptyPermissions, setOwnerReadable,
                         setOwnerWritable, setOwnerExecutable, setOwnerSearchable)                      
import Test.Tasty.QuickCheck (Arbitrary(..))

instance Arbitrary Permissions where
    arbitrary = ($ emptyPermissions) <$> (setOwnerReadable   <$> arbitrary)
                                    <**> (setOwnerWritable   <$> arbitrary)
                                    <**> (setOwnerExecutable <$> arbitrary)
                                    <**> (setOwnerSearchable <$> arbitrary)
#endif
