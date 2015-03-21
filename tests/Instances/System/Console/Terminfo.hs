{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.System.Console.Terminfo
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @terminfo@ library.
-}
module Instances.System.Console.Terminfo () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative (pure)
import Data.Functor ((<$>))
#endif

import System.Console.Terminfo.Color (Color(..))
import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary Color where
    arbitrary = oneof [ pure Black
                      , pure Red
                      , pure Green
                      , pure Yellow
                      , pure Blue
                      , pure Magenta
                      , pure Cyan
                      , pure White
                      , ColorNumber <$> arbitrary
                      ]

-- instance Arbitrary SetupTermError
