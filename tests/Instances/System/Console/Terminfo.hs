{-# LANGUAGE CPP #-}

#if !defined(mingw32_HOST_OS)
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      Instances.System.Console.Terminfo
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for data types in the @terminfo@ library.
-}
module Instances.System.Console.Terminfo () where

#if !defined(mingw32_HOST_OS)
import Prelude ()
import Prelude.Compat

import System.Console.Terminfo.Color (Color(..))

import Test.QuickCheck (Arbitrary(..), oneof)

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
#endif
