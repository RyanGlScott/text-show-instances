{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Distribution.System
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @Distribution.System@
module of the @Cabal@ library.
-}
module Instances.Distribution.System () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif
import Data.Functor ((<$>))
import Distribution.System (Arch(..), OS(..), Platform(..))
import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary Arch where
    arbitrary = oneof [ pure I386
                      , pure X86_64
                      , pure PPC
                      , pure PPC64
                      , pure Sparc
                      , pure Arm
                      , pure Mips
                      , pure SH
                      , pure IA64
                      , pure S390
                      , pure Alpha
                      , pure Hppa
                      , pure Rs6000
                      , pure M68k
                      , pure Vax
                      , pure JavaScript
                      , OtherArch <$> arbitrary
                      ]

instance Arbitrary OS where
    arbitrary = oneof [ pure Linux
                      , pure Windows
                      , pure OSX
                      , pure FreeBSD
                      , pure OpenBSD
                      , pure NetBSD
                      , pure DragonFly
                      , pure Solaris
                      , pure AIX
                      , pure HPUX
                      , pure IRIX
                      , pure HaLVM
                      , pure IOS
                      , pure Ghcjs
                      , OtherOS <$> arbitrary
                      ]

instance Arbitrary Platform where
    arbitrary = Platform <$> arbitrary <*> arbitrary