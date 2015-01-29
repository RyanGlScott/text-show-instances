{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.Simple.Test.Log
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.Simple.Test.Log@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.Simple.Test.Log (
      showbPackageLogPrec
    , showbTestLogsPrec
    , showbTestSuiteLogPrec
    ) where

import Distribution.Simple.Test.Log (PackageLog, TestLogs, TestSuiteLog)

import Text.Show.Text (Builder, showbPrec)
import Text.Show.Text.Distribution.Compiler  ()
import Text.Show.Text.Distribution.Package   ()
import Text.Show.Text.Distribution.System    ()
import Text.Show.Text.Distribution.TestSuite ()
import Text.Show.Text.TH (deriveShow)

-- | Convert a 'PackageLog' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbPackageLogPrec :: Int -> PackageLog -> Builder
showbPackageLogPrec = showbPrec
{-# INLINE showbPackageLogPrec #-}

-- | Convert a 'TestLogs' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbTestLogsPrec :: Int -> TestLogs -> Builder
showbTestLogsPrec = showbPrec
{-# INLINE showbTestLogsPrec #-}

-- | Convert a 'TestSuiteLog' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.2/
showbTestSuiteLogPrec :: Int -> TestSuiteLog -> Builder
showbTestSuiteLogPrec = showbPrec
{-# INLINE showbTestSuiteLogPrec #-}

$(deriveShow ''PackageLog)
$(deriveShow ''TestLogs)
$(deriveShow ''TestSuiteLog)