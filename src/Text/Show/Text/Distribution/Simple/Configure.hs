{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Distribution.Simple.Configure
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Distribution.Simple.Configure@
module of the @Cabal@ library.

/Since: 0.2/
-}
module Text.Show.Text.Distribution.Simple.Configure (showbConfigStateFileError) where

import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.Simple.Configure (ConfigStateFileError(..))
import Distribution.Simple.Utils (cabalVersion)

import Prelude hiding (Show)

import System.Info (compilerName, compilerVersion)

import Text.Show.Text (Show(showb), Builder)
import Text.Show.Text.Distribution.Text (displayB)
import Text.Show.Text.Utils ((<>))

-- | Convert a 'ConfigStateFileError' to a 'Builder'.
-- 
-- /Since: 0.2/
showbConfigStateFileError :: ConfigStateFileError -> Builder
showbConfigStateFileError ConfigStateFileNoHeader =
    "Saved package config file header is missing. "
    <> "Try re-running the 'configure' command."
showbConfigStateFileError ConfigStateFileBadHeader =
    "Saved package config file header is corrupt. "
    <> "Try re-running the 'configure' command."
showbConfigStateFileError ConfigStateFileNoParse =
    "Saved package config file body is corrupt. "
    <> "Try re-running the 'configure' command."
showbConfigStateFileError ConfigStateFileMissing = "Run the 'configure' command first."
showbConfigStateFileError (ConfigStateFileBadVersion oldCabal oldCompiler _) =
    "You need to re-run the 'configure' command. "
    <> "The version of Cabal being used has changed (was "
    <> displayB oldCabal <> ", now "
    <> displayB currentCabalId <> ")."
    <> badCompiler
  where
    badCompiler
      | oldCompiler == currentCompilerId = ""
      | otherwise =
          " Additionally the compiler is different (was "
          <> displayB oldCompiler <> ", now "
          <> displayB currentCompilerId
          <> ") which is probably the cause of the problem."

currentCabalId :: PackageIdentifier
currentCabalId = PackageIdentifier (PackageName "Cabal") cabalVersion
{-# INLINE currentCabalId #-}

currentCompilerId :: PackageIdentifier
currentCompilerId = PackageIdentifier (PackageName compilerName) compilerVersion
{-# INLINE currentCompilerId #-}

instance Show ConfigStateFileError where
    showb = showbConfigStateFileError