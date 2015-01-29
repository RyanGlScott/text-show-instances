{-|
Module:      Text.Show.Text.Distribution
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Imports 'Show' instances for @Distribution@ modules.
-}
module Text.Show.Text.Distribution () where

import Text.Show.Text.Distribution.Compiler              ()
import Text.Show.Text.Distribution.InstalledPackageInfo  ()
import Text.Show.Text.Distribution.License               ()
import Text.Show.Text.Distribution.ModuleName            ()
import Text.Show.Text.Distribution.Package               ()
import Text.Show.Text.Distribution.PackageDescription    ()
import Text.Show.Text.Distribution.ParseUtils            ()
import Text.Show.Text.Distribution.Simple.BuildTarget    ()
import Text.Show.Text.Distribution.Simple.CCompiler      ()
import Text.Show.Text.Distribution.Simple.Compiler       ()
import Text.Show.Text.Distribution.Simple.Configure      ()
import Text.Show.Text.Distribution.Simple.Hpc            ()
import Text.Show.Text.Distribution.Simple.InstallDirs    ()
import Text.Show.Text.Distribution.Simple.LocalBuildInfo ()
import Text.Show.Text.Distribution.Simple.PackageIndex   ()
import Text.Show.Text.Distribution.Simple.Program.Db     ()
import Text.Show.Text.Distribution.Simple.Program.GHC    ()
import Text.Show.Text.Distribution.Simple.Program.Types  ()
import Text.Show.Text.Distribution.Simple.Setup          ()
import Text.Show.Text.Distribution.Simple.Test.Log       ()
import Text.Show.Text.Distribution.System                ()
import Text.Show.Text.Distribution.TestSuite             ()
import Text.Show.Text.Distribution.Utils                 ()
import Text.Show.Text.Distribution.Verbosity             ()
import Text.Show.Text.Distribution.Version               ()