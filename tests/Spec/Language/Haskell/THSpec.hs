{-# LANGUAGE CPP #-}
{-|
Module:      Spec.Language.Haskell.THSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@QuickCheck@ properties for data types in the @template-haskell@ library.
-}
module Spec.Language.Haskell.THSpec (main, spec) where

import Data.Proxy (Proxy(..))

import Instances.Language.Haskell.TH ()

import Language.Haskell.TH.PprLib
import Language.Haskell.TH.Syntax

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import TextShow (fromString)
import TextShow.Language.Haskell.TH (showbName')

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_template_haskell(2,9,0)
    describe "AnnLookup" $
        matchesTextShowSpec (Proxy :: Proxy AnnLookup)
    describe "AnnTarget" $
        matchesTextShowSpec (Proxy :: Proxy AnnTarget)
#endif
#if MIN_VERSION_template_haskell(2,11,0)
    describe "Bang" $
        matchesTextShowSpec (Proxy :: Proxy Bang)
#endif
    describe "Body" $
        matchesTextShowSpec (Proxy :: Proxy Body)
    describe "Callconv" $
        matchesTextShowSpec (Proxy :: Proxy Callconv)
#if MIN_VERSION_template_haskell(2,5,0) && !(MIN_VERSION_template_haskell(2,7,0))
    describe "ClassInstance" $
        matchesTextShowSpec (Proxy :: Proxy ClassInstance)
#endif
    describe "Clause" $
        matchesTextShowSpec (Proxy :: Proxy Clause)
    describe "Con" $
        matchesTextShowSpec (Proxy :: Proxy Con)
    describe "Dec" $
        matchesTextShowSpec (Proxy :: Proxy Dec)
#if MIN_VERSION_template_haskell(2,11,0)
    describe "DecidedStrictness" $
        matchesTextShowSpec (Proxy :: Proxy DecidedStrictness)
#endif
    describe "Doc" $
        matchesTextShowSpec (Proxy :: Proxy Doc)
    describe "Exp" $
        matchesTextShowSpec (Proxy :: Proxy Exp)
    describe "FamFlavour" $
        matchesTextShowSpec (Proxy :: Proxy FamFlavour)
#if MIN_VERSION_template_haskell(2,11,0)
    describe "FamilyResultSig" $
        matchesTextShowSpec (Proxy :: Proxy FamilyResultSig)
#endif
    describe "Fixity" $
        matchesTextShowSpec (Proxy :: Proxy Fixity)
    describe "FixityDirection" $
        matchesTextShowSpec (Proxy :: Proxy FixityDirection)
    describe "Foreign" $
        matchesTextShowSpec (Proxy :: Proxy Foreign)
    describe "FunDep" $
        matchesTextShowSpec (Proxy :: Proxy FunDep)
    describe "Guard" $
        matchesTextShowSpec (Proxy :: Proxy Guard)
    describe "Info" $
        matchesTextShowSpec (Proxy :: Proxy Info)
#if MIN_VERSION_template_haskell(2,11,0)
    describe "InjectivityAnn" $
        matchesTextShowSpec (Proxy :: Proxy InjectivityAnn)
#endif
#if MIN_VERSION_template_haskell(2,8,0)
    describe "Inline" $
        matchesTextShowSpec (Proxy :: Proxy Inline)
#else
    describe "InlineSpec" $
        matchesTextShowSpec (Proxy :: Proxy InlineSpec)
#endif
    describe "Kind" $
        matchesTextShowSpec (Proxy :: Proxy Kind)
    describe "Lit" $
        matchesTextShowSpec (Proxy :: Proxy Lit)
    describe "Loc" $
        matchesTextShowSpec (Proxy :: Proxy Loc)
    describe "Match" $
        matchesTextShowSpec (Proxy :: Proxy Match)
    describe "ModName" $
        matchesTextShowSpec (Proxy :: Proxy ModName)
#if MIN_VERSION_template_haskell(2,9,0)
    describe "Module" $
        matchesTextShowSpec (Proxy :: Proxy Module)
    describe "ModuleInfo" $
        matchesTextShowSpec (Proxy :: Proxy ModuleInfo)
#endif
    describe "Name" $
        matchesTextShowSpec (Proxy :: Proxy Name)
    describe "showbName'" $
        prop "has the same output as showName" prop_showName'
    describe "NameFlavour" $
        matchesTextShowSpec (Proxy :: Proxy NameFlavour)
    describe "NameSpace" $
        matchesTextShowSpec (Proxy :: Proxy NameSpace)
    describe "OccName" $
        matchesTextShowSpec (Proxy :: Proxy OccName)
#if MIN_VERSION_template_haskell(2,11,0)
    describe "Overlap" $
        matchesTextShowSpec (Proxy :: Proxy Overlap)
#endif
    describe "Pat" $
        matchesTextShowSpec (Proxy :: Proxy Pat)
#if __GLASGOW_HASKELL__ >= 801
    describe "PatSynArgs" $
        matchesTextShowSpec (Proxy :: Proxy PatSynArgs)
    describe "PatSynDir" $
        matchesTextShowSpec (Proxy :: Proxy PatSynDir)
#endif
#if MIN_VERSION_template_haskell(2,8,0)
    describe "Phases" $
        matchesTextShowSpec (Proxy :: Proxy Phases)
#endif
    describe "PkgName" $
        matchesTextShowSpec (Proxy :: Proxy PkgName)
    describe "Pred" $
        matchesTextShowSpec (Proxy :: Proxy Pred)
    describe "Pragma" $
        matchesTextShowSpec (Proxy :: Proxy Pragma)
    describe "Range" $
        matchesTextShowSpec (Proxy :: Proxy Range)
#if MIN_VERSION_template_haskell(2,9,0)
    describe "Role" $
        matchesTextShowSpec (Proxy :: Proxy Role)
#endif
#if MIN_VERSION_template_haskell(2,8,0)
    describe "RuleBndr" $
        matchesTextShowSpec (Proxy :: Proxy RuleBndr)
    describe "RuleMatch" $
        matchesTextShowSpec (Proxy :: Proxy RuleMatch)
#endif
    describe "Safety" $
        matchesTextShowSpec (Proxy :: Proxy Safety)
#if MIN_VERSION_template_haskell(2,11,0)
    describe "SourceStrictness" $
        matchesTextShowSpec (Proxy :: Proxy SourceStrictness)
    describe "SourceUnpackedness" $
        matchesTextShowSpec (Proxy :: Proxy SourceUnpackedness)
#endif
    describe "Stmt" $
        matchesTextShowSpec (Proxy :: Proxy Stmt)
    describe "Strict" $
        matchesTextShowSpec (Proxy :: Proxy Strict)
#if MIN_VERSION_template_haskell(2,8,0)
    describe "TyLit" $
        matchesTextShowSpec (Proxy :: Proxy TyLit)
#endif
    describe "Type" $
        matchesTextShowSpec (Proxy :: Proxy Type)
#if MIN_VERSION_template_haskell(2,11,0)
    describe "TypeFamilyHead" $
        matchesTextShowSpec (Proxy :: Proxy TypeFamilyHead)
#endif
#if MIN_VERSION_template_haskell(2,9,0)
    describe "TySynEqn" $
        matchesTextShowSpec (Proxy :: Proxy TySynEqn)
#endif
    describe "TyVarBndr" $
        matchesTextShowSpec (Proxy :: Proxy TyVarBndr)

-- | Verifies that `showName'` and `showbName'` have the same output.
prop_showName' :: NameIs -> Name -> Bool
prop_showName' nameIs name = fromString (showName' nameIs name) == showbName' nameIs name
