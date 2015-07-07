{-# LANGUAGE CPP #-}
{-|
Module:      Spec.Language.Haskell.THSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@QuickCheck@ properties for data types in the @template-haskell@ library.
-}
module Spec.Language.Haskell.THSpec (main, spec) where

import Instances.Language.Haskell.TH ()

import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.PprLib
import Language.Haskell.TH.Syntax

import Spec.Utils (prop_matchesTextShow, prop_genericTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import TextShow (fromString)
import TextShow.Language.Haskell.TH (showbName')

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_template_haskell(2,9,0)
    describe "AnnLookup" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> AnnLookup -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> AnnLookup -> Bool)
    describe "AnnTarget" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> AnnTarget -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> AnnTarget -> Bool)
#endif
    describe "Body" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Body -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Body -> Bool)
    describe "Callconv" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Callconv -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Callconv -> Bool)
#if MIN_VERSION_template_haskell(2,5,0) && !(MIN_VERSION_template_haskell(2,7,0))
    describe "ClassInstance" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> ClassInstance -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> ClassInstance -> Bool)
#endif
    describe "Clause" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Clause -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Clause -> Bool)
    describe "Con" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Con -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Con -> Bool)
    describe "Dec" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Dec -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Dec -> Bool)
    describe "Doc" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Doc -> Bool)
    describe "Exp" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Exp -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Exp -> Bool)
    describe "FamFlavour" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> FamFlavour -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> FamFlavour -> Bool)
    describe "Fixity" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Fixity -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Fixity -> Bool)
    describe "FixityDirection" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> FixityDirection -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> FixityDirection -> Bool)
    describe "Foreign" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Foreign -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Foreign -> Bool)
    describe "FunDep" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> FunDep -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> FunDep -> Bool)
    describe "Guard" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Guard -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Guard -> Bool)
    describe "Info" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Info -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Info -> Bool)
#if MIN_VERSION_template_haskell(2,8,0)
    describe "Inline" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Inline -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Inline -> Bool)
#else
    describe "InlineSpec" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> InlineSpec -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> InlineSpec -> Bool)
#endif
    describe "Kind" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Kind -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Kind -> Bool)
    describe "Lit" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Lit -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Lit -> Bool)
    describe "Loc" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Loc -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Loc -> Bool)
    describe "Match" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Match -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Match -> Bool)
    describe "ModName" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> ModName -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> ModName -> Bool)
#if MIN_VERSION_template_haskell(2,9,0)
    describe "Module" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Module -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Module -> Bool)
    describe "ModuleInfo" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> ModuleInfo -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> ModuleInfo -> Bool)
#endif
    describe "Name" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Name -> Bool)
    describe "showbName'" $ do
        prop "has the same output as showName" prop_showName'
    describe "OccName" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> OccName -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> OccName -> Bool)
    describe "Pat" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Pat -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Pat -> Bool)
#if MIN_VERSION_template_haskell(2,8,0)
    describe "Phases" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Phases -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Phases -> Bool)
#endif
    describe "PkgName" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> PkgName -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> PkgName -> Bool)
    describe "Pred" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Pred -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Pred -> Bool)
    describe "Pragma" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Pragma -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Pragma -> Bool)
    describe "Range" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Range -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Range -> Bool)
#if MIN_VERSION_template_haskell(2,9,0)
    describe "Role" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Role -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Role -> Bool)
#endif
#if MIN_VERSION_template_haskell(2,8,0)
    describe "RuleBndr" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> RuleBndr -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> RuleBndr -> Bool)
    describe "RuleMatch" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> RuleMatch -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> RuleMatch -> Bool)
#endif
    describe "Safety" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Safety -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Safety -> Bool)
    describe "Stmt" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Stmt -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Stmt -> Bool)
    describe "Strict" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Strict -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Strict -> Bool)
#if MIN_VERSION_template_haskell(2,8,0)
    describe "TyLit" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> TyLit -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> TyLit -> Bool)
#endif
    describe "Type" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Type -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> Type -> Bool)
#if MIN_VERSION_template_haskell(2,9,0)
    describe "TySynEqn" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> TySynEqn -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> TySynEqn -> Bool)
#endif
    describe "TyVarBndr" $ do
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> TyVarBndr -> Bool)
        prop "generic TextShow"                (prop_genericTextShow :: Int -> TyVarBndr -> Bool)

-- | Verifies that `showName'` and `showbName'` have the same output.
prop_showName' :: NameIs -> Name -> Bool
prop_showName' nameIs name = fromString (showName' nameIs name) == showbName' nameIs name
