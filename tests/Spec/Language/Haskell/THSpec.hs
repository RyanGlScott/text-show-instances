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

import Spec.Utils (prop_matchesShow, prop_genericShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text (fromString)
import Text.Show.Text.Language.Haskell.TH (showbName')

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_template_haskell(2,9,0)
    describe "AnnLookup" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> AnnLookup -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> AnnLookup -> Bool)
    describe "AnnTarget" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> AnnTarget -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> AnnTarget -> Bool)
#endif
    describe "Body" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Body -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Body -> Bool)
    describe "Callconv" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Callconv -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Callconv -> Bool)
#if MIN_VERSION_template_haskell(2,5,0) && !(MIN_VERSION_template_haskell(2,7,0))
    describe "ClassInstance" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> ClassInstance -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> ClassInstance -> Bool)
#endif
    describe "Clause" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Clause -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Clause -> Bool)
    describe "Con" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Con -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Con -> Bool)
    describe "Dec" $ do
        prop "Dec instance"                    (prop_matchesShow :: Int -> Dec -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Dec -> Bool)
    describe "Doc" $
        prop "Show instance"                   (prop_matchesShow :: Int -> Doc -> Bool)
    describe "Exp" $ do
        prop "Exp instance"                    (prop_matchesShow :: Int -> Exp -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Exp -> Bool)
    describe "FamFlavour" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> FamFlavour -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> FamFlavour -> Bool)
    describe "Fixity" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Fixity -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Fixity -> Bool)
    describe "FixityDirection" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> FixityDirection -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> FixityDirection -> Bool)
    describe "Foreign" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Foreign -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Foreign -> Bool)
    describe "FunDep" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> FunDep -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> FunDep -> Bool)
    describe "Guard" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Guard -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Guard -> Bool)
    describe "Info" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Info -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Info -> Bool)
#if MIN_VERSION_template_haskell(2,8,0)
    describe "Inline" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Inline -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Inline -> Bool)
#else
    describe "InlineSpec" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> InlineSpec -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> InlineSpec -> Bool)
#endif
    describe "Kind" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Kind -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Kind -> Bool)
    describe "Lit" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Lit -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Lit -> Bool)
    describe "Loc" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Loc -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Loc -> Bool)
    describe "Match" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Match -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Match -> Bool)
    describe "ModName" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> ModName -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> ModName -> Bool)
#if MIN_VERSION_template_haskell(2,9,0)
    describe "Module" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Module -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Module -> Bool)
    describe "ModuleInfo" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> ModuleInfo -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> ModuleInfo -> Bool)
#endif
    describe "Name" $
        prop "Show instance"                   (prop_matchesShow :: Int -> Name -> Bool)
    describe "showbName'" $ do
        prop "has the same output as showName" prop_showName'
    describe "OccName" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> OccName -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> OccName -> Bool)
    describe "Pat" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Pat -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Pat -> Bool)
#if MIN_VERSION_template_haskell(2,8,0)
    describe "Phases" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Phases -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Phases -> Bool)
#endif
    describe "PkgName" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> PkgName -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> PkgName -> Bool)
    describe "Pred" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Pred -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Pred -> Bool)
    describe "Pragma" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Pragma -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Pragma -> Bool)
    describe "Range" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Range -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Range -> Bool)
#if MIN_VERSION_template_haskell(2,9,0)
    describe "Role" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Role -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Role -> Bool)
#endif
#if MIN_VERSION_template_haskell(2,8,0)
    describe "RuleBndr" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> RuleBndr -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> RuleBndr -> Bool)
    describe "RuleMatch" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> RuleMatch -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> RuleMatch -> Bool)
#endif
    describe "Safety" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Safety -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Safety -> Bool)
    describe "Stmt" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Stmt -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Stmt -> Bool)
    describe "Strict" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Strict -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Strict -> Bool)
#if MIN_VERSION_template_haskell(2,8,0)
    describe "TyLit" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> TyLit -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> TyLit -> Bool)
#endif
    describe "Type" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> Type -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> Type -> Bool)
#if MIN_VERSION_template_haskell(2,9,0)
    describe "TySynEqn" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> TySynEqn -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> TySynEqn -> Bool)
#endif
    describe "TyVarBndr" $ do
        prop "Show instance"                   (prop_matchesShow :: Int -> TyVarBndr -> Bool)
        prop "generic Show"                    (prop_genericShow :: Int -> TyVarBndr -> Bool)

-- | Verifies that `showName'` and `showbName'` have the same output.
prop_showName' :: NameIs -> Name -> Bool
prop_showName' nameIs name = fromString (showName' nameIs name) == showbName' nameIs name
