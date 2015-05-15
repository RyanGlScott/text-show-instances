{-# LANGUAGE CPP #-}
{-|
Module:      Spec.Language.Haskell.THSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @template-haskell@ library.
-}
module Spec.Language.Haskell.THSpec (main, spec) where

import Instances.Language.Haskell.TH ()

import Language.Haskell.TH.PprLib
import Language.Haskell.TH.Syntax

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text (fromString)
import Text.Show.Text.Language.Haskell.TH (showbName')

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Language.Haskell.TH" $ do
#if MIN_VERSION_template_haskell(2,9,0)
    prop "AnnLookup instance"       (prop_matchesShow :: Int -> AnnLookup -> Bool)
    prop "AnnTarget instance"       (prop_matchesShow :: Int -> AnnTarget -> Bool)
#endif
    prop "Body instance"            (prop_matchesShow :: Int -> Body -> Bool)
    prop "Callconv instance"        (prop_matchesShow :: Int -> Callconv -> Bool)
#if MIN_VERSION_template_haskell(2,5,0) && !(MIN_VERSION_template_haskell(2,7,0))
    prop "ClassInstance instance"   (prop_matchesShow :: Int -> ClassInstance -> Bool)
#endif
    prop "Clause instance"          (prop_matchesShow :: Int -> Clause -> Bool)
    prop "Con instance"             (prop_matchesShow :: Int -> Con -> Bool)
    prop "Dec instance"             (prop_matchesShow :: Int -> Dec -> Bool)
    prop "Exp instance"             (prop_matchesShow :: Int -> Exp -> Bool)
    prop "FamFlavour instance"      (prop_matchesShow :: Int -> FamFlavour -> Bool)
    prop "Fixity instance"          (prop_matchesShow :: Int -> Fixity -> Bool)
    prop "FixityDirection instance" (prop_matchesShow :: Int -> FixityDirection -> Bool)
    prop "Foreign instance"         (prop_matchesShow :: Int -> Foreign -> Bool)
    prop "FunDep instance"          (prop_matchesShow :: Int -> FunDep -> Bool)
    prop "Guard instance"           (prop_matchesShow :: Int -> Guard -> Bool)
    prop "Info instance"            (prop_matchesShow :: Int -> Info -> Bool)
#if MIN_VERSION_template_haskell(2,8,0)
    prop "Inline instance"          (prop_matchesShow :: Int -> Inline -> Bool)
#else
    prop "InlineSpec instance"      (prop_matchesShow :: Int -> InlineSpec -> Bool)
#endif
    prop "Kind instance"            (prop_matchesShow :: Int -> Kind -> Bool)
    prop "Lit instance"             (prop_matchesShow :: Int -> Lit -> Bool)
    prop "Loc instance"             (prop_matchesShow :: Int -> Loc -> Bool)
    prop "Match instance"           (prop_matchesShow :: Int -> Match -> Bool)
    prop "ModName instance"         (prop_matchesShow :: Int -> ModName -> Bool)
#if MIN_VERSION_template_haskell(2,9,0)
    prop "Module instance"          (prop_matchesShow :: Int -> Module -> Bool)
    prop "ModuleInfo instance"      (prop_matchesShow :: Int -> ModuleInfo -> Bool)
#endif
    prop "Name instance"            (prop_matchesShow :: Int -> Name -> Bool)
    prop "showbName' output"        prop_showName'
    prop "OccName instance"         (prop_matchesShow :: Int -> OccName -> Bool)
    prop "Pat instance"             (prop_matchesShow :: Int -> Pat -> Bool)
#if MIN_VERSION_template_haskell(2,8,0)
    prop "Phases instance"          (prop_matchesShow :: Int -> Phases -> Bool)
#endif
    prop "PkgName instance"         (prop_matchesShow :: Int -> PkgName -> Bool)
    prop "Pred instance"            (prop_matchesShow :: Int -> Pred -> Bool)
    prop "Pragma instance"          (prop_matchesShow :: Int -> Pragma -> Bool)
    prop "Range instance"           (prop_matchesShow :: Int -> Range -> Bool)
#if MIN_VERSION_template_haskell(2,9,0)
    prop "Role instance"            (prop_matchesShow :: Int -> Role -> Bool)
#endif
#if MIN_VERSION_template_haskell(2,8,0)
    prop "RuleBndr instance"        (prop_matchesShow :: Int -> RuleBndr -> Bool)
    prop "RuleMatch instance"       (prop_matchesShow :: Int -> RuleMatch -> Bool)
#endif
    prop "Safety instance"          (prop_matchesShow :: Int -> Safety -> Bool)
    prop "Stmt instance"            (prop_matchesShow :: Int -> Stmt -> Bool)
    prop "Strict instance"          (prop_matchesShow :: Int -> Strict -> Bool)
#if MIN_VERSION_template_haskell(2,8,0)
    prop "TyLit instance"           (prop_matchesShow :: Int -> TyLit -> Bool)
#endif
    prop "Type instance"            (prop_matchesShow :: Int -> Type -> Bool)
#if MIN_VERSION_template_haskell(2,9,0)
    prop "TySynEqn instance"        (prop_matchesShow :: Int -> TySynEqn -> Bool)
#endif
    prop "TyVarBndr instance"       (prop_matchesShow :: Int -> TyVarBndr -> Bool)
    prop "Doc instance"             (prop_matchesShow :: Int -> Doc -> Bool)

-- | Verifies that `showName'` and `showbName'` have the same output.
prop_showName' :: NameIs -> Name -> Bool
prop_showName' nameIs name = fromString (showName' nameIs name) == showbName' nameIs name
