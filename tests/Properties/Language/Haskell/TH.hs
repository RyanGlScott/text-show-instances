{-# LANGUAGE CPP #-}
{-|
Module:      Properties.Language.Haskell.TH
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types in the @template-haskell@ library.
-}
module Properties.Language.Haskell.TH (templateHaskellTests) where

import Instances.Language.Haskell.TH ()

import Language.Haskell.TH.PprLib
import Language.Haskell.TH.Syntax

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text (fromString)
import Text.Show.Text.Language.Haskell.TH (showbName')

-- | Verifies that `showName'` and `showbName'` have the same output.
prop_showName' :: NameIs -> Name -> Bool
prop_showName' nameIs name = fromString (showName' nameIs name) == showbName' nameIs name

templateHaskellTests :: [TestTree]
templateHaskellTests =
    [ testGroup "Text.Show.Text.Language.Haskell.TH"
        [
#if MIN_VERSION_template_haskell(2,9,0)
          testProperty "AnnLookup instance"       (prop_matchesShow :: Int -> AnnLookup -> Bool)
        , testProperty "AnnTarget instance"       (prop_matchesShow :: Int -> AnnTarget -> Bool),
#endif
          testProperty "Body instance"            (prop_matchesShow :: Int -> Body -> Bool)
        , testProperty "Callconv instance"        (prop_matchesShow :: Int -> Callconv -> Bool)
#if MIN_VERSION_template_haskell(2,5,0) && !(MIN_VERSION_template_haskell(2,7,0))
        , testProperty "ClassInstance instance"   (prop_matchesShow :: Int -> ClassInstance -> Bool)
#endif
        , testProperty "Clause instance"          (prop_matchesShow :: Int -> Clause -> Bool)
        , testProperty "Con instance"             (prop_matchesShow :: Int -> Con -> Bool)
        , testProperty "Dec instance"             (prop_matchesShow :: Int -> Dec -> Bool)
        , testProperty "Exp instance"             (prop_matchesShow :: Int -> Exp -> Bool)
        , testProperty "FamFlavour instance"      (prop_matchesShow :: Int -> FamFlavour -> Bool)
        , testProperty "Fixity instance"          (prop_matchesShow :: Int -> Fixity -> Bool)
        , testProperty "FixityDirection instance" (prop_matchesShow :: Int -> FixityDirection -> Bool)
        , testProperty "Foreign instance"         (prop_matchesShow :: Int -> Foreign -> Bool)
        , testProperty "FunDep instance"          (prop_matchesShow :: Int -> FunDep -> Bool)
        , testProperty "Guard instance"           (prop_matchesShow :: Int -> Guard -> Bool)
        , testProperty "Info instance"            (prop_matchesShow :: Int -> Info -> Bool)
#if MIN_VERSION_template_haskell(2,8,0)
        , testProperty "Inline instance"          (prop_matchesShow :: Int -> Inline -> Bool)
#else
        , testProperty "InlineSpec instance"      (prop_matchesShow :: Int -> InlineSpec -> Bool)
#endif
        , testProperty "Kind instance"            (prop_matchesShow :: Int -> Kind -> Bool)
        , testProperty "Lit instance"             (prop_matchesShow :: Int -> Lit -> Bool)
        , testProperty "Loc instance"             (prop_matchesShow :: Int -> Loc -> Bool)
        , testProperty "Match instance"           (prop_matchesShow :: Int -> Match -> Bool)
        , testProperty "ModName instance"         (prop_matchesShow :: Int -> ModName -> Bool)
#if MIN_VERSION_template_haskell(2,9,0)
        , testProperty "Module instance"          (prop_matchesShow :: Int -> Module -> Bool)
        , testProperty "ModuleInfo instance"      (prop_matchesShow :: Int -> ModuleInfo -> Bool)
#endif
        , testProperty "Name instance"            (prop_matchesShow :: Int -> Name -> Bool)
        , testProperty "showbName' output"        prop_showName'
        , testProperty "OccName instance"         (prop_matchesShow :: Int -> OccName -> Bool)
        , testProperty "Pat instance"             (prop_matchesShow :: Int -> Pat -> Bool)
#if MIN_VERSION_template_haskell(2,8,0)
        , testProperty "Phases instance"          (prop_matchesShow :: Int -> Phases -> Bool)
#endif
        , testProperty "PkgName instance"         (prop_matchesShow :: Int -> PkgName -> Bool)
        , testProperty "Pred instance"            (prop_matchesShow :: Int -> Pred -> Bool)
        , testProperty "Pragma instance"          (prop_matchesShow :: Int -> Pragma -> Bool)
        , testProperty "Range instance"           (prop_matchesShow :: Int -> Range -> Bool)
#if MIN_VERSION_template_haskell(2,9,0)
        , testProperty "Role instance"            (prop_matchesShow :: Int -> Role -> Bool)
#endif
#if MIN_VERSION_template_haskell(2,8,0)
        , testProperty "RuleBndr instance"        (prop_matchesShow :: Int -> RuleBndr -> Bool)
        , testProperty "RuleMatch instance"       (prop_matchesShow :: Int -> RuleMatch -> Bool)
#endif
        , testProperty "Safety instance"          (prop_matchesShow :: Int -> Safety -> Bool)
        , testProperty "Stmt instance"            (prop_matchesShow :: Int -> Stmt -> Bool)
        , testProperty "Strict instance"          (prop_matchesShow :: Int -> Strict -> Bool)
#if MIN_VERSION_template_haskell(2,8,0)
        , testProperty "TyLit instance"           (prop_matchesShow :: Int -> TyLit -> Bool)
#endif
        , testProperty "Type instance"            (prop_matchesShow :: Int -> Type -> Bool)
#if MIN_VERSION_template_haskell(2,9,0)
        , testProperty "TySynEqn instance"        (prop_matchesShow :: Int -> TySynEqn -> Bool)
#endif
        , testProperty "TyVarBndr instance"       (prop_matchesShow :: Int -> TyVarBndr -> Bool)
        , testProperty "Doc instance"             (prop_matchesShow :: Int -> Doc -> Bool)
        ]
    ]