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

import Spec.Utils (prop_matchesTextShow)

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
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> AnnLookup -> Bool)
    describe "AnnTarget" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> AnnTarget -> Bool)
#endif
#if MIN_VERSION_template_haskell(2,11,0)
    describe "Bang" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Bang -> Bool)
#endif
    describe "Body" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Body -> Bool)
    describe "Callconv" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Callconv -> Bool)
#if MIN_VERSION_template_haskell(2,5,0) && !(MIN_VERSION_template_haskell(2,7,0))
    describe "ClassInstance" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> ClassInstance -> Bool)
#endif
    describe "Clause" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Clause -> Bool)
    describe "Con" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Con -> Bool)
    describe "Dec" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Dec -> Bool)
#if MIN_VERSION_template_haskell(2,11,0)
    describe "DecidedStrictness" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> DecidedStrictness -> Bool)
#endif
    describe "Doc" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Doc -> Bool)
    describe "Exp" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Exp -> Bool)
    describe "FamFlavour" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> FamFlavour -> Bool)
#if MIN_VERSION_template_haskell(2,11,0)
    describe "FamilyResultSig" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> FamilyResultSig -> Bool)
#endif
    describe "Fixity" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Fixity -> Bool)
    describe "FixityDirection" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> FixityDirection -> Bool)
    describe "Foreign" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Foreign -> Bool)
    describe "FunDep" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> FunDep -> Bool)
    describe "Guard" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Guard -> Bool)
    describe "Info" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Info -> Bool)
#if MIN_VERSION_template_haskell(2,11,0)
    describe "InjectivityAnn" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> InjectivityAnn -> Bool)
#endif
#if MIN_VERSION_template_haskell(2,8,0)
    describe "Inline" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Inline -> Bool)
#else
    describe "InlineSpec" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> InlineSpec -> Bool)
#endif
    describe "Kind" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Kind -> Bool)
    describe "Lit" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Lit -> Bool)
    describe "Loc" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Loc -> Bool)
    describe "Match" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Match -> Bool)
    describe "ModName" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> ModName -> Bool)
#if MIN_VERSION_template_haskell(2,9,0)
    describe "Module" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Module -> Bool)
    describe "ModuleInfo" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> ModuleInfo -> Bool)
#endif
    describe "Name" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Name -> Bool)
    describe "showbName'" $
        prop "has the same output as showName" prop_showName'
    describe "OccName" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> OccName -> Bool)
    describe "Pat" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Pat -> Bool)
#if MIN_VERSION_template_haskell(2,8,0)
    describe "Phases" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Phases -> Bool)
#endif
    describe "PkgName" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> PkgName -> Bool)
    describe "Pred" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Pred -> Bool)
    describe "Pragma" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Pragma -> Bool)
    describe "Range" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Range -> Bool)
#if MIN_VERSION_template_haskell(2,9,0)
    describe "Role" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Role -> Bool)
#endif
#if MIN_VERSION_template_haskell(2,8,0)
    describe "RuleBndr" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> RuleBndr -> Bool)
    describe "RuleMatch" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> RuleMatch -> Bool)
#endif
    describe "Safety" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Safety -> Bool)
#if MIN_VERSION_template_haskell(2,11,0)
    describe "SourceStrictness" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> SourceStrictness -> Bool)
    describe "SourceUnpackedness" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> SourceUnpackedness -> Bool)
#endif
    describe "Stmt" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Stmt -> Bool)
    describe "Strict" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Strict -> Bool)
#if MIN_VERSION_template_haskell(2,8,0)
    describe "TyLit" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> TyLit -> Bool)
#endif
    describe "Type" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> Type -> Bool)
#if MIN_VERSION_template_haskell(2,11,0)
    describe "TypeFamilyHead" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> TypeFamilyHead -> Bool)
#endif
#if MIN_VERSION_template_haskell(2,9,0)
    describe "TySynEqn" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> TySynEqn -> Bool)
#endif
    describe "TyVarBndr" $
        prop "TextShow instance"               (prop_matchesTextShow :: Int -> TyVarBndr -> Bool)

-- | Verifies that `showName'` and `showbName'` have the same output.
prop_showName' :: NameIs -> Name -> Bool
prop_showName' nameIs name = fromString (showName' nameIs name) == showbName' nameIs name
