{-# LANGUAGE CPP, TemplateHaskell #-}
#if !(MIN_VERSION_template_haskell(2,10,0))
{-# LANGUAGE MagicHash #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Language.Haskell.TH
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @template-haskell@ library.

/Since: 0.1/
-}
module Text.Show.Text.Language.Haskell.TH (
#if MIN_VERSION_template_haskell(2,9,0)
      showbAnnLookupPrec
    , showbAnnTargetPrec,
#endif
      showbBodyPrec
    , showbCallconv
#if MIN_VERSION_template_haskell(2,5,0) && !(MIN_VERSION_template_haskell(2,7,0))
    , showbClassInstancePrec
#endif
    , showbClausePrec
    , showbConPrec
    , showbDecPrec
    , showbExpPrec
    , showbFamFlavour
    , showbFixityPrec
    , showbFixityDirection
    , showbForeignPrec
    , showbFunDepPrec
    , showbGuardPrec
    , showbInfoPrec
#if MIN_VERSION_template_haskell(2,8,0)
    , showbInline
#else
    , showbInlineSpecPrec
#endif
#if !(MIN_VERSION_template_haskell(2,8,0))
    , showbKindPrec
#endif
    , showbLitPrec
    , showbLocPrec
    , showbMatchPrec
    , showbModNamePrec
#if MIN_VERSION_template_haskell(2,9,0)
    , showbModulePrec
    , showbModuleInfoPrec
#endif
    , showbName
    , showbName'
    , showbOccNamePrec
    , showbPatPrec
#if MIN_VERSION_template_haskell(2,8,0)
    , showbPhasesPrec
#endif
    , showbPkgNamePrec
    , showbPragmaPrec
#if !(MIN_VERSION_template_haskell(2,10,0))
    , showbPredPrec
#endif
    , showbRangePrec
#if MIN_VERSION_template_haskell(2,9,0)
    , showbRole
#endif
#if MIN_VERSION_template_haskell(2,8,0)
    , showbRuleBndrPrec
    , showbRuleMatch
#endif
    , showbSafety
    , showbStmtPrec
    , showbStrict
#if MIN_VERSION_template_haskell(2,8,0)
    , showbTyLitPrec
#endif
    , showbTypePrec
    , showbTyVarBndrPrec
#if MIN_VERSION_template_haskell(2,9,0)
    , showbTySynEqnPrec
#endif
    ) where

import           Data.Char (isAlpha)
import           Data.Maybe (fromJust)
import qualified Data.Text.Lazy as TL (Text, dropWhile, null, tail)
import           Data.Text.Lazy (uncons)

#if !(MIN_VERSION_template_haskell(2,10,0))
import           GHC.Exts (Int(I#))
#endif

import           Language.Haskell.TH.Syntax

import           Prelude hiding (Show)

import           Text.Show.Text (Show(showb, showbPrec), Builder,
                                 fromString, toLazyText)
import           Text.Show.Text.Data.Integral (showbIntPrec)
import           Text.Show.Text.TH (deriveShowPragmas, defaultInlineShowb,
                                    defaultInlineShowbPrec)
import           Text.Show.Text.Utils ((<>), s)

#include "inline.h"

-- | Convert a 'Body' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbBodyPrec :: Int -> Body -> Builder
showbBodyPrec = showbPrec
{-# INLINE showbBodyPrec #-}

-- | Convert a 'Callconv' to a 'Builder'.
-- 
-- /Since: 0.1/
showbCallconv :: Callconv -> Builder
showbCallconv = showb
{-# INLINE showbCallconv #-}

-- | Convert a 'Clause' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbClausePrec :: Int -> Clause -> Builder
showbClausePrec = showbPrec
{-# INLINE showbClausePrec #-}

-- | Convert a 'Con' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbConPrec :: Int -> Con -> Builder
showbConPrec = showbPrec
{-# INLINE showbConPrec #-}

-- | Convert a 'Dec' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbDecPrec :: Int -> Dec -> Builder
showbDecPrec = showbPrec
{-# INLINE showbDecPrec #-}

-- | Convert an 'Exp' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbExpPrec :: Int -> Exp -> Builder
showbExpPrec = showbPrec
{-# INLINE showbExpPrec #-}

-- | Convert a 'FamFlavour' to a 'Builder'.
-- 
-- /Since: 0.1/
showbFamFlavour :: FamFlavour -> Builder
showbFamFlavour = showb
{-# INLINE showbFamFlavour #-}

-- | Convert a 'Fixity' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbFixityPrec :: Int -> Fixity -> Builder
showbFixityPrec = showbPrec
{-# INLINE showbFixityPrec #-}

-- | Convert a 'FixityDirection' to a 'Builder'.
-- 
-- /Since: 0.1/
showbFixityDirection :: FixityDirection -> Builder
showbFixityDirection = showb
{-# INLINE showbFixityDirection #-}

-- | Convert a 'Foreign' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbForeignPrec :: Int -> Foreign -> Builder
showbForeignPrec = showbPrec
{-# INLINE showbForeignPrec #-}

-- | Convert a 'FunDep' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbFunDepPrec :: Int -> FunDep -> Builder
showbFunDepPrec = showbPrec
{-# INLINE showbFunDepPrec #-}

-- | Convert a 'Guard' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbGuardPrec :: Int -> Guard -> Builder
showbGuardPrec = showbPrec
{-# INLINE showbGuardPrec #-}

-- | Convert an 'Info' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbInfoPrec :: Int -> Info -> Builder
showbInfoPrec = showbPrec
{-# INLINE showbInfoPrec #-}

-- | Convert a 'Lit' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbLitPrec :: Int -> Dec -> Builder
showbLitPrec = showbPrec
{-# INLINE showbLitPrec #-}

-- | Convert a 'Loc' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbLocPrec :: Int -> Loc -> Builder
showbLocPrec = showbPrec
{-# INLINE showbLocPrec #-}

-- | Convert a 'Match' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbMatchPrec :: Int -> Match -> Builder
showbMatchPrec = showbPrec
{-# INLINE showbMatchPrec #-}

-- | Convert a 'ModName' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbModNamePrec :: Int -> ModName -> Builder
showbModNamePrec = showbPrec
{-# INLINE showbModNamePrec #-}

-- | Convert a 'Name' to a 'Builder'.
-- 
-- /Since: 0.1/
showbName :: Name -> Builder
showbName = showbName' Alone
{-# INLINE showbName #-}

-- | Convert a 'Name' to a 'Builder' with the given 'NameIs' settings.
-- 
-- /Since: 0.1/
showbName' :: NameIs -> Name -> Builder
showbName' ni nm = case ni of
    Alone           -> nms
    Applied
        | pnam      -> nms
        | otherwise -> s '(' <> nms <> s ')'
    Infix
        | pnam      -> s '`' <> nms <> s '`'
        | otherwise -> nms
  where
    -- For now, we make the NameQ and NameG print the same, even though
    -- NameQ is a qualified name (so what it means depends on what the
    -- current scope is), and NameG is an original name (so its meaning
    -- should be independent of what's in scope.
    -- We may well want to distinguish them in the end.
    -- Ditto NameU and NameL
    nms :: Builder
    nms = case nm of
               Name occ NameS         -> occB occ
               Name occ (NameQ m)     -> modB m   <> s '.' <> occB occ
               Name occ (NameG _ _ m) -> modB m   <> s '.' <> occB occ
               Name occ (NameU u)     -> occB occ <> s '_' <> showbIntPrec 0 (mkInt u)
               Name occ (NameL u)     -> occB occ <> s '_' <> showbIntPrec 0 (mkInt u)
    
#if MIN_VERSION_template_haskell(2,10,0)
    mkInt = id
#else
    mkInt i# = I# i#
#endif
    
    occB :: OccName -> Builder
    occB = fromString . occString
    
    modB :: ModName -> Builder
    modB = fromString . modString
    
    pnam :: Bool
    pnam = classify $ toLazyText nms
    
    -- True if we are function style, e.g. f, [], (,)
    -- False if we are operator style, e.g. +, :+
    classify :: TL.Text -> Bool
    classify t
        | TL.null t  = False -- shouldn't happen; . operator is handled below
        | otherwise = case fromJust $ uncons t of
              (x, xs) -> if isAlpha x || (x `elem` "_[]()")
                            then let t' = TL.dropWhile (/= '.') xs
                                 in if TL.null t'
                                       then True
                                       else classify $ TL.tail t'
                            else False
                                     

-- | Convert an 'OccName' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbOccNamePrec :: Int -> OccName -> Builder
showbOccNamePrec = showbPrec
{-# INLINE showbOccNamePrec #-}

-- | Convert a 'Pat' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbPatPrec :: Int -> Pat -> Builder
showbPatPrec = showbPrec
{-# INLINE showbPatPrec #-}

-- | Convert a 'PkgName' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbPkgNamePrec :: Int -> PkgName -> Builder
showbPkgNamePrec = showbPrec
{-# INLINE showbPkgNamePrec #-}

-- | Convert a 'Pragma' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbPragmaPrec :: Int -> Pragma -> Builder
showbPragmaPrec = showbPrec
{-# INLINE showbPragmaPrec #-}

-- | Convert a 'Range' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbRangePrec :: Int -> Range -> Builder
showbRangePrec = showbPrec
{-# INLINE showbRangePrec #-}

-- | Convert a 'Safety' to a 'Builder'.
-- 
-- /Since: 0.1/
showbSafety :: Safety -> Builder
showbSafety = showb
{-# INLINE showbSafety #-}

-- | Convert a 'Stmt' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbStmtPrec :: Int -> Stmt -> Builder
showbStmtPrec = showbPrec
{-# INLINE showbStmtPrec #-}

-- | Convert a 'Strict' to a 'Builder'.
-- 
-- /Since: 0.1/
showbStrict :: Strict -> Builder
showbStrict = showb
{-# INLINE showbStrict #-}

-- | Convert a 'Type' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbTypePrec :: Int -> Type -> Builder
showbTypePrec = showbPrec
{-# INLINE showbTypePrec #-}

-- | Convert a 'TyVarBndr' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbTyVarBndrPrec :: Int -> TyVarBndr -> Builder
showbTyVarBndrPrec = showbPrec
{-# INLINE showbTyVarBndrPrec #-}

#if MIN_VERSION_template_haskell(2,5,0) && !(MIN_VERSION_template_haskell(2,7,0))
-- | Convert a 'ClassInstance' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.1/
showbClassInstancePrec :: Int -> ClassInstance -> Builder
showbClassInstancePrec = showbPrec
{-# INLINE showbClassInstancePrec #-}
#endif

#if MIN_VERSION_template_haskell(2,8,0)
-- | Convert an 'Inline' to a 'Builder'.
-- This function is only available with @template-haskell-2.8.0.0@ or later.
-- 
-- /Since: 0.1/
showbInline :: Inline -> Builder
showbInline = showb
{-# INLINE showbInline #-}

-- | Convert a 'Phases' to a 'Builder' with the given precedence.
-- This function is only available with @template-haskell-2.8.0.0@ or later.
-- 
-- /Since: 0.1/
showbPhasesPrec :: Int -> Phases -> Builder
showbPhasesPrec = showbPrec
{-# INLINE showbPhasesPrec #-}

-- | Convert a 'RuleMatch' to a 'Builder'.
-- This function is only available with @template-haskell-2.8.0.0@ or later.
-- 
-- /Since: 0.1/
showbRuleMatch :: RuleMatch -> Builder
showbRuleMatch = showb
{-# INLINE showbRuleMatch #-}

-- | Convert a 'RuleBndr' to a 'Builder' with the given precedence.
-- This function is only available with @template-haskell-2.8.0.0@ or later.
-- 
-- /Since: 0.1/
showbRuleBndrPrec :: Int -> RuleBndr -> Builder
showbRuleBndrPrec = showbPrec
{-# INLINE showbRuleBndrPrec #-}

-- | Convert a 'TyLit' to a 'Builder' with the given precedence.
-- This function is only available with @template-haskell-2.8.0.0@ or later.
-- 
-- /Since: 0.1/
showbTyLitPrec :: Int -> TyLit -> Builder
showbTyLitPrec = showbPrec
{-# INLINE showbTyLitPrec #-}
#else
-- | Convert an 'InlineSpec' to a 'Builder' with the given precedence.
-- This function is only available with @template-haskell-2.7.0.0@ or earlier.
-- 
-- /Since: 0.1/
showbInlineSpecPrec :: Int -> InlineSpec -> Builder
showbInlineSpecPrec = showbPrec
{-# INLINE showbInlineSpecPrec #-}

-- | Convert a 'Kind' to a 'Builder' with the given precedence.
-- This function is only available with @template-haskell-2.7.0.0@ or earlier, as
-- 'Kind' is a type synonym for 'Type' in @template-haskell-2.8.0.0@ or later.
-- 
-- /Since: 0.1/
showbKindPrec :: Int -> Kind -> Builder
showbKindPrec = showbPrec
{-# INLINE showbKindPrec #-}
#endif

#if MIN_VERSION_template_haskell(2,9,0)
-- | Convert an 'AnnLookup' to a 'Builder' with the given precedence.
-- This function is only available with @template-haskell-2.9.0.0@ or later.
-- 
-- /Since: 0.1/
showbAnnLookupPrec :: Int -> AnnLookup -> Builder
showbAnnLookupPrec = showbPrec
{-# INLINE showbAnnLookupPrec #-}

-- | Convert an 'AnnTarget' to a 'Builder' with the given precedence.
-- This function is only available with @template-haskell-2.9.0.0@ or later.
-- 
-- /Since: 0.1/
showbAnnTargetPrec :: Int -> AnnTarget -> Builder
showbAnnTargetPrec = showbPrec
{-# INLINE showbAnnTargetPrec #-}

-- | Convert a 'Module' to a 'Builder' with the given precedence.
-- This function is only available with @template-haskell-2.9.0.0@ or later.
-- 
-- /Since: 0.1/
showbModulePrec :: Int -> Module -> Builder
showbModulePrec = showbPrec
{-# INLINE showbModulePrec #-}

-- | Convert a 'ModuleInfo' to a 'Builder' with the given precedence.
-- This function is only available with @template-haskell-2.9.0.0@ or later.
-- 
-- /Since: 0.1/
showbModuleInfoPrec :: Int -> ModuleInfo -> Builder
showbModuleInfoPrec = showbPrec
{-# INLINE showbModuleInfoPrec #-}

-- | Convert a 'Role' to a 'Builder'.
-- This function is only available with @template-haskell-2.9.0.0@ or later.
-- 
-- /Since: 0.1/
showbRole :: Role -> Builder
showbRole = showb
{-# INLINE showbRole #-}

-- | Convert a 'TySynEqn' to a 'Builder' with the given precedence.
-- This function is only available with @template-haskell-2.9.0.0@ or later.
-- 
-- /Since: 0.1/
showbTySynEqnPrec :: Int -> TySynEqn -> Builder
showbTySynEqnPrec = showbPrec
{-# INLINE showbTySynEqnPrec #-}
#endif

#if !(MIN_VERSION_template_haskell(2,10,0))
-- | Convert a 'Pred' to a 'Builder' with the given precedence.
-- This function is only available with @template-haskell-2.9.0.0@ or earlier, as
-- 'Pred' is a type synonym for 'Type' in @template-haskell-2.10.0.0@ or later.
-- 
-- /Since: 0.1/
showbPredPrec :: Int -> Pred -> Builder
showbPredPrec = showbPrec
{-# INLINE showbPredPrec #-}
#endif

$(deriveShowPragmas defaultInlineShowbPrec ''Body)
$(deriveShowPragmas defaultInlineShowb     ''Callconv)
$(deriveShowPragmas defaultInlineShowbPrec ''Clause)
$(deriveShowPragmas defaultInlineShowbPrec ''Con)
$(deriveShowPragmas defaultInlineShowbPrec ''Dec)
$(deriveShowPragmas defaultInlineShowbPrec ''Exp)
$(deriveShowPragmas defaultInlineShowb     ''FamFlavour)
$(deriveShowPragmas defaultInlineShowbPrec ''Fixity)
$(deriveShowPragmas defaultInlineShowb     ''FixityDirection)
$(deriveShowPragmas defaultInlineShowbPrec ''Foreign)
$(deriveShowPragmas defaultInlineShowbPrec ''FunDep)
$(deriveShowPragmas defaultInlineShowbPrec ''Guard)
$(deriveShowPragmas defaultInlineShowbPrec ''Info)
$(deriveShowPragmas defaultInlineShowbPrec ''Lit)
$(deriveShowPragmas defaultInlineShowbPrec ''Loc)
$(deriveShowPragmas defaultInlineShowbPrec ''Match)
$(deriveShowPragmas defaultInlineShowbPrec ''ModName)

instance Show Name where
    showb = showbName
    INLINE_INST_FUN(showb)

$(deriveShowPragmas defaultInlineShowbPrec ''OccName)
$(deriveShowPragmas defaultInlineShowbPrec ''Pat)
$(deriveShowPragmas defaultInlineShowbPrec ''PkgName)
$(deriveShowPragmas defaultInlineShowbPrec ''Pragma)
$(deriveShowPragmas defaultInlineShowbPrec ''Range)
$(deriveShowPragmas defaultInlineShowb     ''Safety)
$(deriveShowPragmas defaultInlineShowbPrec ''Stmt)
$(deriveShowPragmas defaultInlineShowb     ''Strict)
$(deriveShowPragmas defaultInlineShowbPrec ''Type)
$(deriveShowPragmas defaultInlineShowbPrec ''TyVarBndr)

#if MIN_VERSION_template_haskell(2,5,0) && !(MIN_VERSION_template_haskell(2,7,0))
$(deriveShowPragmas defaultInlineShowbPrec ''ClassInstance)
#endif

#if MIN_VERSION_template_haskell(2,8,0)
$(deriveShowPragmas defaultInlineShowb     ''Inline)
$(deriveShowPragmas defaultInlineShowbPrec ''Phases)
$(deriveShowPragmas defaultInlineShowbPrec ''RuleBndr)
$(deriveShowPragmas defaultInlineShowb     ''RuleMatch)
$(deriveShowPragmas defaultInlineShowbPrec ''TyLit)
#else
$(deriveShowPragmas defaultInlineShowb     ''InlineSpec)
$(deriveShowPragmas defaultInlineShowbPrec ''Kind)
#endif

#if MIN_VERSION_template_haskell(2,9,0)
$(deriveShowPragmas defaultInlineShowbPrec ''AnnLookup)
$(deriveShowPragmas defaultInlineShowbPrec ''AnnTarget)
$(deriveShowPragmas defaultInlineShowbPrec ''Module)
$(deriveShowPragmas defaultInlineShowbPrec ''ModuleInfo)
$(deriveShowPragmas defaultInlineShowb     ''Role)
$(deriveShowPragmas defaultInlineShowbPrec ''TySynEqn)
#endif

#if !(MIN_VERSION_template_haskell(2,10,0))
$(deriveShowPragmas defaultInlineShowbPrec ''Pred)
#endif
