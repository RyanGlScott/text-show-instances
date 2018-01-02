{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Language.Haskell.TH
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @template-haskell@ library.

/Since: 2/
-}
module TextShow.Language.Haskell.TH (showbName, showbName') where

import           Data.Char (isAlpha)
import           Data.Maybe (fromJust)
import           Data.Monoid.Compat
import qualified Data.Text.Lazy as TL (Text, dropWhile, null, tail)
import           Data.Text.Lazy (uncons)

#if !(MIN_VERSION_template_haskell(2,10,0))
import           GHC.Exts (Int(I#))
#endif

import           Language.Haskell.TH.PprLib (Doc, to_HPJ_Doc)
import           Language.Haskell.TH.Syntax

import           TextShow (TextShow(..), Builder,
                           fromString, singleton, toLazyText)
import           TextShow.Text.PrettyPrint (renderB)
import           TextShow.TH (deriveTextShow)

-- | Convert a 'Name' to a 'Builder'.
--
-- /Since: 2/
showbName :: Name -> Builder
showbName = showbName' Alone

-- | Convert a 'Name' to a 'Builder' with the given 'NameIs' settings.
--
-- /Since: 2/
showbName' :: NameIs -> Name -> Builder
showbName' ni nm = case ni of
    Alone           -> nms
    Applied
        | pnam      -> nms
        | otherwise -> singleton '(' <> nms <> singleton ')'
    Infix
        | pnam      -> singleton '`' <> nms <> singleton '`'
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
               Name occ (NameQ m)     -> modB m   <> singleton '.' <> occB occ
               Name occ (NameG _ _ m) -> modB m   <> singleton '.' <> occB occ
               Name occ (NameU u)     -> occB occ <> singleton '_' <> showb (mkInt u)
               Name occ (NameL u)     -> occB occ <> singleton '_' <> showb (mkInt u)

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

-- | /Since: 2/
$(deriveTextShow ''Body)
-- | /Since: 2/
$(deriveTextShow ''Callconv)
-- | /Since: 2/
$(deriveTextShow ''Clause)
-- | /Since: 2/
$(deriveTextShow ''Con)
-- | /Since: 2/
$(deriveTextShow ''Dec)
-- | /Since: 2/
$(deriveTextShow ''Exp)
#if !(MIN_VERSION_template_haskell(2,13,0))
-- | /Since: 2/
$(deriveTextShow ''FamFlavour)
#endif
-- | /Since: 2/
$(deriveTextShow ''Fixity)
-- | /Since: 2/
$(deriveTextShow ''FixityDirection)
-- | /Since: 2/
$(deriveTextShow ''Foreign)
-- | /Since: 2/
$(deriveTextShow ''FunDep)
-- | /Since: 2/
$(deriveTextShow ''Guard)
-- | /Since: 2/
$(deriveTextShow ''Info)
-- | /Since: 2/
$(deriveTextShow ''Lit)
-- | /Since: 2/
$(deriveTextShow ''Loc)
-- | /Since: 2/
$(deriveTextShow ''Match)
-- | /Since: 2/
$(deriveTextShow ''ModName)

-- | /Since: 2/
instance TextShow Name where
    showb = showbName

-- | /Since: 3.3/
$(deriveTextShow ''NameFlavour)
-- | /Since: 3.3/
$(deriveTextShow ''NameSpace)
-- | /Since: 2/
$(deriveTextShow ''OccName)
-- | /Since: 2/
$(deriveTextShow ''Pat)
-- | /Since: 2/
$(deriveTextShow ''PkgName)
-- | /Since: 2/
$(deriveTextShow ''Pragma)
-- | /Since: 2/
$(deriveTextShow ''Range)
-- | /Since: 2/
$(deriveTextShow ''Safety)
-- | /Since: 2/
$(deriveTextShow ''Stmt)
-- | /Since: 2/
$(deriveTextShow ''Type)
-- | /Since: 2/
$(deriveTextShow ''TyVarBndr)

-- | /Since: 2/
instance TextShow Doc where
    showb = renderB . to_HPJ_Doc

#if MIN_VERSION_template_haskell(2,8,0)
-- | Only available with @template-haskell-2.8.0.0@ or later.
--
-- /Since: 2/
$(deriveTextShow ''Inline)
-- | Only available with @template-haskell-2.8.0.0@ or later.
--
-- /Since: 2/
$(deriveTextShow ''Phases)
-- | Only available with @template-haskell-2.8.0.0@ or later.
--
-- /Since: 2/
$(deriveTextShow ''RuleBndr)
-- | Only available with @template-haskell-2.8.0.0@ or later.
--
-- /Since: 2/
$(deriveTextShow ''RuleMatch)
-- | Only available with @template-haskell-2.8.0.0@ or later.
--
-- /Since: 2/
$(deriveTextShow ''TyLit)
#else
-- | /Since: 2/
$(deriveTextShow ''InlineSpec)
-- | /Since: 2/
$(deriveTextShow ''Kind)
#endif

#if MIN_VERSION_template_haskell(2,9,0)
-- | Only available with @template-haskell-2.9.0.0@ or later.
--
-- /Since: 2/
$(deriveTextShow ''AnnLookup)
-- | Only available with @template-haskell-2.9.0.0@ or later.
--
-- /Since: 2/
$(deriveTextShow ''AnnTarget)
-- | Only available with @template-haskell-2.9.0.0@ or later.
--
-- /Since: 2/
$(deriveTextShow ''Module)
-- | Only available with @template-haskell-2.9.0.0@ or later.
--
-- /Since: 2/
$(deriveTextShow ''ModuleInfo)
-- | Only available with @template-haskell-2.9.0.0@ or later.
--
-- /Since: 2/
$(deriveTextShow ''Role)
-- | Only available with @template-haskell-2.9.0.0@ or later.
--
-- /Since: 2/
$(deriveTextShow ''TySynEqn)
#endif

#if !(MIN_VERSION_template_haskell(2,10,0))
-- | Only available with @template-haskell-2.10@ or earlier.
--
-- /Since: 2/
$(deriveTextShow ''Pred)
#endif

#if MIN_VERSION_template_haskell(2,11,0)
-- | Only available with @template-haskell-2.11.0.0@ or later.
--
-- /Since: 3/
$(deriveTextShow ''Bang)
-- | Only available with @template-haskell-2.11.0.0@ or later.
--
-- /Since: 3/
$(deriveTextShow ''DecidedStrictness)
-- | Only available with @template-haskell-2.11.0.0@ or later.
--
-- /Since: 3/
$(deriveTextShow ''FamilyResultSig)
-- | Only available with @template-haskell-2.11.0.0@ or later.
--
-- /Since: 3/
$(deriveTextShow ''InjectivityAnn)
-- | Only available with @template-haskell-2.11.0.0@ or later.
--
-- /Since: 3/
$(deriveTextShow ''Overlap)
-- | Only available with @template-haskell-2.11.0.0@ or later.
--
-- /Since: 3/
$(deriveTextShow ''SourceStrictness)
-- | Only available with @template-haskell-2.11.0.0@ or later.
--
-- /Since: 3/
$(deriveTextShow ''SourceUnpackedness)
-- | Only available with @template-haskell-2.11.0.0@ or later.
--
-- /Since: 3/
$(deriveTextShow ''TypeFamilyHead)
#else
-- | Only available with @template-haskell-2.11@ or earlier.
--
-- /Since: 3/
$(deriveTextShow ''Strict)
#endif

#if MIN_VERSION_template_haskell(2,12,0)
-- | Only available with @template-haskell-2.12.0.0@ or later.
--
-- /Since: 3.6/
$(deriveTextShow ''DerivClause)
-- | Only available with @template-haskell-2.12.0.0@ or later.
--
-- /Since: 3.6/
$(deriveTextShow ''DerivStrategy)
-- | Only available with @template-haskell-2.12.0.0@ or later.
--
-- /Since: 3.3/
$(deriveTextShow ''PatSynArgs)
-- | Only available with @template-haskell-2.12.0.0@ or later.
--
-- /Since: 3.3/
$(deriveTextShow ''PatSynDir)
#endif
