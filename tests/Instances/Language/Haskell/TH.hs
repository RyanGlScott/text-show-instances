{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Language.Haskell.TH
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for data types in the @template-haskell@ library.
-}
module Instances.Language.Haskell.TH () where

import Instances.Utils ((<@>))
import Instances.Utils.GenericArbitrary (genericArbitrary)

import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.PprLib (Doc, text)
import Language.Haskell.TH.Syntax
#if !(MIN_VERSION_template_haskell(2,8,0))
import Language.Haskell.TH.Syntax.Internals
#endif

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, oneof)

instance Arbitrary Body where
    arbitrary = oneof $ map pure [ GuardedB [(fGuard, fExp)]
                                 , NormalB  fExp
                                 ]
--     arbitrary = oneof [GuardedB <$> arbitrary, NormalB <$> arbitrary]

deriving instance Bounded Callconv
deriving instance Enum Callconv
instance Arbitrary Callconv where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Clause where
    arbitrary = pure $ Clause [fPat] fBody [fDec]
--     arbitrary = Clause <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Con where
    arbitrary = oneof [ NormalC  <$> arbitrary <*> arbitrary
                      , RecC     <$> arbitrary <*> arbitrary
                      , InfixC   <$> arbitrary <*> arbitrary <*> arbitrary
                      , ForallC  <$> arbitrary <@> [fPred]   <@> fCon
#if MIN_VERSION_template_haskell(2,11,0)
                      , GadtC    <$> arbitrary <*> arbitrary <*> arbitrary
                      , RecGadtC <$> arbitrary <*> arbitrary <*> arbitrary
#endif
                      ]
--     arbitrary = oneof [ NormalC <$> arbitrary <*> arbitrary
--                       , RecC    <$> arbitrary <*> arbitrary
--                       , InfixC  <$> arbitrary <*> arbitrary <*> arbitrary
--                       , ForallC <$> arbitrary <*> arbitrary <*> arbitrary
--                       ]

instance Arbitrary Dec where
    arbitrary = oneof [
          FunD <$> arbitrary <@> [fClause]
        , pure $ ValD fPat fBody [fDec]
        , DataD [fPred] <$> arbitrary
                        <*> arbitrary
#if MIN_VERSION_template_haskell(2,11,0)
                        <*> arbitrary
#endif
                        <*> arbitrary
                        <*> arbitrary
        , NewtypeD [fPred] <$> arbitrary
                           <*> arbitrary
#if MIN_VERSION_template_haskell(2,11,0)
                           <*> arbitrary
#endif
                           <*> arbitrary
                           <*> arbitrary
        , TySynD <$> arbitrary <*> arbitrary <*> arbitrary
        , ClassD [fPred] <$> arbitrary <*> arbitrary <*> arbitrary <@> [fDec]
        , pure InstanceD
#if MIN_VERSION_template_haskell(2,11,0)
                         <*> arbitrary
#endif
                         <@> [fPred]
                         <*> arbitrary
                         <@> [fDec]
        , SigD <$> arbitrary <*> arbitrary
        , ForeignD <$> arbitrary
        , PragmaD <$> arbitrary
        , DataInstD [fPred] <$> arbitrary
                            <*> arbitrary
#if MIN_VERSION_template_haskell(2,11,0)
                            <*> arbitrary
#endif
                            <*> arbitrary
                            <*> arbitrary
        , NewtypeInstD [fPred] <$> arbitrary
                               <*> arbitrary
#if MIN_VERSION_template_haskell(2,11,0)
                               <*> arbitrary
#endif
                               <*> arbitrary
                               <*> arbitrary
#if MIN_VERSION_template_haskell(2,8,0)
        , InfixD <$> arbitrary <*> arbitrary
#endif
#if MIN_VERSION_template_haskell(2,9,0)
        , ClosedTypeFamilyD <$> arbitrary
                            <*> arbitrary
# if !(MIN_VERSION_template_haskell(2,11,0))
                            <*> arbitrary
                            <*> arbitrary
# endif
        , RoleAnnotD <$> arbitrary <*> arbitrary
        , TySynInstD <$> arbitrary <*> arbitrary
#else
        , TySynInstD <$> arbitrary <*> arbitrary <*> arbitrary
#endif
#if MIN_VERSION_template_haskell(2,10,0)
        , StandaloneDerivD <$> arbitrary
                           <*> arbitrary
# if MIN_VERSION_template_haskell(2,12,0)
                           <*> arbitrary
# endif
        , DefaultSigD <$> arbitrary <*> arbitrary
#endif
#if MIN_VERSION_template_haskell(2,11,0)
        , DataFamilyD <$> arbitrary <*> arbitrary <*> arbitrary
        , OpenTypeFamilyD <$> arbitrary
#else
        , FamilyD <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
#endif
#if MIN_VERSION_template_haskell(2,12,0)
        , PatSynD <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , PatSynSigD <$> arbitrary <*> arbitrary
#endif
        ]
--     arbitrary = oneof [
--           FunD              <$> arbitrary <*> arbitrary
--         , ValD              <$> arbitrary <*> arbitrary <*> arbitrary
--         , DataD             <$> arbitrary <*> arbitrary <*> arbitrary
--                             <*> arbitrary <*> arbitrary
--         , NewtypeD          <$> arbitrary <*> arbitrary <*> arbitrary
--                             <*> arbitrary <*> arbitrary
--         , TySynD            <$> arbitrary <*> arbitrary <*> arbitrary
--         , ClassD            <$> arbitrary <*> arbitrary <*> arbitrary
--                             <*> arbitrary <*> arbitrary
--         , InstanceD         <$> arbitrary <*> arbitrary <*> arbitrary
--         , SigD              <$> arbitrary <*> arbitrary
--         , ForeignD          <$> arbitrary
--         , PragmaD           <$> arbitrary
--         , FamilyD           <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--         , DataInstD         <$> arbitrary <*> arbitrary <*> arbitrary
--                             <*> arbitrary <*> arbitrary
--         , NewtypeInstD      <$> arbitrary <*> arbitrary <*> arbitrary
--                             <*> arbitrary <*> arbitrary
-- #if MIN_VERSION_template_haskell(2,8,0)
--         , InfixD            <$> arbitrary <*> arbitrary
-- #endif
-- #if MIN_VERSION_template_haskell(2,9,0)
--         , ClosedTypeFamilyD <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--         , RoleAnnotD        <$> arbitrary <*> arbitrary
--         , TySynInstD        <$> arbitrary <*> arbitrary
-- #else
--         , TySynInstD        <$> arbitrary <*> arbitrary <*> arbitrary
-- #endif
-- #if MIN_VERSION_template_haskell(2,10,0)
--         , StandaloneDerivD  <$> arbitrary <*> arbitrary
--         , DefaultSigD       <$> arbitrary <*> arbitrary
-- #endif
--         ]

instance Arbitrary Exp where
    arbitrary = oneof [ VarE <$> arbitrary
                      , ConE <$> arbitrary
                      , LitE <$> arbitrary
                      , pure $ AppE fExp fExp
                      , pure $ InfixE (Just fExp) fExp (Just fExp)
                      , pure $ LamE [fPat] fExp
                      , pure $ TupE [fExp]
                      , pure $ CondE fExp fExp fExp
                      , pure $ LetE [fDec] fExp
                      , pure $ CaseE fExp [fMatch]
                      , pure $ DoE [fStmt]
                      , pure $ CompE [fStmt]
                      , pure $ ArithSeqE fRange
                      , pure $ ListE [fExp]
                      , SigE fExp <$> arbitrary
                      , RecConE <$> arbitrary <@> [fFieldExp]
                      , pure $ RecUpdE fExp [fFieldExp]
#if MIN_VERSION_template_haskell(2,6,0)
                      , pure $ UnboxedTupE [fExp]
#endif
#if MIN_VERSION_template_haskell(2,7,0)
                      , pure $ UInfixE fExp fExp fExp
                      , pure $ ParensE fExp
#endif
#if MIN_VERSION_template_haskell(2,8,0)
                      , pure $ LamCaseE [fMatch]
                      , pure $ MultiIfE [(fGuard, fExp)]
#endif
#if MIN_VERSION_template_haskell(2,10,0)
                      , pure $ StaticE fExp
#endif
#if MIN_VERSION_template_haskell(2,11,0)
                      , UnboundVarE <$> arbitrary
#endif
                      ]
--     arbitrary = oneof [ VarE        <$> arbitrary
--                       , ConE        <$> arbitrary
--                       , LitE        <$> arbitrary
--                       , AppE        <$> arbitrary <*> arbitrary
--                       , InfixE      <$> arbitrary <*> arbitrary <*> arbitrary
--                       , LamE        <$> arbitrary <*> arbitrary
--                       , TupE        <$> arbitrary
--                       , CondE       <$> arbitrary <*> arbitrary <*> arbitrary
--                       , LetE        <$> arbitrary <*> arbitrary
--                       , CaseE       <$> arbitrary <*> arbitrary
--                       , DoE         <$> arbitrary
--                       , CompE       <$> arbitrary
--                       , ArithSeqE   <$> arbitrary
--                       , ListE       <$> arbitrary
--                       , SigE        <$> arbitrary <*> arbitrary
--                       , RecConE     <$> arbitrary <*> arbitrary
--                       , RecUpdE     <$> arbitrary <*> arbitrary
-- #if MIN_VERSION_template_haskell(2,6,0)
--                       , UnboxedTupE <$> arbitrary
-- #endif
-- #if MIN_VERSION_template_haskell(2,7,0)
--                       , UInfixE     <$> arbitrary <*> arbitrary <*> arbitrary
--                       , ParensE     <$> arbitrary
-- #endif
-- #if MIN_VERSION_template_haskell(2,8,0)
--                       , LamCaseE    <$> arbitrary
--                       , MultiIfE    <$> arbitrary
-- #endif
-- #if MIN_VERSION_template_haskell(2,10,0)
--                       , StaticE     <$> arbitrary
-- #endif
--                       ]

deriving instance Bounded FamFlavour
deriving instance Enum FamFlavour
instance Arbitrary FamFlavour where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Fixity where
    arbitrary = genericArbitrary

deriving instance Bounded FixityDirection
deriving instance Enum FixityDirection
instance Arbitrary FixityDirection where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Foreign where
    arbitrary = genericArbitrary

instance Arbitrary FunDep where
    arbitrary = genericArbitrary

instance Arbitrary Guard where
        arbitrary = oneof $ map pure [ NormalG fExp
                                     , PatG    [fStmt]
                                     ]
--     arbitrary = oneof [NormalG <$> arbitrary, PatG <$> arbitrary]

instance Arbitrary Info where
    arbitrary = oneof [
#if   MIN_VERSION_template_haskell(2,7,0)
          pure $ ClassI fDec [fDec]
#elif MIN_VERSION_template_haskell(2,5,0)
          pure $ ClassI fDec [fClassInstance]
#else
          pure $ ClassI fDec
#endif
        , ClassOpI   <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
#if !(MIN_VERSION_template_haskell(2,11,0))
                     <*> arbitrary
#endif
        , pure $ TyConI fDec
        , PrimTyConI <$> arbitrary <*> arbitrary <*> arbitrary
        , DataConI   <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
#if !(MIN_VERSION_template_haskell(2,11,0))
                     <*> arbitrary
#endif
        , VarI       <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
#if !(MIN_VERSION_template_haskell(2,11,0))
                     <*> arbitrary
#endif
        , TyVarI     <$> arbitrary <*> arbitrary
#if MIN_VERSION_template_haskell(2,7,0)
        , pure $ FamilyI fDec [fDec]
#endif
#if MIN_VERSION_template_haskell(2,12,0)
        , PatSynI    <$> arbitrary <*> arbitrary
#endif
        ]
--     arbitrary = oneof [
-- #if MIN_VERSION_template_haskell(2,5,0)
--           ClassI     <$> arbitrary <*> arbitrary
-- #else
--           ClassI     <$> arbitrary
-- #endif
--         , ClassOpI   <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--         , TyConI     <$> arbitrary
--         , PrimTyConI <$> arbitrary <*> arbitrary <*> arbitrary
--         , DataConI   <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--         , VarI       <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--         , TyVarI     <$> arbitrary <*> arbitrary
-- #if MIN_VERSION_template_haskell(2,7,0)
--         , FamilyI    <$> arbitrary <*> arbitrary
-- #endif
--         ]

instance Arbitrary Lit where
    arbitrary = genericArbitrary

instance Arbitrary Loc where
    arbitrary = genericArbitrary

instance Arbitrary Match where
    arbitrary = Match <$> arbitrary <@> fBody <@> [fDec]
--     arbitrary = Match <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Name where
    arbitrary = genericArbitrary

instance Arbitrary NameFlavour where
    arbitrary = genericArbitrary

deriving instance Bounded NameIs
deriving instance Enum NameIs
deriving instance Show NameIs
instance Arbitrary NameIs where
    arbitrary = arbitraryBoundedEnum

deriving instance Bounded NameSpace
deriving instance Enum NameSpace
instance Arbitrary NameSpace where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Pat where
    arbitrary = oneof [ LitP <$> arbitrary
                      , VarP <$> arbitrary
                      , pure $ TupP [fPat]
                      , ConP <$> arbitrary <@> [fPat]
                      , InfixP fPat <$> arbitrary <@> fPat
                      , pure $ TildeP fPat
                      , pure $ BangP fPat
                      , AsP <$> arbitrary <@> fPat
                      , pure WildP
                      , RecP <$> arbitrary <@> [fFieldPat]
                      , pure $ ListP [fPat]
                      , SigP fPat <$> arbitrary
#if MIN_VERSION_template_haskell(2,5,0)
                      , pure $ ViewP fExp fPat
#endif
#if MIN_VERSION_template_haskell(2,6,0)
                      , pure $ UnboxedTupP [fPat]
#endif
#if MIN_VERSION_template_haskell(2,7,0)
                      , UInfixP fPat <$> arbitrary <@> fPat
                      , pure $ ParensP fPat
#endif
                      ]
--     arbitrary = oneof [ LitP        <$> arbitrary
--                       , VarP        <$> arbitrary
--                       , TupP        <$> arbitrary
--                       , ConP        <$> arbitrary <*> arbitrary
--                       , InfixP      <$> arbitrary <*> arbitrary <*> arbitrary
--                       , TildeP      <$> arbitrary
--                       , BangP       <$> arbitrary
--                       , AsP         <$> arbitrary <*> arbitrary
--                       , pure WildP
--                       , RecP        <$> arbitrary <*> arbitrary
--                       , ListP       <$> arbitrary
--                       , SigP        <$> arbitrary <*> arbitrary
-- #if MIN_VERSION_template_haskell(2,5,0)
--                       , ViewP       <$> arbitrary <*> arbitrary
-- #endif
-- #if MIN_VERSION_template_haskell(2,6,0)
--                       , UnboxedTupP <$> arbitrary
-- #endif
-- #if MIN_VERSION_template_haskell(2,7,0)
--                       , UInfixP     <$> arbitrary <*> arbitrary <*> arbitrary
--                       , ParensP     <$> arbitrary
-- #endif
--                       ]

instance Arbitrary Pragma where
    arbitrary = genericArbitrary

instance Arbitrary Range where
    arbitrary = oneof $ map pure [ FromR       fExp
                                 , FromThenR   fExp fExp
                                 , FromToR     fExp fExp
                                 , FromThenToR fExp fExp fExp
                                 ]
--     arbitrary = oneof [ FromR       <$> arbitrary
--                       , FromThenR   <$> arbitrary <*> arbitrary
--                       , FromToR     <$> arbitrary <*> arbitrary
--                       , FromThenToR <$> arbitrary <*> arbitrary <*> arbitrary
--                       ]

deriving instance Bounded Safety
deriving instance Enum Safety
instance Arbitrary Safety where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Stmt where
    arbitrary = oneof $ map pure [ BindS   fPat fExp
                                 , LetS    [fDec]
                                 , NoBindS fExp
                                 , ParS    [[fStmt]]
                                 ]
--     arbitrary = oneof [ BindS   <$> arbitrary <*> arbitrary
--                       , LetS    <$> arbitrary
--                       , NoBindS <$> arbitrary
--                       , ParS    <$> arbitrary
--                       ]

instance Arbitrary Type where
    arbitrary = oneof [ pure $ ForallT [fTyVarBndr] [fPred] fType
                      , VarT <$> arbitrary
                      , ConT <$> arbitrary
                      , TupleT <$> arbitrary
                      , pure ArrowT
                      , pure ListT
                      , pure $ AppT fType fType
                      , pure $ SigT fType fKind
#if MIN_VERSION_template_haskell(2,6,0)
                      , UnboxedTupleT <$> arbitrary
#endif
#if MIN_VERSION_template_haskell(2,8,0)
                      , PromotedT <$> arbitrary
                      , PromotedTupleT <$> arbitrary
                      , pure PromotedNilT
                      , pure PromotedConsT
                      , pure StarT
                      , pure ConstraintT
                      , LitT <$> arbitrary
#endif
#if MIN_VERSION_template_haskell(2,10,0)
                      , pure EqualityT
#endif
#if MIN_VERSION_template_haskell(2,11,0)
                      , InfixT  fType <$> arbitrary <@> fType
                      , UInfixT fType <$> arbitrary <@> fType
                      , pure $ ParensT fType
                      , pure WildCardT
#endif
                      ]
--     arbitrary = oneof [ ForallT        <$> arbitrary <*> arbitrary <*> arbitrary
--                       , VarT           <$> arbitrary
--                       , ConT           <$> arbitrary
--                       , TupleT         <$> arbitrary
--                       , pure ArrowT
--                       , pure ListT
--                       , AppT           <$> arbitrary <*> arbitrary
--                       , SigT           <$> arbitrary <*> arbitrary
-- #if MIN_VERSION_template_haskell(2,6,0)
--                       , UnboxedTupleT  <$> arbitrary
-- #endif
-- #if MIN_VERSION_template_haskell(2,7,0)
--                       , PromotedT      <$> arbitrary
--                       , PromotedTupleT <$> arbitrary
--                       , pure PromotedNilT
--                       , pure PromotedConsT
--                       , pure StarT
--                       , pure ConstraintT
--                       , LitT           <$> arbitrary
-- #endif
-- #if MIN_VERSION_template_haskell(2,10,0)
--                       , pure EqualityT
-- #endif
--                       ]

instance Arbitrary TyVarBndr where
    arbitrary = oneof [PlainTV  <$> arbitrary, KindedTV <$> arbitrary <@> fKind]
--     arbitrary = oneof [PlainTV <$> arbitrary, KindedTV <$> arbitrary <*> arbitrary]

#if MIN_VERSION_template_haskell(2,5,0) && !(MIN_VERSION_template_haskell(2,7,0))
instance Arbitrary ClassInstance where
    arbitrary = genericArbitrary
#endif

#if MIN_VERSION_template_haskell(2,8,0)
deriving instance Bounded Inline
deriving instance Enum Inline
instance Arbitrary Inline where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Phases where
    arbitrary = genericArbitrary

instance Arbitrary RuleBndr where
    arbitrary = genericArbitrary

deriving instance Bounded RuleMatch
deriving instance Enum RuleMatch
instance Arbitrary RuleMatch where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary TyLit where
    arbitrary = genericArbitrary
#else
instance Arbitrary InlineSpec where
    arbitrary = genericArbitrary

instance Arbitrary Kind where
    arbitrary = oneof [pure StarK, pure $ ArrowK fKind fKind]
--     arbitrary = oneof [pure StarK, ArrowK <$> arbitrary <*> arbitrary]
#endif

#if MIN_VERSION_template_haskell(2,9,0)
instance Arbitrary AnnLookup where
    arbitrary = genericArbitrary

instance Arbitrary AnnTarget where
    arbitrary = genericArbitrary

instance Arbitrary Module where
    arbitrary = genericArbitrary

instance Arbitrary ModuleInfo where
    arbitrary = genericArbitrary

deriving instance Bounded Role
deriving instance Enum Role
instance Arbitrary Role where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary TySynEqn where
    arbitrary = genericArbitrary
#endif

instance Arbitrary Doc where
    arbitrary = text <$> arbitrary

#if !(MIN_VERSION_template_haskell(2,10,0))
instance Arbitrary Pred where
    arbitrary = oneof [ ClassP <$> arbitrary <@> [fType]
                      , pure $ EqualP fType fType
                      ]
--     arbitrary = oneof [ ClassP <$> arbitrary <*> arbitrary
--                       , EqualP <$> arbitrary <*> arbitrary
--                       ]
#endif

#if MIN_VERSION_template_haskell(2,11,0)
instance Arbitrary Bang where
    arbitrary = genericArbitrary

deriving instance Bounded DecidedStrictness
deriving instance Enum DecidedStrictness
instance Arbitrary DecidedStrictness where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary FamilyResultSig where
    arbitrary = oneof [ pure NoSig
                      , pure $ KindSig fKind
                      , pure $ TyVarSig fTyVarBndr
                      ]

instance Arbitrary InjectivityAnn where
    arbitrary = pure $ InjectivityAnn fName [fName]

deriving instance Bounded Overlap
deriving instance Enum Overlap
instance Arbitrary Overlap where
    arbitrary = arbitraryBoundedEnum

deriving instance Bounded SourceStrictness
deriving instance Enum SourceStrictness
instance Arbitrary SourceStrictness where
    arbitrary = arbitraryBoundedEnum

deriving instance Bounded SourceUnpackedness
deriving instance Enum SourceUnpackedness
instance Arbitrary SourceUnpackedness where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary TypeFamilyHead where
    arbitrary = TypeFamilyHead fName
                               [fTyVarBndr]
                           <$> arbitrary
                           <*> arbitrary
#else
deriving instance Bounded Strict
deriving instance Enum Strict
instance Arbitrary Strict where
    arbitrary = arbitraryBoundedEnum
#endif

#if MIN_VERSION_template_haskell(2,12,0)
instance Arbitrary DerivClause where
    arbitrary = genericArbitrary

deriving instance Bounded DerivStrategy
deriving instance Enum DerivStrategy
instance Arbitrary DerivStrategy where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary PatSynArgs where
    arbitrary = oneof $ map pure [ PrefixPatSyn [fName]
                                 , InfixPatSyn fName fName
                                 , RecordPatSyn [fName]
                                 ]

instance Arbitrary PatSynDir where
    arbitrary = oneof $ map pure [ Unidir, ImplBidir, ExplBidir [fClause] ]
#endif

deriving instance Arbitrary ModName
deriving instance Arbitrary OccName
deriving instance Arbitrary PkgName

-------------------------------------------------------------------------------
-- Workarounds to make Arbitrary instances faster
-------------------------------------------------------------------------------

fBody :: Body
fBody = GuardedB []

#if MIN_VERSION_template_haskell(2,5,0) && !MIN_VERSION_template_haskell(2,7,0)
fClassInstance :: ClassInstance
fClassInstance = ClassInstance fName [fTyVarBndr] [fPred] fName [fType]
#endif

fClause :: Clause
fClause = Clause [] fBody []

fCon :: Con
fCon = NormalC fName []

fDec :: Dec
fDec = FunD fName []

fExp :: Exp
fExp = TupE []

fFieldExp :: FieldExp
fFieldExp = (fName, fExp)

fFieldPat :: FieldPat
fFieldPat = (fName, fPat)

fGuard :: Guard
fGuard = PatG []

fKind :: Kind
#if MIN_VERSION_template_haskell(2,8,0)
fKind = fType
#else
fKind = StarK
#endif

fMatch :: Match
fMatch = Match fPat fBody []

fName :: Name
fName = Name (OccName "") NameS

fPat :: Pat
fPat = WildP

fPred :: Pred
#if MIN_VERSION_template_haskell(2,10,0)
fPred = fType
#else
fPred = ClassP fName []
#endif

fRange :: Range
fRange = FromR fExp

fStmt :: Stmt
fStmt = LetS []

fType :: Type
fType = ListT

fTyVarBndr :: TyVarBndr
fTyVarBndr = PlainTV fName
