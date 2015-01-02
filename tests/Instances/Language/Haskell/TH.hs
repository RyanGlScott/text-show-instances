{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
#if !(MIN_VERSION_template_haskell(2,10,0))
{-# LANGUAGE MagicHash #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Language.Haskell.TH
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types in the @template-haskell@ library.
-}
module Instances.Language.Haskell.TH () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif

import Data.Functor ((<$>))

#if !(MIN_VERSION_template_haskell(2,10,0))
import GHC.Exts (Int(I#))
#endif

import Language.Haskell.TH.Syntax
#if !(MIN_VERSION_template_haskell(2,8,0))
import Language.Haskell.TH.Syntax.Internals (ModName(..), PkgName(..), OccName(..))
#endif

import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary Body where
    arbitrary = oneof $ map pure [ GuardedB [(fGuard, fExp)]
                                 , NormalB  fExp
                                 ]
--     arbitrary = oneof [GuardedB <$> arbitrary, NormalB <$> arbitrary]

instance Arbitrary Callconv where
    arbitrary = oneof $ map pure [ CCall
                                 , StdCall
#if MIN_VERSION_template_haskell(2,10,0)
                                 , CApi
                                 , Prim
                                 , JavaScript
#endif
                                 ]

instance Arbitrary Clause where
    arbitrary = pure $ Clause [fPat] fBody [fDec]
--     arbitrary = Clause <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Con where
    arbitrary = oneof [ NormalC <$> arbitrary <*> arbitrary
                      , RecC    <$> arbitrary <*> arbitrary
                      , InfixC  <$> arbitrary <*> arbitrary <*> arbitrary
                      , (flip . flip ForallC) [fPred] fCon <$> arbitrary
                      ]
--     arbitrary = oneof [ NormalC <$> arbitrary <*> arbitrary
--                       , RecC    <$> arbitrary <*> arbitrary
--                       , InfixC  <$> arbitrary <*> arbitrary <*> arbitrary
--                       , ForallC <$> arbitrary <*> arbitrary <*> arbitrary
--                       ]

instance Arbitrary Dec where
    arbitrary = oneof [
          flip FunD [fClause] <$> arbitrary
        , pure $ ValD fPat fBody [fDec]
        , DataD [fPred] <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , NewtypeD [fPred] <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , TySynD <$> arbitrary <*> arbitrary <*> arbitrary
        , (flip . ((flip . (flip .)) .) . ClassD) [fPred] [fDec]
            <$> arbitrary <*> arbitrary <*> arbitrary
        , (flip . InstanceD) [fPred] [fDec] <$> arbitrary
        , SigD <$> arbitrary <*> arbitrary
        , ForeignD <$> arbitrary
        , PragmaD <$> arbitrary
        , FamilyD <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , DataInstD [fPred] <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , NewtypeInstD [fPred] <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
#if MIN_VERSION_template_haskell(2,8,0)
        , InfixD <$> arbitrary <*> arbitrary
#endif
#if MIN_VERSION_template_haskell(2,9,0)
        , ClosedTypeFamilyD <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , RoleAnnotD <$> arbitrary <*> arbitrary
        , TySynInstD <$> arbitrary <*> arbitrary
#else
        , TySynInstD <$> arbitrary <*> arbitrary <*> arbitrary
#endif
#if MIN_VERSION_template_haskell(2,10,0)
        , StandaloneDerivD <$> arbitrary <*> arbitrary
        , DefaultSigD <$> arbitrary <*> arbitrary
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
                      , flip RecConE [fFieldExp] <$> arbitrary
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

instance Arbitrary FamFlavour where
    arbitrary = oneof $ map pure [TypeFam, DataFam]

instance Arbitrary Fixity where
    arbitrary = Fixity <$> arbitrary <*> arbitrary

instance Arbitrary FixityDirection where
    arbitrary = oneof $ map pure [InfixL, InfixR, InfixN]

instance Arbitrary Foreign where
    arbitrary = oneof [ ImportF <$> arbitrary <*> arbitrary <*> arbitrary
                                <*> arbitrary <*> arbitrary
                      , ExportF <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      ]

instance Arbitrary FunDep where
    arbitrary = FunDep <$> arbitrary <*> arbitrary

instance Arbitrary Guard where
        arbitrary = oneof $ map pure [ NormalG fExp
                                     , PatG    [fStmt]
                                     ]
--     arbitrary = oneof [NormalG <$> arbitrary, PatG <$> arbitrary]

instance Arbitrary Info where
    arbitrary = oneof [
#if MIN_VERSION_template_haskell(2,5,0)
          pure $ ClassI fDec [fDec]
#else
          pure $ ClassI fDec
#endif
        , ClassOpI   <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , pure $ TyConI fDec
        , PrimTyConI <$> arbitrary <*> arbitrary <*> arbitrary
        , DataConI   <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , VarI       <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , TyVarI     <$> arbitrary <*> arbitrary
#if MIN_VERSION_template_haskell(2,7,0)
        , pure $ FamilyI fDec [fDec]
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
    arbitrary = oneof [ CharL       <$> arbitrary
                      , StringL     <$> arbitrary
                      , IntegerL    <$> arbitrary
                      , RationalL   <$> arbitrary
                      , IntPrimL    <$> arbitrary
                      , WordPrimL   <$> arbitrary
                      , FloatPrimL  <$> arbitrary
                      , DoublePrimL <$> arbitrary
#if MIN_VERSION_template_haskell(2,5,0)
                      , StringPrimL <$> arbitrary
#endif
                      ]

instance Arbitrary Loc where
    arbitrary = Loc <$> arbitrary <*> arbitrary <*> arbitrary
                    <*> arbitrary <*> arbitrary

#if !(MIN_VERSION_template_haskell(2,10,0))
deriving instance Show Loc
#endif

instance Arbitrary Match where
    arbitrary = (flip . flip Match) fBody [fDec] <$> arbitrary
--     arbitrary = Match <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Name where
    arbitrary = Name <$> arbitrary <*> arbitrary

instance Arbitrary NameFlavour where
    arbitrary = oneof [ pure NameS
                      , NameQ <$> arbitrary
#if MIN_VERSION_template_haskell(2,10,0)
                      , NameU <$> arbitrary
                      , NameL <$> arbitrary
#else
                      , (\(I# i#) -> NameU i#) <$> arbitrary
                      , (\(I# i#) -> NameL i#) <$> arbitrary
#endif
                      , NameG <$> arbitrary <*> arbitrary <*> arbitrary
                      ]

instance Arbitrary NameIs where
    arbitrary = oneof $ map pure [Alone, Applied, Infix]

deriving instance Show NameIs

instance Arbitrary NameSpace where
    arbitrary = oneof $ map pure [VarName, DataName, TcClsName]

instance Arbitrary Pat where
    arbitrary = oneof [ LitP <$> arbitrary
                      , VarP <$> arbitrary
                      , pure $ TupP [fPat]
                      , flip ConP [fPat] <$> arbitrary
                      , (flip . InfixP) fPat fPat <$> arbitrary
                      , pure $ TildeP fPat
                      , pure $ BangP fPat
                      , flip AsP fPat <$> arbitrary
                      , pure WildP
                      , flip RecP [fFieldPat] <$> arbitrary
                      , pure $ ListP [fPat]
                      , SigP fPat <$> arbitrary
#if MIN_VERSION_template_haskell(2,5,0)
                      , pure $ ViewP fExp fPat
#endif
#if MIN_VERSION_template_haskell(2,6,0)
                      , pure $ UnboxedTupP [fPat]
#endif
#if MIN_VERSION_template_haskell(2,7,0)
                      , (flip . UInfixP) fPat fPat <$> arbitrary
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
    arbitrary = oneof
        [
#if MIN_VERSION_template_haskell(2,8,0)
          InlineP         <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , SpecialiseP     <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , SpecialiseInstP <$> arbitrary
        , RuleP           <$> arbitrary <*> arbitrary <*> arbitrary
                          <*> arbitrary <*> arbitrary
# if MIN_VERSION_template_haskell(2,9,0)
        , AnnP            <$> arbitrary <*> arbitrary
# endif
# if MIN_VERSION_template_haskell(2,10,0)
        , LineP           <$> arbitrary <*> arbitrary
# endif
#else
          InlineP     <$> arbitrary <*> arbitrary
        , SpecialiseP <$> arbitrary <*> arbitrary <*> arbitrary
#endif
        ]

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

instance Arbitrary Safety where
    arbitrary = oneof $ map pure [ Unsafe
                                 , Safe
#if MIN_VERSION_template_haskell(2,6,0)
                                 , Interruptible
#else
                                 , Threadsafe
#endif
                                 ]

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

instance Arbitrary Strict where
    arbitrary = oneof $ map pure [ IsStrict
                                 , NotStrict
#if MIN_VERSION_template_haskell(2,7,0)
                                 , Unpacked
#endif
                                 ]

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
                      , UnboxedTupleT  <$> arbitrary
#endif
#if MIN_VERSION_template_haskell(2,7,0)
                      , PromotedT      <$> arbitrary
                      , PromotedTupleT <$> arbitrary
                      , pure PromotedNilT
                      , pure PromotedConsT
                      , pure StarT
                      , pure ConstraintT
                      , LitT           <$> arbitrary
#endif
#if MIN_VERSION_template_haskell(2,10,0)
                      , pure EqualityT
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
    arbitrary = oneof [ PlainTV <$> arbitrary
                      , flip KindedTV fKind <$> arbitrary
                      ]
--     arbitrary = oneof [PlainTV <$> arbitrary, KindedTV <$> arbitrary <*> arbitrary]

#if MIN_VERSION_template_haskell(2,5,0) && !(MIN_VERSION_template_haskell(2,7,0))
instance Arbitrary ClassInstance where
    arbitrary = ClassInstance <$> arbitrary <*> arbitrary <*> arbitrary
                              <*> arbitrary <*> arbitrary
#endif

#if MIN_VERSION_template_haskell(2,8,0)
instance Arbitrary Inline where
    arbitrary = oneof $ map pure [NoInline, Inline, Inlinable]

instance Arbitrary Phases where
    arbitrary = oneof [ pure AllPhases
                      , FromPhase   <$> arbitrary
                      , BeforePhase <$> arbitrary
                      ]

instance Arbitrary RuleBndr where
    arbitrary = oneof [RuleVar <$> arbitrary, TypedRuleVar <$> arbitrary <*> arbitrary]

instance Arbitrary RuleMatch where
    arbitrary = oneof $ map pure [ConLike, FunLike]

instance Arbitrary TyLit where
    arbitrary = oneof [NumTyLit <$> arbitrary, StrTyLit <$> arbitrary]
#else
instance Arbitrary InlineSpec where
    arbitrary = InlineSpec <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Kind where
    arbitrary = oneof [pure StarK, pure $ ArrowK fKind fKind]
--     arbitrary = oneof [pure StarK, ArrowK <$> arbitrary <*> arbitrary]
#endif

#if MIN_VERSION_template_haskell(2,9,0)
instance Arbitrary AnnLookup where
    arbitrary = oneof [AnnLookupModule <$> arbitrary, AnnLookupName <$> arbitrary]

instance Arbitrary AnnTarget where
    arbitrary = oneof [ pure ModuleAnnotation
                      , TypeAnnotation  <$> arbitrary
                      , ValueAnnotation <$> arbitrary
                      ]

instance Arbitrary Module where
    arbitrary = Module <$> arbitrary <*> arbitrary
                      
instance Arbitrary ModuleInfo where
    arbitrary = ModuleInfo <$> arbitrary

instance Arbitrary Role where
    arbitrary = oneof $ map pure [NominalR, RepresentationalR, PhantomR, InferR]

instance Arbitrary TySynEqn where
    arbitrary = TySynEqn <$> arbitrary <*> arbitrary
#endif

#if !(MIN_VERSION_template_haskell(2,10,0))
instance Arbitrary Pred where
    arbitrary = oneof [ flip ClassP [fType] <$> arbitrary
                      , pure $ EqualP fType fType
                      ]
--     arbitrary = oneof [ ClassP <$> arbitrary <*> arbitrary
--                       , EqualP <$> arbitrary <*> arbitrary
--                       ]
#endif

deriving instance Arbitrary ModName
deriving instance Arbitrary OccName
deriving instance Arbitrary PkgName

-------------------------------------------------------------------------------
-- Workarounds to make Arbitrary instances faster
-------------------------------------------------------------------------------

fBody :: Body
fBody = GuardedB []

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