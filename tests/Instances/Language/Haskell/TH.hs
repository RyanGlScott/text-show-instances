{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, MagicHash, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Language.Haskell.TH
Copyright:   (C) 2014 Ryan Scott
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

import GHC.Exts (Int(I#))

import Language.Haskell.TH.Syntax
#if !(MIN_VERSION_template_haskell(2,8,0))
import Language.Haskell.TH.Syntax.Internals (ModName(..), PkgName(..), OccName(..))
#endif

import Test.Tasty.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary Body where
    arbitrary = oneof [GuardedB <$> arbitrary, NormalB <$> arbitrary]

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
    arbitrary = Clause <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Con where
    arbitrary = oneof [ NormalC <$> arbitrary <*> arbitrary
                      , RecC    <$> arbitrary <*> arbitrary
                      , InfixC  <$> arbitrary <*> arbitrary <*> arbitrary
                      , ForallC <$> arbitrary <*> arbitrary <*> arbitrary
                      ]

instance Arbitrary Dec where
    arbitrary = oneof [
          FunD               <$> arbitrary <*> arbitrary
        , ValD               <$> arbitrary <*> arbitrary <*> arbitrary
        , DataD              <$> arbitrary <*> arbitrary <*> arbitrary
                             <*> arbitrary <*> arbitrary
        , NewtypeD           <$> arbitrary <*> arbitrary <*> arbitrary
                             <*> arbitrary <*> arbitrary
        , TySynD             <$> arbitrary <*> arbitrary <*> arbitrary
        , ClassD             <$> arbitrary <*> arbitrary <*> arbitrary
                             <*> arbitrary <*> arbitrary
        , InstanceD          <$> arbitrary <*> arbitrary <*> arbitrary
        , SigD               <$> arbitrary <*> arbitrary
        , ForeignD           <$> arbitrary
        , PragmaD            <$> arbitrary
        , FamilyD            <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , DataInstD          <$> arbitrary <*> arbitrary <*> arbitrary
                             <*> arbitrary <*> arbitrary
        , NewtypeInstD       <$> arbitrary <*> arbitrary <*> arbitrary
                             <*> arbitrary <*> arbitrary
#if MIN_VERSION_template_haskell(2,8,0)
        , InfixD             <$> arbitrary <*> arbitrary
#endif
#if MIN_VERSION_template_haskell(2,9,0)
        , ClosedTypeFamilyD  <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , RoleAnnotD         <$> arbitrary <*> arbitrary
        , TySynInstD         <$> arbitrary <*> arbitrary
#else
        , TySynInstD         <$> arbitrary <*> arbitrary <*> arbitrary
#endif
#if MIN_VERSION_template_haskell(2,10,0)
        , StandaloneDeriving <$> arbitrary <*> arbitrary
        , DefaultSigD        <$> arbitrary <*> arbitrary
#endif
        ]

instance Arbitrary Exp where
    arbitrary = oneof [ VarE        <$> arbitrary
                      , ConE        <$> arbitrary
                      , LitE        <$> arbitrary
                      , AppE        <$> arbitrary <*> arbitrary
                      , InfixE      <$> arbitrary <*> arbitrary <*> arbitrary
                      , LamE        <$> arbitrary <*> arbitrary
                      , TupE        <$> arbitrary
                      , CondE       <$> arbitrary <*> arbitrary <*> arbitrary
                      , LetE        <$> arbitrary <*> arbitrary
                      , CaseE       <$> arbitrary <*> arbitrary
                      , DoE         <$> arbitrary
                      , CompE       <$> arbitrary
                      , ArithSeqE   <$> arbitrary
                      , ListE       <$> arbitrary
                      , SigE        <$> arbitrary <*> arbitrary
                      , RecConE     <$> arbitrary <*> arbitrary
                      , RecUpdE     <$> arbitrary <*> arbitrary
#if MIN_VERSION_template_haskell(2,6,0)
                      , UnboxedTupE <$> arbitrary
#endif
#if MIN_VERSION_template_haskell(2,7,0)
                      , UInfixE     <$> arbitrary <*> arbitrary <*> arbitrary
                      , ParensE     <$> arbitrary
#endif
#if MIN_VERSION_template_haskell(2,8,0)
                      , LamCaseE    <$> arbitrary
                      , MultiIfE    <$> arbitrary
#endif
#if MIN_VERSION_template_haskell(2,10,0)
                      , StaticE     <$> arbitrary
#endif
                      ]

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
    arbitrary = oneof [NormalG <$> arbitrary, PatG <$> arbitrary]

instance Arbitrary Info where
    arbitrary = oneof [
#if MIN_VERSION_template_haskell(2,5,0)
          ClassI     <$> arbitrary <*> arbitrary
#else
          ClassI     <$> arbitrary
#endif
        , ClassOpI   <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , TyConI     <$> arbitrary
        , PrimTyConI <$> arbitrary <*> arbitrary <*> arbitrary
        , DataConI   <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , VarI       <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , TyVarI     <$> arbitrary <*> arbitrary
#if MIN_VERSION_template_haskell(2,7,0)
        , FamilyI    <$> arbitrary <*> arbitrary
#endif
        ]

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
    arbitrary = Match <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Name where
    arbitrary = Name <$> arbitrary <*> arbitrary

instance Arbitrary NameFlavour where
    arbitrary = oneof [ pure NameS
                      , NameQ <$> arbitrary
                      , (\(I# i#) -> NameU i#) <$> arbitrary
                      , (\(I# i#) -> NameL i#) <$> arbitrary
                      , NameG <$> arbitrary <*> arbitrary <*> arbitrary
                      ]

instance Arbitrary NameIs where
    arbitrary = oneof $ map pure [Alone, Applied, Infix]

deriving instance Show NameIs

instance Arbitrary NameSpace where
    arbitrary = oneof $ map pure [VarName, DataName, TcClsName]

instance Arbitrary Pat where
    arbitrary = oneof [ LitP          <$> arbitrary
                      , VarP          <$> arbitrary
                      , TupP          <$> arbitrary
                      , ConP          <$> arbitrary <*> arbitrary
                      , InfixP        <$> arbitrary <*> arbitrary <*> arbitrary
                      , TildeP        <$> arbitrary
                      , BangP         <$> arbitrary
                      , AsP           <$> arbitrary <*> arbitrary
                      , pure WildP
                      , RecP          <$> arbitrary <*> arbitrary
                      , ListP         <$> arbitrary
                      , SigP          <$> arbitrary <*> arbitrary
#if MIN_VERSION_template_haskell(2,5,0)
                      , ViewP         <$> arbitrary <*> arbitrary
#endif
#if MIN_VERSION_template_haskell(2,6,0)
                      , UnboxedTupP <$> arbitrary
#endif
#if MIN_VERSION_template_haskell(2,7,0)
                      , UInfixP       <$> arbitrary <*> arbitrary <*> arbitrary
                      , ParensP       <$> arbitrary
#endif
                      ]

instance Arbitrary Pragma where
    arbitrary = oneof
        [
#if MIN_VERSION_template_haskell(2,8,0)
          InlineP         <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , SpecialiseP     <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , SpecialiseInstP <$> arbitrary
        , RuleP           <$> arbitrary <*> arbitrary <*> arbitrary
                          <*> arbitrary <*> arbitrary
        , AnnP            <$> arbitrary <*> arbitrary
# if MIN_VERSION_template_haskell(2,10,0)
        , LineP           <$> arbitrary <*> arbitrary
# endif
#else
          InlineP     <$> arbitrary <*> arbitrary
        , SpecialiseP <$> arbitrary <*> arbitrary <*> arbitrary
#endif
        ]

instance Arbitrary Range where
    arbitrary = oneof [ FromR       <$> arbitrary
                      , FromThenR   <$> arbitrary <*> arbitrary
                      , FromToR     <$> arbitrary <*> arbitrary
                      , FromThenToR <$> arbitrary <*> arbitrary <*> arbitrary
                      ]

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
    arbitrary = oneof [ BindS   <$> arbitrary <*> arbitrary
                      , LetS    <$> arbitrary
                      , NoBindS <$> arbitrary
                      , ParS    <$> arbitrary
                      ]

instance Arbitrary Strict where
    arbitrary = oneof $ map pure [ IsStrict
                                 , NotStrict
#if MIN_VERSION_template_haskell(2,7,0)
                                 , Unpacked
#endif
                                 ]

instance Arbitrary Type where
    arbitrary = oneof [ ForallT        <$> arbitrary <*> arbitrary <*> arbitrary
                      , VarT           <$> arbitrary
                      , ConT           <$> arbitrary
                      , TupleT         <$> arbitrary
                      , pure ArrowT
                      , pure ListT
                      , AppT           <$> arbitrary <*> arbitrary
                      , SigT           <$> arbitrary <*> arbitrary
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

instance Arbitrary TyVarBndr where
    arbitrary = oneof [PlainTV <$> arbitrary, KindedTV <$> arbitrary <*> arbitrary]

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
    arbitrary = oneof [pure StarK, ArrowK <$> arbitrary <*> arbitrary]
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
    arbitrary = oneof [ ClassP <$> arbitrary <*> arbitrary
                      , EqualP <$> arbitrary <*> arbitrary
                      ]
#endif

deriving instance Arbitrary ModName
deriving instance Arbitrary OccName
deriving instance Arbitrary PkgName