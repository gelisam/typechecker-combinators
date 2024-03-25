{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Typechecker.Core
  ( Check
  , Infer
  , TypeChecker
  , checkWithTypeChecker
  , inferWithTypeChecker
  , (<+>)
  , checker
  , inferer
  ) where

import Control.Applicative (empty)
import Control.Monad.Trans.Maybe (MaybeT)

import Typechecker.Fix
import Typechecker.MonadEq
import Typechecker.Sum


type Check term tp m
  = term -> tp -> MaybeT m ()
type Infer term tp m
  = term -> MaybeT m tp

data TypeChecker exprF tp m = TypeChecker
  { mkCheck
      :: forall r
       . Check r tp m
      -> Infer r tp m
      -> Check (exprF r) tp m
  , mkInfer
      :: forall r
       . Check r tp m
      -> Infer r tp m
      -> Infer (exprF r) tp m
  }

checkInferWithTypeChecker
  :: TypeChecker exprF tp m
  -> ( Check (Fix exprF) tp m
     , Infer (Fix exprF) tp m
     )
checkInferWithTypeChecker tc = (checkR, inferR)
  where
    checkR (Fix fFix)
      = mkCheck tc checkR inferR fFix
    inferR (Fix fFix)
      = mkInfer tc checkR inferR fFix

checkWithTypeChecker
  :: TypeChecker exprF tp m
  -> Check (Fix exprF) tp m
checkWithTypeChecker = fst . checkInferWithTypeChecker

inferWithTypeChecker
  :: TypeChecker exprF tp m
  -> Infer (Fix exprF) tp m
inferWithTypeChecker = snd . checkInferWithTypeChecker

(<+>)
  :: forall exprF exprG tp m
   . TypeChecker exprF tp m
  -> TypeChecker exprG tp m
  -> TypeChecker (exprF + exprG) tp m
tcF <+> tcG = TypeChecker mkCheckFG mkInferFG
  where
    mkCheckFG
      :: Check r tp m
      -> Infer r tp m
      -> Check ((+) exprF exprG r) tp m
    mkCheckFG checkR inferR (InL fFix) tp
      = mkCheck tcF checkR inferR fFix tp
    mkCheckFG checkR inferR (InR gFix) tp
      = mkCheck tcG checkR inferR gFix tp

    mkInferFG
      :: Check r tp m
      -> Infer r tp m
      -> Infer ((+) exprF exprG r) tp m
    mkInferFG checkR inferR (InL fFix)
      = mkInfer tcF checkR inferR fFix
    mkInferFG checkR inferR (InR gFix)
      = mkInfer tcG checkR inferR gFix

checker
  :: forall exprF tp m. Monad m
  => ( forall r
     . Check r tp m
    -> Infer r tp m
    -> Check (exprF r) tp m
     )
  -> TypeChecker exprF tp m
checker mkCheckF = TypeChecker mkCheckF mkInferF
  where
    mkInferF
      :: Check r tp m
      -> Infer r tp m
      -> Infer (exprF r) tp m
    mkInferF _checkR _inferR _fR = do
      empty

inferer
  :: forall exprF tp m. MonadEq tp m
  => ( forall r
     . Check r tp m
    -> Infer r tp m
    -> Infer (exprF r) tp m
     )
  -> TypeChecker exprF tp m
inferer mkInferF = TypeChecker mkCheckF mkInferF
  where
    mkCheckF
      :: Check r tp m
      -> Infer r tp m
      -> Check (exprF r) tp m
    mkCheckF checkR inferR fR expected = do
      actual <- mkInferF checkR inferR fR
      assertEq actual expected