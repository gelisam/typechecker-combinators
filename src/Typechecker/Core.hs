{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Typechecker.Core
  ( Check
  , Infer
  , TypeChecker
  , check
  , infer
  , combine
  , checker
  , inferer
  , unifier
  ) where

import Control.Applicative (empty)
import Control.Monad (guard)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Maybe (MaybeT)

import Typechecker.Fix
import Typechecker.Match
import Typechecker.Sum
import Typechecker.Unifix


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

checkInfer
  :: TypeChecker exprF tp m
  -> ( Check (Fix exprF) tp m
     , Infer (Fix exprF) tp m
     )
checkInfer tc = (checkR, inferR)
  where
    checkR (Fix fFix) tp
      = mkCheck tc checkR inferR fFix tp
    inferR (Fix fFix)
      = mkInfer tc checkR inferR fFix

check
  :: TypeChecker exprF tp m
  -> Check (Fix exprF) tp m
check = fst . checkInfer

infer
  :: TypeChecker exprF tp m
  -> Infer (Fix exprF) tp m
infer = snd . checkInfer

combine
  :: forall exprF exprG tp m
   . TypeChecker exprF tp m
  -> TypeChecker exprG tp m
  -> TypeChecker (exprF + exprG) tp m
combine tcF tcG = TypeChecker mkCheckFG mkInferFG
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
  :: forall exprF tpF m. (Eq (Fix tpF), Monad m)
  => ( forall r
     . Check r (Fix tpF) m
    -> Infer r (Fix tpF) m
    -> Infer (exprF r) (Fix tpF) m
     )
  -> TypeChecker exprF (Fix tpF) m
inferer mkInferF = TypeChecker mkCheckF mkInferF
  where
    mkCheckF
      :: Check r (Fix tpF) m
      -> Infer r (Fix tpF) m
      -> Check (exprF r) (Fix tpF) m
    mkCheckF checkR inferR fR expected = do
      actual <- mkInferF checkR inferR fR
      guard (actual == expected)

unifier
    :: forall exprF tpF s m. (Match tpF, MonadState s m)
  => WhichUnificationState s tpF
  -> ( forall r
     . Check r (Unifix tpF) m
    -> Infer r (Unifix tpF) m
    -> Infer (exprF r) (Unifix tpF) m
     )
  -> TypeChecker exprF (Unifix tpF) m
unifier unificationState mkInferF = TypeChecker mkCheckF mkInferF
  where
    mkCheckF
      :: Check r (Unifix tpF) m
      -> Infer r (Unifix tpF) m
      -> Check (exprF r) (Unifix tpF) m
    mkCheckF checkR inferR fR expected = do
      actual <- mkInferF checkR inferR fR
      unify unificationState actual expected