{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
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
  ) where

import Control.Applicative (Alternative(empty))
import Control.Monad (guard)

import Typechecker.Fix


type Check term tp m
  = term -> tp -> m ()
type Infer term tp m
  = term -> m tp

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
  :: forall exprF tp m. Alternative m
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
  :: forall exprF tp m. (Eq tp, Monad m, Alternative m)
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
      guard (actual == expected)