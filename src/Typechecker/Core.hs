{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Typechecker.Core
  ( Handle(check, infer)
  , TypeChecker
  , runTypeChecker
  , (<+>)
  , checked
  , infered
  ) where

import Control.Applicative (empty)
import Control.Monad.Trans.Maybe (MaybeT)

import Typechecker.Elem
import Typechecker.Fix
import Typechecker.MonadEq


data Handle term tp m = Handle
  { check
      :: term -> tp -> MaybeT m ()
  , infer
      :: term -> MaybeT m tp
  }

contramap
  :: (term' -> term)
  -> Handle term tp m
  -> Handle term' tp m
contramap f h = Handle
  { check = \term' tp -> do
      check h (f term') tp
  , infer = \term' -> do
      infer h (f term')
  }

newtype TypeChecker exprF tp m = TypeChecker
  { unTypeChecker
      :: forall r
       . Handle r tp m
      -> Handle (exprF r) tp m
  }

runTypeChecker
  :: forall exprF tp m
   . TypeChecker exprF tp m
  -> Handle (Fix exprF) tp m
runTypeChecker tc = handleFix
  where
    handleFFix
      :: Handle (exprF (Fix exprF)) tp m
    handleFFix
      = unTypeChecker tc handleFix

    handleFix
      :: Handle (Fix exprF) tp m
    handleFix
      = contramap unFix handleFFix

(<+>)
  :: forall exprF exprG tp m
   . TypeChecker exprF tp m
  -> TypeChecker exprG tp m
  -> TypeChecker (exprF + exprG) tp m
tcF <+> tcG = TypeChecker $ \handleR -> Handle
  { check = \case
      InL fFix -> check (unTypeChecker tcF handleR) fFix
      InR gFix -> check (unTypeChecker tcG handleR) gFix
  , infer = \case
      InL fFix -> infer (unTypeChecker tcF handleR) fFix
      InR gFix -> infer (unTypeChecker tcG handleR) gFix
  }

checked
  :: forall exprF tp m. Monad m
  => ( forall r
     . Handle r tp m
    -> exprF r
    -> tp
    -> MaybeT m ()
     )
  -> TypeChecker exprF tp m
checked checkF = TypeChecker $ \handleR -> Handle
  { check = checkF handleR
  , infer = const empty
  }

infered
  :: forall exprF tp m. MonadEq tp m
  => ( forall r
     . Handle r tp m
    -> exprF r
    -> MaybeT m tp
     )
  -> TypeChecker exprF tp m
infered inferF = TypeChecker $ \handleR -> Handle
  { check = \fR expected -> do
      actual <- inferF handleR fR
      assertEq actual expected
  , infer = inferF handleR
  }