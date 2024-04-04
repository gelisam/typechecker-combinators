{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Typechecker.Handle
  ( Handle(Handle, check, infer)
  , contramap
  , (<+>)
  ) where

import Control.Monad.Trans.Maybe (MaybeT)

import Typechecker.Sum


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

(<+>)
  :: forall exprF exprG r tp m
   . Handle (exprF r) tp m
  -> Handle (exprG r) tp m
  -> Handle ((+) exprF exprG r) tp m
hF <+> hG = Handle
  { check = \case
      InL f -> check hF f
      InR g -> check hG g
  , infer = \case
      InL f -> infer hF f
      InR g -> infer hG g
  }