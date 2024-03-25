{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Typechecker.MonadEq where

import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT)

import Typechecker.Fix
import Typechecker.Unify


class Monad m => MonadEq tp m where
  assertEq :: tp -> tp -> MaybeT m ()

instance (Eq (Fix tpF), Monad m)
      => MonadEq (Fix tpF) m
         where
  assertEq tp1 tp2 = do
    guard (tp1 == tp2)

instance (Match tpF, MonadUnification s tpF m)
      => MonadEq (Unifix tpF) m
         where
  assertEq tp1 tp2 = do
    unify tp1 tp2