{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Typechecker.Terms where

import Typechecker.Sum
import Typechecker.Fix


newtype Var term = Var String

data App term = App term term

data Lam term = Lam String term

data Zero term = Zero

newtype Succ term = Succ term

newtype NatLit term = NatLit Int

data Plus term = Plus term term


deriving instance Show (Var term)

deriving instance Show term => Show (App term)

deriving instance Show term => Show (Lam term)

deriving instance Show (Zero term)

deriving instance Show term => Show (Succ term)

deriving instance Show (NatLit term)

deriving instance Show term => Show (Plus term)


var
  :: Elem Var exprF
  => String
  -> Fix exprF
var x = roll $ Var x

app
  :: Elem App exprF
  => Fix exprF
  -> Fix exprF
  -> Fix exprF
app x y = roll $ App x y

lam
  :: Elem Lam exprF
  => String -> Fix exprF
  -> Fix exprF
lam x y = roll $ Lam x y

zero
  :: Elem Zero exprF
  => Fix exprF
zero = roll Zero

succ
  :: Elem Succ exprF
  => Fix exprF
  -> Fix exprF
succ x = roll $ Succ x

natLit
  :: Elem NatLit exprF
  => Int
  -> Fix exprF
natLit x = roll $ NatLit x

(+)
  :: Elem Plus exprF
  => Fix exprF
  -> Fix exprF
  -> Fix exprF
(+) x y = roll $ Plus x y