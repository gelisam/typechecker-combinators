{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module TypecheckerCombinators.Terms where

import TypecheckerCombinators.Elem
import TypecheckerCombinators.Fix


newtype Var term = Var String

data App term = App term term

data Lam term = Lam String term

newtype NatLit term = NatLit Int

newtype StrLit term = StrLit String

data Plus term = Plus term term

data Len term = Len term


deriving instance Show (Var term)

deriving instance Show term => Show (App term)

deriving instance Show term => Show (Lam term)

deriving instance Show (NatLit term)

deriving instance Show (StrLit term)

deriving instance Show term => Show (Plus term)

deriving instance Show term => Show (Len term)


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

natLit
  :: Elem NatLit exprF
  => Int
  -> Fix exprF
natLit x = roll $ NatLit x

strLit
  :: Elem StrLit exprF
  => String
  -> Fix exprF
strLit x = roll $ StrLit x

(+)
  :: Elem Plus exprF
  => Fix exprF
  -> Fix exprF
  -> Fix exprF
(+) x y = roll $ Plus x y

len
  :: Elem Len exprF
  => Fix exprF
  -> Fix exprF
len x = roll $ Len x
