{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Typechecker.Terms where

import Typechecker.Fix


newtype Var term = Var String

data App term = App term term

data Lam term = Lam String term

data Zero term = Zero

data Succ term = Succ term


deriving instance Show term => Show (Var term)

deriving instance Show term => Show (App term)

deriving instance Show term => Show (Lam term)


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