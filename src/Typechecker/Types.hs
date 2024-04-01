{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Typechecker.Types where

import Typechecker.Elem
import Typechecker.Fix


data Arr ty = Arr ty ty

data Nat ty = Nat

data Str ty = Str


deriving instance Eq term => Eq (Arr term)

deriving instance Eq (Nat term)

deriving instance Eq (Str term)


deriving instance Show term => Show (Arr term)

deriving instance Show (Nat term)

deriving instance Show (Str term)


deriving instance Functor Arr

deriving instance Functor Nat

deriving instance Functor Str


instance Applicative Arr where
  pure x
    = Arr x x
  Arr f1 f2 <*> Arr x1 x2
    = Arr (f1 x1) (f2 x2)

instance Applicative Nat where
  pure _
    = Nat
  Nat <*> Nat
    = Nat

instance Applicative Str where
  pure _
    = Str
  Str <*> Str
    = Str


deriving instance Foldable Arr

deriving instance Foldable Nat

deriving instance Foldable Str


deriving instance Traversable Arr

deriving instance Traversable Nat

deriving instance Traversable Str


arr
  :: (Roll fix, Elem Arr tpF)
  => fix tpF
  -> fix tpF
  -> fix tpF
arr x y = roll $ Arr x y

nat
  :: (Roll fix, Elem Nat tpF)
  => fix tpF
nat = roll Nat

str
  :: (Roll fix, Elem Str tpF)
  => fix tpF
str = roll Str