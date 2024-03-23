{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Typechecker.Types where

import Typechecker.Sum
import Typechecker.Unifix


data Arr ty = Arr ty ty

data Nat ty = Nat


deriving instance Show term => Show (Arr term)

deriving instance Show (Nat term)


deriving instance Functor Arr

deriving instance Functor Nat


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


deriving instance Foldable Arr

deriving instance Foldable Nat


deriving instance Traversable Arr

deriving instance Traversable Nat


arr
  :: Elem Arr tpF
  => Unifix tpF
  -> Unifix tpF
  -> Unifix tpF
arr x y = uniroll $ Arr x y

nat
  :: Elem Nat tpF
  => Unifix tpF
nat = uniroll Nat