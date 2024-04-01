{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Typechecker.Unifix
  ( Unifix(..)
  ) where

import Data.UnionFind.IntMap qualified as UnionFind

import Typechecker.Elem
import Typechecker.Fix


data Unifix tpF
  = UnifixV (UnionFind.Point Int)
  | UnifixF (tpF (Unifix tpF))

instance Roll Unifix where
  mkFix
    = UnifixF . inj
  unFix (UnifixF fsUnifix) = do
    pure fsUnifix
  unFix _ = do
    Nothing
