{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module TypecheckerCombinators.Unifix
  ( Unifix(..)
  ) where

import Data.UnionFind.IntMap qualified as UnionFind

import TypecheckerCombinators.Elem
import TypecheckerCombinators.Fix


data Unifix tpF
  = UnifixV (UnionFind.Point Int)
  | UnifixF (tpF (Unifix tpF))

instance Roll Unifix where
  mkFix
    = UnifixF . inj
  matchFix (UnifixF fsUnifix) = do
    pure fsUnifix
  matchFix _ = do
    Nothing
