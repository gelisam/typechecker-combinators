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

import Typechecker.Fix
import Typechecker.Sum


data Unifix tpF
  = UnifixV (UnionFind.Point Int)
  | UnifixF (tpF (Unifix tpF))

instance Roll Unifix where
  roll
    = UnifixF . inj
  unroll (UnifixF fsUnifix)
    = prj fsUnifix
  unroll _
    = Nothing
