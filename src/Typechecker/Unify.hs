{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Typechecker.Unify
  ( UnificationState
  , emptyUnificationState
  , WhichUnificationState
  , Unifix
  , newUnivar
  , noUnivars
  , unify
  ) where

import Control.Lens (Lens', use, (.=), (+=), (%=), makeLenses)
import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import Control.Monad.State (MonadState)
import Data.Foldable (for_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.UnionFind.IntMap qualified as UnionFind

import Typechecker.Fix
import Typechecker.Match
import Typechecker.Unifix

-- $setup
-- >>> :set -XTypeOperators
-- >>> import Control.Monad.Trans.Maybe (runMaybeT)


data UnificationState tpF = UnificationState
  { _unificationState_PointSupply
      :: !(UnionFind.PointSupply Int)
  , _unificationState_NextUnivar
      :: !Int
  , _unificationState_Contents
      :: !(IntMap (tpF (Unifix tpF)))
  }
makeLenses ''UnificationState

type WhichUnificationState s tpF = Lens' s (UnificationState tpF)

emptyUnificationState
  :: UnificationState tpF
emptyUnificationState = UnificationState
  { _unificationState_PointSupply
      = UnionFind.newPointSupply
  , _unificationState_NextUnivar
      = 0
  , _unificationState_Contents
      = IntMap.empty
  }

eqV
  :: MonadState s m
  => WhichUnificationState s tpF
  -> UnionFind.Point Int
  -> UnionFind.Point Int
  -> m Bool
eqV unificationState pt1 pt2 = do
  pointSupply <- use (unificationState . unificationState_PointSupply)
  pure $ UnionFind.equivalent pointSupply pt1 pt2

newUnivar
  :: MonadState s m
  => WhichUnificationState s tpF
  -> m (Unifix tpF)
newUnivar unificationState = do
  pointSupply <- use (unificationState . unificationState_PointSupply)
  nextUnivar <- use (unificationState . unificationState_NextUnivar)
  let (pointSupply', pt) = UnionFind.fresh pointSupply nextUnivar
  unificationState . unificationState_PointSupply .= pointSupply'
  unificationState . unificationState_NextUnivar += 1
  pure $ UnifixV pt

-- make sure the outer layer is a UnifixF if possible,
-- and the representative variable otherwise.
zonk1
  :: MonadState s m
  => WhichUnificationState s tpF
  -> Unifix tpF
  -> m (Unifix tpF)
zonk1 unificationState (UnifixV pt) = do
  pointSupply <- use (unificationState . unificationState_PointSupply)
  contents <- use (unificationState . unificationState_Contents)
  let i = UnionFind.descriptor pointSupply pt
  case IntMap.lookup i contents of
    Nothing -> do
      pure $ UnifixV $ UnionFind.repr pointSupply pt
    Just fX -> do
      pure $ UnifixF fX
zonk1 _whichUnificationState (UnifixF fX) = do
  pure $ UnifixF fX

-- expand all variables to UnifixF when possible,
-- and use the representative variable otherwise.
zonk
  :: (MonadState s m, Traversable tpF)
  => WhichUnificationState s tpF
  -> Unifix tpF
  -> m (Unifix tpF)
zonk unificationState x = do
  zonk1 unificationState x >>= \case
    UnifixV repr -> do
      pure $ UnifixV repr
    UnifixF fX -> do
      UnifixF <$> traverse (zonk unificationState) fX

noUnivars
  :: forall s m tpF. (MonadState s m, Traversable tpF)
  => WhichUnificationState s tpF
  -> Unifix tpF
  -> MaybeT m (Fix tpF)
noUnivars unificationState x0 = MaybeT (go <$> zonk unificationState x0)
  where
    go
      :: Unifix tpF
      -> Maybe (Fix tpF)
    go (UnifixV _) = do
      Nothing
    go (UnifixF fX) = do
      Fix <$> traverse go fX

occursIn
  :: forall s m tpF. (MonadState s m, Traversable tpF)
  => WhichUnificationState s tpF
  -> UnionFind.Point Int
  -> Unifix tpF
  -> m Bool
occursIn unificationState pt0 x0 = do
  pointSupply <- use (unificationState . unificationState_PointSupply)
  x <- zonk unificationState x0
  go (UnionFind.repr pointSupply pt0) x
  where
    go
      :: UnionFind.Point Int
      -> Unifix tpF
      -> m Bool
    go needle (UnifixV v) = do
      eqV unificationState needle v
    go needle (UnifixF fX) = do
      or <$> traverse (go needle) fX

-- |
-- >>> type UniTp = Unifix (Arr + Nat)
--
-- >>> lhs <- newUnivar :: IO UniTp
-- >>> rhs <- newUnivar :: IO UniTp
-- >>> tp1 = arr lhs nat :: UniTp
-- >>> tp2 = arr nat rhs :: UniTp
-- >>> runMaybeT $ unify tp1 tp2
-- Just ()
-- >>> runMaybeT $ noUnivars tp1
-- Just (Arr Nat Nat)
--
-- >>> a <- newUnivar :: IO UniTp
-- >>> b <- newUnivar :: IO UniTp
-- >>> tp1 = arr a a :: UniTp
-- >>> tp2 = arr nat b :: UniTp
-- >>> runMaybeT $ unify tp1 tp2
-- Just ()
-- >>> runMaybeT $ noUnivars tp1
-- Just (Arr Nat Nat)
--
-- >>> a <- newUnivar :: IO UniTp
-- >>> b <- newUnivar :: IO UniTp
-- >>> tp1 = arr a a :: UniTp
-- >>> tp2 = arr nat tp1 :: UniTp
-- >>> runMaybeT $ unify tp1 tp2
-- Nothing
unify
  :: forall s m tpF. (MonadState s m, Match tpF)
  => WhichUnificationState s tpF
  -> Unifix tpF
  -> Unifix tpF
  -> MaybeT m ()
unify whichUnificationState = unifyXX
  where
    unifyXX
      :: Unifix tpF
      -> Unifix tpF
      -> MaybeT m ()
    unifyXX x1 x2 = do
      z1 <- zonk whichUnificationState x1
      z2 <- zonk whichUnificationState x2
      unifyZZ z1 z2

    unifyZZ
      :: Unifix tpF
      -> Unifix tpF
      -> MaybeT m ()
    unifyZZ (UnifixV repr1) (UnifixV repr2) = do
      unifyRR repr1 repr2
    unifyZZ (UnifixV repr) (UnifixF fX) = do
      unifyRF repr fX
    unifyZZ (UnifixF fX) (UnifixV repr) = do
      unifyRF repr fX
    unifyZZ (UnifixF fX1) (UnifixF fX2) = do
      unifyFF fX1 fX2

    unifyRR
      :: UnionFind.Point Int
      -> UnionFind.Point Int
      -> MaybeT m ()
    unifyRR repr1 repr2 = do
      pointSupply <- use (whichUnificationState . unificationState_PointSupply)
      let pointSupply' = UnionFind.union pointSupply repr1 repr2
      whichUnificationState . unificationState_PointSupply .= pointSupply'

    unifyRF
      :: UnionFind.Point Int
      -> tpF (Unifix tpF)
      -> MaybeT m ()
    unifyRF pt fX = do
      bad <- occursIn whichUnificationState pt (UnifixF fX)
      guard (not bad)
      pointSupply <- use (whichUnificationState . unificationState_PointSupply)
      let i = UnionFind.descriptor pointSupply pt
      whichUnificationState . unificationState_Contents %= IntMap.insert i fX

    unifyFF
      :: tpF (Unifix tpF)
      -> tpF (Unifix tpF)
      -> MaybeT m ()
    unifyFF fX1 fX2 = do
      maybeUnit <- matchM fX1 fX2 $ \fX1' fX2' -> do
        for_ ((,) <$> fX1' <*> fX2') $ \(x1, x2) -> do
          unifyXX x1 x2
      MaybeT $ pure maybeUnit
