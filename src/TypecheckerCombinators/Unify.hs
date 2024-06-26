{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module TypecheckerCombinators.Unify
  ( Match
  , UnificationState
  , HasUnificationState
  , MonadUnification
  , emptyUnificationState
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

import TypecheckerCombinators.Fix
import TypecheckerCombinators.Match
import TypecheckerCombinators.Unifix

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

class HasUnificationState s tpF | s -> tpF where
  unificationState :: Lens' s (UnificationState tpF)

type MonadUnification s tpF m = (MonadState s m, HasUnificationState s tpF)

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
  :: MonadUnification s tpF m
  => UnionFind.Point Int
  -> UnionFind.Point Int
  -> m Bool
eqV pt1 pt2 = do
  pointSupply <- use (unificationState . unificationState_PointSupply)
  pure $ UnionFind.equivalent pointSupply pt1 pt2

newUnivar
  :: MonadUnification s tpF m
  => m (Unifix tpF)
newUnivar = do
  pointSupply <- use (unificationState . unificationState_PointSupply)
  nextUnivar <- use (unificationState . unificationState_NextUnivar)
  let (pointSupply', pt) = UnionFind.fresh pointSupply nextUnivar
  unificationState . unificationState_PointSupply .= pointSupply'
  unificationState . unificationState_NextUnivar += 1
  pure $ UnifixV pt

-- make sure the outer layer is a UnifixF if possible,
-- and the representative variable otherwise.
zonk1
  :: MonadUnification s tpF m
  => Unifix tpF
  -> m (Unifix tpF)
zonk1 (UnifixV pt) = do
  pointSupply <- use (unificationState . unificationState_PointSupply)
  contents <- use (unificationState . unificationState_Contents)
  let i = UnionFind.descriptor pointSupply pt
  case IntMap.lookup i contents of
    Nothing -> do
      pure $ UnifixV $ UnionFind.repr pointSupply pt
    Just fX -> do
      pure $ UnifixF fX
zonk1 (UnifixF fX) = do
  pure $ UnifixF fX

-- expand all variables to UnifixF when possible,
-- and use the representative variable otherwise.
zonk
  :: (MonadUnification s tpF m, Traversable tpF)
  => Unifix tpF
  -> m (Unifix tpF)
zonk x = do
  zonk1 x >>= \case
    UnifixV repr -> do
      pure $ UnifixV repr
    UnifixF fX -> do
      UnifixF <$> traverse zonk fX

noUnivars
  :: forall s m tpF. (MonadUnification s tpF m, Traversable tpF)
  => Unifix tpF
  -> MaybeT m (Fix tpF)
noUnivars x0 = MaybeT (go <$> zonk x0)
  where
    go
      :: Unifix tpF
      -> Maybe (Fix tpF)
    go (UnifixV _) = do
      Nothing
    go (UnifixF fX) = do
      Fix <$> traverse go fX

occursIn
  :: forall s m tpF. (MonadUnification s tpF m, Traversable tpF)
  => UnionFind.Point Int
  -> Unifix tpF
  -> m Bool
occursIn pt0 x0 = do
  pointSupply <- use (unificationState . unificationState_PointSupply)
  x <- zonk x0
  go (UnionFind.repr pointSupply pt0) x
  where
    go
      :: UnionFind.Point Int
      -> Unifix tpF
      -> m Bool
    go needle (UnifixV v) = do
      eqV needle v
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
  :: forall s m tpF. (MonadUnification s tpF m, Match tpF)
  => Unifix tpF
  -> Unifix tpF
  -> MaybeT m ()
unify = unifyXX
  where
    unifyXX
      :: Unifix tpF
      -> Unifix tpF
      -> MaybeT m ()
    unifyXX x1 x2 = do
      z1 <- zonk x1
      z2 <- zonk x2
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
      pointSupply <- use (unificationState . unificationState_PointSupply)
      let pointSupply' = UnionFind.union pointSupply repr1 repr2
      unificationState . unificationState_PointSupply .= pointSupply'

    unifyRF
      :: UnionFind.Point Int
      -> tpF (Unifix tpF)
      -> MaybeT m ()
    unifyRF pt fX = do
      bad <- occursIn pt (UnifixF fX)
      guard (not bad)
      pointSupply <- use (unificationState . unificationState_PointSupply)
      let i = UnionFind.descriptor pointSupply pt
      unificationState . unificationState_Contents %= IntMap.insert i fX

    unifyFF
      :: tpF (Unifix tpF)
      -> tpF (Unifix tpF)
      -> MaybeT m ()
    unifyFF fX1 fX2 = do
      maybeUnit <- matchM fX1 fX2 $ \fX1' fX2' -> do
        for_ ((,) <$> fX1' <*> fX2') $ \(x1, x2) -> do
          unifyXX x1 x2
      MaybeT $ pure maybeUnit
