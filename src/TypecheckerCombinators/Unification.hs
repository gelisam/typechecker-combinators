{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module TypecheckerCombinators.Unification where

import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import Data.Foldable (for_)
import Data.UnionFind.IO qualified as UnionFind

import TypecheckerCombinators.Fix

-- $setup
-- >>> :set -XTypeOperators
-- >>> import Control.Monad.Trans.Maybe (runMaybeT)


class Match fs where
  match
    :: fs a
    -> fs a
    -> ( forall f. (Applicative f, Traversable f)
      => f a
      -> f a
      -> r
       )
    -> Maybe r

instance ( Match fs
         , Match gs
         )
        => Match (fs + gs)
           where
  match (InL fs1) (InL fs2) cc
    = match fs1 fs2 cc
  match (InR gs1) (InR gs2) cc
    = match gs1 gs2 cc
  match _ _ _
    = Nothing

instance {-# OVERLAPPABLE #-}
         ( Applicative f
         , Traversable f
         )
        => Match f
           where
  match f1 f2 cc
    = Just (cc f1 f2)

matchM
  :: (Monad m, Match fs)
  => fs a
  -> fs a
  -> ( forall f. (Applicative f, Traversable f)
    => f a
    -> f a
    -> m r
     )
  -> m (Maybe r)
matchM fs1 fs2 cc = do
  case match fs1 fs2 cc of
    Nothing -> do
      pure Nothing
    Just mr -> do
      r <- mr
      pure $ Just r


newtype Univar tpF = Univar
  { unUnivar :: UnionFind.Point (Maybe (tpF (Unifix tpF)))
  }

eqV
  :: Univar tpF
  -> Univar tpF
  -> IO Bool
eqV (Univar pt1) (Univar pt2) = do
  UnionFind.equivalent pt1 pt2

data Unifix tpF
  = UnifixV (Univar tpF)
  | UnifixF (tpF (Unifix tpF))

newUnivar
  :: IO (Unifix tpF)
newUnivar = do
  UnifixV <$> Univar <$> UnionFind.fresh Nothing

uniroll :: Elem f fs => f (Unifix fs) -> Unifix fs
uniroll = UnifixF . inj

-- make sure the outer layer is a UnifixF if possible,
-- and the representative variable otherwise.
zonk1
  :: Functor tpF
  => Unifix tpF
  -> IO (Unifix tpF)
zonk1 (UnifixV (Univar pt)) = do
  UnionFind.descriptor pt >>= \case
    Nothing -> do
      repr <- Univar <$> UnionFind.repr pt
      pure $ UnifixV repr
    Just fX -> do
      pure $ UnifixF fX
zonk1 (UnifixF fX) = do
  pure $ UnifixF fX

-- expand all variables to UnifixF when possible,
-- and use the representative variable otherwise.
zonk
  :: Traversable tpF
  => Unifix tpF
  -> IO (Unifix tpF)
zonk x = do
  zonk1 x >>= \case
    UnifixV repr -> do
      pure $ UnifixV repr
    UnifixF fX -> do
      UnifixF <$> traverse zonk fX

noUnivars
  :: forall tpF. Traversable tpF
  => Unifix tpF
  -> MaybeT IO (Fix tpF)
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
  :: forall tpF. Traversable tpF
  => Univar tpF
  -> Unifix tpF
  -> IO Bool
occursIn (Univar pt0) x0 = do
  pt <- UnionFind.repr pt0
  x <- zonk x0
  go (Univar pt) x
  where
    go
      :: Univar tpF
      -> Unifix tpF
      -> IO Bool
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
  :: forall tpF. (Traversable tpF, Match tpF)
  => Unifix tpF
  -> Unifix tpF
  -> MaybeT IO ()
unify = unifyXX
  where
    unifyXX
      :: Unifix tpF
      -> Unifix tpF
      -> MaybeT IO ()
    unifyXX x1 x2 = do
      z1 <- lift $ zonk1 x1
      z2 <- lift $ zonk1 x2
      unifyZZ z1 z2

    unifyZZ
      :: Unifix tpF
      -> Unifix tpF
      -> MaybeT IO ()
    unifyZZ (UnifixV repr1) (UnifixV repr2) = do
      unifyRR repr1 repr2
    unifyZZ (UnifixV repr) (UnifixF fX) = do
      unifyRF repr fX
    unifyZZ (UnifixF fX) (UnifixV repr) = do
      unifyRF repr fX
    unifyZZ (UnifixF fX1) (UnifixF fX2) = do
      unifyFF fX1 fX2

    unifyRR
      :: Univar tpF
      -> Univar tpF
      -> MaybeT IO ()
    unifyRR (Univar repr1) (Univar repr2) = do
      lift $ UnionFind.union repr1 repr2

    unifyRF
      :: Univar tpF
      -> tpF (Unifix tpF)
      -> MaybeT IO ()
    unifyRF (Univar pt) fX = do
      bad <- lift $ occursIn (Univar pt) (UnifixF fX)
      guard (not bad)
      lift $ UnionFind.setDescriptor pt $ Just fX

    unifyFF
      :: tpF (Unifix tpF)
      -> tpF (Unifix tpF)
      -> MaybeT IO ()
    unifyFF fX1 fX2 = do
      maybeUnit <- matchM fX1 fX2 $ \fX1' fX2' -> do
        for_ ((,) <$> fX1' <*> fX2') $ \(x1, x2) -> do
          unifyXX x1 x2
      MaybeT $ pure maybeUnit