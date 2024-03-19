{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module TypecheckerCombinators where

import Control.Applicative (Alternative(empty))
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.UnionFind.IO qualified as UnionFind
import Test.DocTest (doctest)

-- $setup
-- >>> :set -XTypeOperators
-- >>> import Control.Monad.Trans.Maybe (runMaybeT)


-----------
-- Terms --
-----------

-- Each of our primitive type-checker combinators will support one term
-- constructor, and then we'll use 'combine' to combine two primitive
-- type-checkers into a bigger type-checker which covers both term
-- constructors.
--
-- For example, a type checker which only supports variables has type
-- @TypeChecker Var tp@, a type checker which only supports function
-- application has type @TypeChecker Lam tp@, and the combination of those two
-- type checkers has type @TypeChecker (Var + Lam) tp@.
--
-- This means that the usual way to define terms, as an algebraic datatype:
--
--     data Term
--       = Var String
--       | Lam String Term
--       | App Term Term
--
-- does not work because each of 'Var' and 'App' must be a type of their own,
-- not just a data constructor for a bigger type. So we do this instead:

newtype Var term = Var String

data App term = App term term

data Lam term = Lam String term

data Zero term = Zero

data Succ term = Succ term

-- |
-- Then, we use the approach from the Data Types à la Carte paper to combine
-- those individual types into a bigger type like (Var + App):
--
-- >>> type Term = Fix (Var + Lam)
-- >>> (Fix $ InR $ Lam "hello" $ Fix $ InL $ Var "world") :: Term
-- Lam "hello" (Var "world")
--
-- Along with some boilerplate for making it easy to construct and match on
-- each term constructor:
--
-- >>> term = (lam "hello" $ var "world") :: Term
-- >>> term
-- Lam "hello" (Var "world")
--
-- >>> Just (Lam s _) = unroll term
-- >>> s
-- "hello"


-----------
-- Types --
-----------

data Arr ty = Arr ty ty

data Nat ty = Nat


-----------------
-- TypeChecker --
-----------------

type Check term tp m
  = term -> tp -> m ()
type Infer term tp m
  = term -> m tp

data TypeChecker exprF tp m = TypeChecker
  { mkCheck
      :: forall r
       . Check r tp m
      -> Infer r tp m
      -> Check (exprF r) tp m
  , mkInfer
      :: forall r
       . Check r tp m
      -> Infer r tp m
      -> Infer (exprF r) tp m
  }

checkInferTC
  :: TypeChecker exprF tp m
  -> ( Check (Fix exprF) tp m
     , Infer (Fix exprF) tp m
     )
checkInferTC tc = (check, infer)
  where
    check (Fix fFix) tp
      = mkCheck tc check infer fFix tp
    infer (Fix fFix)
      = mkInfer tc check infer fFix

checkTC
  :: TypeChecker exprF tp m
  -> Check (Fix exprF) tp m
checkTC = fst . checkInferTC

inferTC
  :: TypeChecker exprF tp m
  -> Infer (Fix exprF) tp m
inferTC = snd . checkInferTC

combine
  :: forall exprF exprG tp m
   . TypeChecker exprF tp m
  -> TypeChecker exprG tp m
  -> TypeChecker (exprF + exprG) tp m
combine tcF tcG = TypeChecker mkCheckFG mkInferFG
  where
    mkCheckFG
      :: Check r tp m
      -> Infer r tp m
      -> Check ((+) exprF exprG r) tp m
    mkCheckFG checkR inferR (InL fFix) tp
      = mkCheck tcF checkR inferR fFix tp
    mkCheckFG checkR inferR (InR gFix) tp
      = mkCheck tcG checkR inferR gFix tp

    mkInferFG
      :: Check r tp m
      -> Infer r tp m
      -> Infer ((+) exprF exprG r) tp m
    mkInferFG checkR inferR (InL fFix)
      = mkInfer tcF checkR inferR fFix
    mkInferFG checkR inferR (InR gFix)
      = mkInfer tcG checkR inferR gFix

checker
  :: forall exprF tp m. Alternative m
  => ( forall r
     . Check r tp m
    -> Infer r tp m
    -> Check (exprF r) tp m
     )
  -> TypeChecker exprF tp m
checker mkCheckF = TypeChecker mkCheckF mkInferF
  where
    mkInferF
      :: Check r tp m
      -> Infer r tp m
      -> Infer (exprF r) tp m
    mkInferF _checkR _inferR _fR = do
      empty

inferer
  :: forall exprF tp m. (Eq tp, Monad m, Alternative m)
  => ( forall r
     . Check r tp m
    -> Infer r tp m
    -> Infer (exprF r) tp m
     )
  -> TypeChecker exprF tp m
inferer mkInferF = TypeChecker mkCheckF mkInferF
  where
    mkCheckF
      :: Check r tp m
      -> Infer r tp m
      -> Check (exprF r) tp m
    mkCheckF checkR inferR fR expected = do
      actual <- mkInferF checkR inferR fR
      guard (actual == expected)

--varChecker
--  ::
--  => TypeChecker Var


----------------------
-- Term boilerplate --
----------------------

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


----------------------
-- Type boilerplate --
----------------------

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


---------------------------
-- Data Types à la Carte --
---------------------------

data (+) f g a
  = InL (f a)
  | InR (g a)

instance ( Show (f a)
         , Show (g a)
         )
        => Show ((+) f g a)
           where
  showsPrec p (InL fa) = showsPrec p fa
  showsPrec p (InR ga) = showsPrec p ga

deriving instance (Functor f, Functor g) => Functor (f + g)

deriving instance (Foldable f, Foldable g) => Foldable (f + g)

deriving instance (Traversable f, Traversable g) => Traversable (f + g)


data Dir = L | R

type family Find (f :: Type -> Type)
                 (fs :: Type -> Type)
              :: Maybe [Dir]
                 where
  Find f f
    = 'Just '[]
  Find f (fs + gs)
    = Find'L f (Find f fs) gs
  Find _ _
    = 'Nothing

type family Find'L (f :: Type -> Type)
                   (mayebPath :: Maybe [Dir])
                   (gs :: Type -> Type)
                :: Maybe [Dir]
                   where
  Find'L f ('Just path) gs
    = 'Just ('L ': path)
  Find'L f 'Nothing gs
    = Find'R (Find f gs)

type family Find'R (mayebPath :: Maybe [Dir])
                :: Maybe [Dir]
                   where
  Find'R ('Just path)
    = 'Just ('R ': path)
  Find'R 'Nothing
    = 'Nothing

class Elem (f :: Type -> Type)
           (fs :: Type -> Type)
           where
  inj :: f a -> fs a
  prj :: fs a -> Maybe (f a)

instance ( Find f fs ~ 'Just path
         , Elem'Aux f fs path
         )
        => Elem f fs
           where
  inj = injAux (Proxy @path)
  prj = prjAux (Proxy @path)

class Elem'Aux (f :: Type -> Type)
               (fs :: Type -> Type)
               (path :: [Dir])
               where
  injAux :: Proxy path -> f a -> fs a
  prjAux :: Proxy path -> fs a -> Maybe (f a)

instance fs ~ f
      => Elem'Aux f fs '[]
         where
  injAux _ = id
  prjAux _ = Just

instance Elem'Aux f fs path
      => Elem'Aux f (fs + gs) ('L ': path)
         where
  injAux _ = InL . injAux (Proxy @path)
  prjAux _ (InL fs) = prjAux (Proxy @path) fs
  prjAux _ _ = Nothing

instance Elem'Aux f gs path
      => Elem'Aux f (fs + gs) ('R ': path)
         where
  injAux _ = InR . injAux (Proxy @path)
  prjAux _ (InR gs) = prjAux (Proxy @path) gs
  prjAux _ _ = Nothing


newtype Fix f = Fix
  { unFix :: f (Fix f)
  }

instance Show (f (Fix f))
      => Show (Fix f)
         where
  showsPrec p (Fix fFix) = showsPrec p fFix

roll :: Elem f fs => f (Fix fs) -> Fix fs
roll = Fix . inj

unroll :: Elem f fs => Fix fs -> Maybe (f (Fix fs))
unroll = prj . unFix


-----------
-- Match --
-----------

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


-----------------
-- Unification --
-----------------

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
