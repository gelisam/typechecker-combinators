{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- Data Types à la Carte
module Typechecker.Elem
  ( type (+)(InL, InR)
  , Elem(inj, prj)
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))


data (+) f g a
  = InL (f a)
  | InR (g a)

instance ( Eq (f a)
         , Eq (g a)
         )
        => Eq ((+) f g a)
           where
  InL fa1 == InL fa2 = fa1 == fa2
  InR ga1 == InR ga2 = ga1 == ga2
  _ == _ = False

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