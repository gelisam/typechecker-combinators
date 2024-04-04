{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Typechecker.Sum


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