{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Typechecker.Match
  ( Match(match)
  , matchM
  ) where

import Typechecker.Sum


class Traversable fs => Match fs where
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
      Just <$> mr
