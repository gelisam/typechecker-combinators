{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- Data Types à la Carte
module TypecheckerCombinators.Sum
  ( type (+)(InL, InR)
  ) where


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
