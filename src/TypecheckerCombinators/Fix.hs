{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypecheckerCombinators.Fix where

import TypecheckerCombinators.Elem


newtype Fix f = Fix
  { unFix :: f (Fix f)
  }

deriving instance Eq (f (Fix f)) => Eq (Fix f)

instance Show (f (Fix f))
      => Show (Fix f)
         where
  showsPrec p (Fix fFix)
    = showsPrec p fFix

class Roll fix where
  mkFix
    :: fs (fix fs)
    -> fix fs
  matchFix
    :: fix fs
    -> Maybe (fs (fix fs))

instance Roll Fix where
  mkFix
    = Fix
  matchFix (Fix fFix) = do
    pure fFix

roll
  :: (Roll fix, Elem f fs)
  => f (fix fs)
  -> fix fs
roll
  = mkFix . inj

unroll
  :: (Roll fix, Elem f fs)
  => fix fs
  -> Maybe (f (fix fs))
unroll fix = do
  fsFix <- matchFix fix
  prj fsFix

reroll
  :: (Roll fix, Functor fs)
  => Fix fs
  -> fix fs
reroll (Fix fsFix)
  = mkFix (fmap reroll fsFix)
