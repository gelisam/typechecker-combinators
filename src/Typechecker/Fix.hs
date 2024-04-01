{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Typechecker.Fix where

import Typechecker.Sum


newtype Fix f = Fix (f (Fix f))

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
  unFix
    :: fix fs
    -> Maybe (fs (fix fs))

instance Roll Fix where
  mkFix
    = Fix
  unFix (Fix fFix) = do
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
  fsFix <- unFix fix
  prj fsFix

reroll
  :: (Roll fix, Functor fs)
  => Fix fs
  -> fix fs
reroll (Fix fsFix)
  = mkFix (fmap reroll fsFix)