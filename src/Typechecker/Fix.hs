{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Typechecker.Fix where

import Typechecker.Sum


newtype Fix f = Fix
  { unFix :: f (Fix f)
  }

instance Show (f (Fix f))
      => Show (Fix f)
         where
  showsPrec p (Fix fFix) = showsPrec p fFix

roll
  :: Elem f fs
  => f (Fix fs)
  -> Fix fs
roll
  = Fix . inj

unroll
  :: Elem f fs
  => Fix fs
  -> Maybe (f (Fix fs))
unroll
  = prj . unFix
