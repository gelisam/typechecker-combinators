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

class Roll fix where
  roll
    :: Elem f fs
    => f (fix fs)
    -> fix fs
  unroll
    :: Elem f fs
    => fix fs
    -> Maybe (f (fix fs))

instance Roll Fix where
  roll
    = Fix . inj
  unroll
    = prj . unFix