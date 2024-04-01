{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Typechecker.Elems
  ( Elems(rolls, unrolls)
  ) where

import Data.Kind (Type)

import Typechecker.Elem
import Typechecker.Fix


class Elems (fffFix :: Type)
            (fix :: (Type -> Type) -> Type)
            (fs :: Type -> Type)
            where
  -- rolls
  --   :: f1 (f2 (f3 (fix fs)))
  --   -> fixFs
  rolls
    :: fffFix
    -> fix fs

  -- unrolls
  --   :: fix fs
  --   -> Maybe (f1 (f2 (f3 (fix fs))))
  unrolls
    :: fix fs
    -> Maybe fffFix

instance {-# OVERLAPPABLE #-}
         Elems (fix fs) fix fs
         where
  rolls
    = id
  unrolls
    = Just

instance {-# OVERLAPPING #-}
       ( Traversable f
       , Roll fix
       , Elem f fs
       , Elems ffFix fix fs
       )
      => Elems (f ffFix) fix fs
         where
  rolls
    = roll . fmap rolls
  unrolls fixFs = do
    fFix <- unroll fixFs
    traverse unrolls fFix