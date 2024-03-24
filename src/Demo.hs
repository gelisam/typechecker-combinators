{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Demo where

import Prelude hiding ((+))

import Data.Functor.Identity

import Typechecker.Combinators
import Typechecker.Terms
import Typechecker.Types


-- README BEGINS

-- # Typechecker Combinators
--
-- This is a Haskell library for writing typecheckers out of composable parts;
-- like parser combinators, but for typechecking.
--
-- ## Hutton's razor
--
-- Let's begin with a simple language which only has integers and addition.
-- In this file, we are focusing on type-checking, so the definitions of
-- `NatLit`, `natLit`, `Plus`, `(+)`, and `Nat` are omitted.

type Hutton = NatLit + Plus

huttonProgram
  :: Fix Hutton
huttonProgram
  = natLit 2 + natLit 3 + natLit 4

-- The typechecker for this language is not very interesting since every term
-- has type Nat; what is interesting is that the two typechecker combinators we
-- implement for this language can be reused in the more complex languages later
-- in this file. That's the magic of combinators!

natLitTC
  :: (Elem Nat tpF, Eq (Fix tpF), Monad m)
  => TypeChecker NatLit (Fix tpF) m
natLitTC = inferer $ \_check _infer (NatLit _n) -> do
  pure nat

plusTC
  :: (Elem Nat tpF, Eq (Fix tpF), Monad m)
  => TypeChecker Plus (Fix tpF) m
plusTC = inferer $ \check _infer (Plus x y) -> do
  check x nat
  check y nat
  pure nat

-- |
-- >>> inferHutton huttonProgram
-- MaybeT (Identity (Just Nat))
inferHutton
  :: Fix Hutton
  -> MaybeT Identity (Fix Nat)
inferHutton = inferWithTypeChecker (natLitTC <+> plusTC)
