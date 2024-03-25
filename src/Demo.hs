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
  :: ( Elem Nat tpF
     , Roll fix
     , MonadEq (fix tpF) m
     )
  => TypeChecker NatLit (fix tpF) m
natLitTC = inferer $ \_check _infer (NatLit _n) -> do
  pure nat

plusTC
  :: ( Elem Nat tpF
     , Roll fix
     , MonadEq (fix tpF) m
     )
  => TypeChecker Plus (fix tpF) m
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

-- ### Explanation
--
-- As you can see, typechecker combinators use bidirectional typechecking. There
-- are two kinds of combinators, `inferer`, and `checker`, depending on which
-- direction should be used for the construct in question, and the combinator is
-- implemented by making recursive calls to `check` and `infer`.
--
-- Each primitive combinator only checks a single construct, in this case
-- `NatLit` and `Plus`. Composing them using `<+>` allows us to typecheck a
-- language consisting of multiple constructs, so `NatList + Plus`. At the type
-- level, each construct is represented by a `Functor` whose elements are the
-- sub-terms, and the type-level sum `NatPlus + Plus` is also a `Functor`. The
-- AST for a language is a tree of constructs, that is, `Fix (NatLit + Plus)`.
-- If that seems strange, the paper
-- [Data Types à la Carte](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/data-types-a-la-carte/14416CB20C4637164EA9F77097909409)
-- explains the technique in detail.
--
-- ### Constraints
--
-- The type of those typechecker combinators have a bunch of type variables and
-- constraints. This polymorphisms allows them to be reused in many languages.
-- Here is what each constraint means.
--
-- We have seen that the data type for terms is `Fix (NatLit + Plus)`, the
-- fixpoint of a type-level sum. The data type for types is also the fixpoint of
-- a type-level sum, for example `Fix (Arr + Nat)`. The `Elem Nat tpF`
-- constraint checks that `Nat` is present in `Arr + Nat`.
--
-- Types may also contain unification variables, in which case we use `Unifix
-- (Arr + Nat)` instead of `Fix (Arr + Nat)`. The `Roll fix` constraint checks
-- that `fix` is either instantiated to `Fix` or `Unifix`.
--
-- The `MonadEq` constraint ensures that the monad in which we run the
-- typechecker is compatible with that choice: if unification variables are
-- present, the monad must support `MonadUnification`. In the case of
-- `inferHutton`, we use `Fix`, so the `Identity` monad suffices.
--
-- ```haskell
-- inferWithTypeChecker :: TypeChecker termF tp m -> Fix termF -> MaybeT m tp
-- ```