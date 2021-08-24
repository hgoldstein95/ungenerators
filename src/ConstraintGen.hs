{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module ConstraintGen where

import BiGen (BiGen (..), Choice)
import Control.Applicative (Alternative (..))
import Control.Arrow (first)
import Control.Monad (MonadPlus, ap, guard, msum, (>=>))
import Profmonad (Profmonad, Profunctor (..))
import qualified Test.QuickCheck as QC

type Var = String

type Constraint a = ()

newtype CGen b a = CGen {runCGen :: QC.Gen (a, Constraint b)}

instance Functor (CGen b) where
  fmap f x = undefined

instance Applicative (CGen b) where
  pure = return
  (<*>) = ap

instance Monad (CGen b) where
  return x = undefined
  mx >>= f = undefined

instance Profunctor CGen where
  comap _ = undefined

instance Profmonad CGen

instance BiGen CGen where
  uniform = undefined
  select sid gs = undefined
