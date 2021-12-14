{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

module BiGen where

import Control.Monad (ap)
import Profmonad (Profmonad, Profunctor (..))

class BiGen g where
  select :: Eq a => String -> [g a a] -> g a a
  uniform :: [a] -> g a a

type Choice = (String, Int)

type BG a = forall g. BiGen g => g a a

data (:*:) p q u v = (:*:) {pfst :: p u v, psnd :: q u v}

instance (Functor (p u), Functor (q u)) => Functor ((p :*: q) u) where
  fmap f (p :*: q) = fmap f p :*: fmap f q

instance (Monad (p u), Monad (q u)) => Applicative ((p :*: q) u) where
  pure = return
  (<*>) = ap

instance (Monad (p u), Monad (q u)) => Monad ((p :*: q) u) where
  return y = return y :*: return y
  py :*: qy >>= kz = (py >>= pfst . kz) :*: (qy >>= psnd . kz)

instance (forall a. Functor ((p :*: q) a), Profunctor p, Profunctor q) => Profunctor (p :*: q) where
  comap f (py :*: qy) = comap f py :*: comap f qy

instance (Profmonad p, Profmonad q) => Profmonad (p :*: q)

instance (BiGen g, BiGen u) => BiGen (g :*: u) where
  select s cs = select s (pfst <$> cs) :*: select s (psnd <$> cs)
  uniform xs = uniform xs :*: uniform xs
