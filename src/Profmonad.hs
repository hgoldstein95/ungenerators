{-# LANGUAGE QuantifiedConstraints #-}

module Profmonad where

type Iso a b = (a -> b, b -> Maybe a)

class (forall a. Functor (p a)) => Profunctor p where
  comap :: (u -> Maybe u') -> p u' v -> p u v

class (Profunctor p, forall a. Monad (p a)) => Profmonad p

(<$$>) :: forall p a b. Profunctor p => Iso a b -> p a a -> p b b
(f, b) <$$> p = comap b (fmap f p)

infixr 0 <$$>

(<**>) :: forall p a b. Profmonad p => p a a -> p b b -> p (a, b) (a, b)
px <**> py = do
  x <- comap mfst px
  y <- comap msnd py
  return (x, y)
  where
    mfst (x, _) = Just x
    msnd (_, y) = Just y

infixl 4 <**>
