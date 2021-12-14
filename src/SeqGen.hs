{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module SeqGen where

import BiGen (BiGen (..), Choice)
import Control.Applicative (Alternative (..))
import Control.Arrow (first)
import Control.Monad (MonadPlus, ap, msum, (>=>))
import Profmonad (Profmonad, Profunctor (..))
import Test.QuickCheck (Arbitrary (..), Property, discard, forAll, (===))

----------------------------
-- SeqGen
----------------------------

newtype SeqGen b a = SeqGen {runSeqGen :: [Choice] -> Maybe (a, [Choice])}

instance Functor (SeqGen b) where
  fmap f x = SeqGen $ (first f <$>) . runSeqGen x

instance Applicative (SeqGen b) where
  pure = return
  (<*>) = ap

instance Monad (SeqGen b) where
  return x = SeqGen $ \cs -> Just (x, cs)
  mx >>= f =
    SeqGen $
      runSeqGen mx >=> \(x, cs') -> runSeqGen (f x) cs'

instance Profunctor SeqGen where
  comap _ = SeqGen . runSeqGen

instance Profmonad SeqGen

instance BiGen SeqGen where
  uniform = pure . head
  select sid gs = do
    (s, i) <- choice
    if sid /= s
      then SeqGen (const Nothing)
      else gs !! i
    where
      choice = SeqGen $ \case
        [] -> Nothing
        (c : cs) -> Just (c, cs)

----------------------------
-- SeqUnGen
----------------------------

newtype SeqUnGen b a = SeqUnGen {runSeqUnGen :: b -> Maybe (a, [Choice])}

instance Functor (SeqUnGen b) where
  fmap f x = SeqUnGen $ fmap (first f) . runSeqUnGen x

instance Applicative (SeqUnGen b) where
  pure = return
  (<*>) = ap

instance Monad (SeqUnGen b) where
  return x = SeqUnGen $ const (Just (x, []))
  mx >>= f = SeqUnGen $ \b ->
    case runSeqUnGen mx b of
      Nothing -> Nothing
      Just (x, cs) -> case runSeqUnGen (f x) b of
        Nothing -> Nothing
        Just (y, cs') -> Just (y, cs ++ cs')

instance Alternative (SeqUnGen b) where
  empty = SeqUnGen $ const Nothing
  ux <|> uy = SeqUnGen $ \b ->
    let extractMaybe = ($ b) . runSeqUnGen
     in extractMaybe ux <|> extractMaybe uy

instance MonadPlus (SeqUnGen b)

instance Profunctor SeqUnGen where
  comap f ug = SeqUnGen $ f >=> runSeqUnGen ug

instance Profmonad SeqUnGen

instance BiGen SeqUnGen where
  select s = msum . zipWith recordChoice [0 ..]
    where
      tell x = SeqUnGen $ \_ -> Just ((), x)
      recordChoice i d = do
        tell [(s, i)]
        x <- d
        comap (\y -> if x == y then Just x else Nothing) (pure x)
  uniform = msum . map pure

----------------------------
-- Properties
----------------------------

wBRT :: (Arbitrary u, Eq u, Show u) => (forall b. BiGen b => b u u) -> Property
wBRT bg =
  forAll arbitrary $ \x ->
    case runSeqUnGen bg x of
      Nothing -> discard
      Just (y, cs) -> runSeqGen bg cs === Just (y, [])
