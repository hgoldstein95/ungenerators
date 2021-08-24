{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RLGen where

import BiGen (Choice)
import Control.Applicative (Alternative (..))
import Control.Arrow (first)
import Control.Monad (MonadPlus, ap, msum, (<=<), (>=>))
import CtxGen (CtxGen (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, maybeToList)
import Profmonad (Profmonad, Profunctor (..))
import SeqGen (SeqUnGen (runSeqUnGen))
import qualified Test.QuickCheck as QC

type RLModel c = Map (String, c) [Int]

newtype RLGen c b a = RLGen {runRLGen :: RLModel c -> QC.Gen a}

instance Functor (RLGen c b) where
  fmap f x = RLGen $ fmap f . runRLGen x

instance Applicative (RLGen c b) where
  pure = return
  (<*>) = ap

instance Monad (RLGen c b) where
  return x = RLGen $ \_ -> pure x
  mx >>= f = RLGen $ \b -> do
    x <- runRLGen mx b
    runRLGen (f x) b

instance Profunctor (RLGen c) where
  comap _ = RLGen . runRLGen

instance Profmonad (RLGen c)

instance Ord c => CtxGen c (RLGen c) where
  uniformCtx _ gs = RLGen $ \m -> QC.oneof $ map (`runRLGen` m) gs
  selectCtx s c gs = RLGen $ \m -> do
    let fs = Map.findWithDefault [1 ..] (s, c) m
    i <- QC.frequency (zip fs (pure <$> [0 ..]))
    runRLGen (gs !! i) m

newtype RLUnGen c b a = RLUnGen {runRLUnGen :: b -> Maybe (a, [(c, Choice)])}

instance Functor (RLUnGen c b) where
  fmap f x = RLUnGen $ fmap (first f) . runRLUnGen x

instance Applicative (RLUnGen c b) where
  pure = return
  (<*>) = ap

instance Monad (RLUnGen c b) where
  return x = RLUnGen $ const (Just (x, []))
  mx >>= f = RLUnGen $ \b ->
    case runRLUnGen mx b of
      Nothing -> Nothing
      Just (x, cs) -> case runRLUnGen (f x) b of
        Nothing -> Nothing
        Just (y, cs') -> Just (y, cs ++ cs')

instance Alternative (RLUnGen c b) where
  empty = RLUnGen $ const Nothing
  ux <|> uy = RLUnGen $ \b ->
    let extractMaybe = ($ b) . runRLUnGen
     in extractMaybe ux <|> extractMaybe uy

instance MonadPlus (RLUnGen c b)

instance Profunctor (RLUnGen c) where
  comap f ug = RLUnGen $ f >=> runRLUnGen ug

instance Profmonad (RLUnGen c)

instance Ord c => CtxGen c (RLUnGen c) where
  selectCtx s c = msum . zipWith recordChoice [0 ..]
    where
      tell x = RLUnGen $ \b -> Just ((), x)
      recordChoice i d = do
        tell [(c, (s, i))]
        x <- d
        comap (\y -> if x == y then Just x else Nothing) (pure x)
  uniformCtx _ = msum

reward :: RLUnGen c a a -> Double -> [(c, Choice)] -> RLModel c -> RLModel c
reward = undefined

train :: RLUnGen c a a -> [(Double, a)] -> RLModel c
train u = foldr (\(r, x) -> reward u r (ungenerate u x)) Map.empty

ungenerate :: RLUnGen c a a -> a -> [(c, Choice)]
ungenerate g t = snd . fromJust $ runRLUnGen g t

rlCheck ::
  forall a c.
  Ord c =>
  (forall g. CtxGen c g => g a a) ->
  [(Double, a)] ->
  (a -> Double) ->
  QC.Gen [a]
rlCheck cg corpus r = aux (train u corpus)
  where
    aux m = do
      x <- runRLGen g m
      let m' = reward u (r x) (ungenerate u x) m
      (x :) <$> aux m'
    g = cg :: RLGen c a a
    u = cg :: RLUnGen c a a
