{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module HellGen where

import BiGen (BiGen (..))
import Control.Applicative (Alternative (..))
import Control.Arrow (first)
import Control.Monad (MonadPlus, ap, msum, (>=>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Profmonad (Profmonad, Profunctor (..))
import qualified Test.QuickCheck as QC

----------------------------
-- HellGen
----------------------------

type WeightMap = Map String [Int]

(<++>) :: WeightMap -> WeightMap -> WeightMap
(<++>) = Map.unionWith (zipWith (+))

newtype HellGen b a = HellGen {runHellGen :: WeightMap -> QC.Gen a}

instance Functor (HellGen b) where
  fmap f x = HellGen $ (f <$>) . runHellGen x

instance Applicative (HellGen b) where
  pure = return
  (<*>) = ap

instance Monad (HellGen b) where
  return x = HellGen $ const (pure x)
  mx >>= f = HellGen $ \m ->
    runHellGen mx m >>= \x -> runHellGen (f x) m

instance Profunctor HellGen where
  comap _ = HellGen . runHellGen

instance Profmonad HellGen

instance BiGen HellGen where
  uniform gs = HellGen $ \_ -> QC.elements gs
  select sid gs = HellGen $ \m ->
    QC.frequency (zip (m Map.! sid) ((`runHellGen` m) <$> gs))

----------------------------
-- HellUnGen
----------------------------

newtype HellUnGen b a = HellUnGen {runHellUnGen :: b -> Maybe (a, WeightMap)}

instance Functor (HellUnGen b) where
  fmap f x = HellUnGen $ fmap (first f) . runHellUnGen x

instance Applicative (HellUnGen b) where
  pure = return
  (<*>) = ap

instance Monad (HellUnGen b) where
  return x = HellUnGen $ const (Just (x, Map.empty))
  mx >>= f = HellUnGen $ \b ->
    case runHellUnGen mx b of
      Nothing -> Nothing
      Just (x, cs) -> case runHellUnGen (f x) b of
        Nothing -> Nothing
        Just (y, cs') -> Just (y, cs' <++> cs)

instance Alternative (HellUnGen b) where
  empty = HellUnGen $ const Nothing
  ux <|> uy = HellUnGen $ \b ->
    let extractMaybe = ($ b) . runHellUnGen
     in extractMaybe ux <|> extractMaybe uy

instance MonadPlus (HellUnGen b)

instance Profunctor HellUnGen where
  comap f ug = HellUnGen $ f >=> runHellUnGen ug

instance Profmonad HellUnGen

instance BiGen HellUnGen where
  select s gs = msum . zipWith recordChoice [0 ..] $ gs
    where
      emit (_, i) =
        HellUnGen $ \_ ->
          Just ((), Map.singleton s [if j == i then 1 else 0 | j <- [0 .. length gs - 1]])
      recordChoice i d = do
        x <- d
        emit (s, i)
        comap (\y -> if x == y then Just x else Nothing) (pure x)
  uniform = msum . map pure

mine :: HellUnGen a a -> [a] -> WeightMap
mine g = foldr ((<++>) . snd . fromJust . runHellUnGen g) Map.empty
