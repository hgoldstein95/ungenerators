{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module CheckGen where

import BiGen (BiGen (..))
import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus, ap, msum, (>=>))
import Control.Monad.Logic (Logic, MonadLogic ((>>-)), observeAll)
import Profmonad (Profmonad, Profunctor (..))
import QuickCheck.GenT (MonadGen (..), oneof)
import Test.QuickCheck (Gen, Property, forAll, (===))

----------------------------
-- PureGen
----------------------------

newtype PureGen b a = PureGen {runPureGen :: b -> Maybe a}

instance Functor (PureGen b) where
  fmap f m = PureGen $ fmap f . runPureGen m

instance Applicative (PureGen b) where
  pure = return
  (<*>) = ap

instance Monad (PureGen b) where
  return x = PureGen $ const (Just x)
  mx >>= f = PureGen $ \b ->
    runPureGen mx b >>= \x -> runPureGen (f x) b

instance Alternative (PureGen b) where
  empty = PureGen $ const Nothing
  mx <|> my = PureGen $ \b -> runPureGen mx b <|> runPureGen my b

instance MonadPlus (PureGen b)

instance Profunctor PureGen where
  comap f g = PureGen $ f >=> runPureGen g

instance Profmonad PureGen

instance BiGen PureGen where
  uniform = msum . map pure
  select _ gs = PureGen $ \b ->
    msum [if a == Just b then a else Nothing | g <- gs, let a = runPureGen g b]

purify :: (forall g. BiGen g => g u v) -> u -> Maybe v
purify = runPureGen

----------------------------
-- EnumGen
----------------------------

newtype EnumGen b a = EnumGen {runEnumGen :: Logic a}
  deriving (Functor, Applicative, Alternative, MonadPlus, MonadLogic)

instance Monad (EnumGen b) where
  return = EnumGen . pure
  mx >>= f = EnumGen (runEnumGen mx >>- runEnumGen . f)

instance Profunctor EnumGen where
  comap _ g = EnumGen (runEnumGen g)

instance Profmonad EnumGen

instance BiGen EnumGen where
  uniform = msum . fmap pure
  select _ = msum

enumerate :: (forall g. BiGen g => g u v) -> [v]
enumerate = observeAll . runEnumGen

----------------------------
-- EnumGen'
----------------------------

newtype EnumGen' b a = EnumGen' {runEnumGen' :: Gen a}
  deriving (Functor, Applicative, Monad, MonadGen)

instance Profunctor EnumGen' where
  comap _ g = EnumGen' (runEnumGen' g)

instance Profmonad EnumGen'

instance BiGen EnumGen' where
  uniform = oneof . fmap pure
  select _ = oneof

enumerate' :: (forall g. BiGen g => g u v) -> Gen v
enumerate' = runEnumGen'

sound :: (Eq u, Show u) => (forall g. BiGen g => g u u) -> Property
sound bg = forAll (enumerate' bg) $ \x ->
  purify bg x === Just x
