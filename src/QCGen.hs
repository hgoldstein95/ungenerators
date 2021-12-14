{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

module QCGen where

import BiGen (BiGen (..))
import Control.Monad (ap)
import Profmonad (Profmonad, Profunctor (..))
import qualified Test.QuickCheck as QC

newtype QCGen b a = QCGen {runQCGen :: QC.Gen a}

instance Functor (QCGen b) where
  fmap f = QCGen . fmap f . runQCGen

instance Applicative (QCGen b) where
  pure = return
  (<*>) = ap

instance Monad (QCGen b) where
  return = QCGen . return
  mx >>= f = QCGen $ runQCGen mx >>= runQCGen . f

instance Profunctor QCGen where
  comap _ = QCGen . runQCGen

instance Profmonad QCGen

instance BiGen QCGen where
  uniform = QCGen . QC.elements
  select _ = QCGen . QC.oneof . map runQCGen
