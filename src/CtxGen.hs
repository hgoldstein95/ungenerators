{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module CtxGen where

import BiGen (BiGen (..))
import Profmonad (Profmonad)

class Profmonad g => CtxGen c g where
  selectCtx :: Eq a => String -> c -> [g a a] -> g a a
  uniformCtx :: c -> [g a a] -> g a a
