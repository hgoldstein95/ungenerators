{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Examples where

import BiGen (BG, BiGen (..), Choice)
import CheckGen
import CtxGen (CtxGen (..))
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import HellGen
import Profmonad (Iso, Profmonad, Profunctor (..), (<$$>), (<**>))
import RLGen
import SeqGen (SeqGen, wBRT)
import Test.QuickCheck (Arbitrary (..), elements, genericShrink, oneof, quickCheck)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)

data Tree
  = Leaf
  | Node Tree Int Tree
  deriving (Show, Eq, Generic)

instance Arbitrary Tree where
  arbitrary = genericArbitrary
  shrink = genericShrink

node :: Iso ((Tree, Int), Tree) Tree
node = (\((l, x), r) -> Node l x r, \case Node l x r -> Just ((l, x), r); _ -> Nothing)

nodeVal :: Tree -> Maybe Int
nodeVal (Node _ x _) = Just x
nodeVal _ = Nothing

nodeLeft :: Tree -> Maybe Tree
nodeLeft (Node l _ _) = Just l
nodeLeft _ = Nothing

nodeRight :: Tree -> Maybe Tree
nodeRight (Node _ _ r) = Just r
nodeRight _ = Nothing

genTree :: forall g. (BiGen g, Profmonad g) => g Tree Tree
genTree = aux 4
  where
    aux 0 = pure Leaf
    aux n =
      select
        "Tree"
        [ pure Leaf,
          do
            x <- comap nodeVal genInt
            l <- comap nodeLeft (aux (n - 1))
            r <- comap nodeRight (aux (n - 1))
            pure (Node l x r)
        ]
    genInt = select "Int" [pure x | x <- [0 .. 10]]

badGenTree :: forall g. (BiGen g, Profmonad g) => g Tree Tree
badGenTree = aux 4
  where
    aux 0 = pure Leaf
    aux n =
      select
        "Tree"
        [ pure Leaf,
          do
            x <- comap nodeVal genInt
            l <- comap nodeRight (aux (n - 1))
            r <- comap nodeLeft (aux (n - 1))
            pure (Node l x r)
        ]
    genInt = select "Int" [pure x | x <- [0 .. 10]]

(<:>) :: a -> [a] -> [a]
(<:>) x = take 4 . (x :)

genTree' :: forall g. CtxGen [String] g => g Tree Tree
genTree' = aux []
  where
    aux ctx = do
      selectCtx
        "Tree"
        ctx
        [ pure Leaf,
          do
            i <- comap nodeVal (genInt ctx)
            let ctx' = show i <:> ctx
            node <$$> aux ("L" <:> ctx') <**> pure i <**> aux ("R" <:> ctx')
        ]
    genInt ctx = selectCtx "Int" ctx [pure x | x <- [0 .. 10]]
