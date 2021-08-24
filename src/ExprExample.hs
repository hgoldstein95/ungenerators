{-# LANGUAGE LambdaCase #-}

module ExprExample where

import BiGen (BiGen (..))
import CheckGen
import Profmonad (Iso, (<$$>), (<**>))
import Prelude hiding (div)

data Expr
  = Term Term
  | Plus Expr Term
  | Minus Expr Term
  deriving (Show, Eq)

data Term = Factor Factor | Times Term Factor | Div Term Factor
  deriving (Show, Eq)

data Factor = Digits Digits | Pos Factor | Neg Factor | Parens Expr
  deriving (Show, Eq)

data Digits = Digit Char | More Char Digits
  deriving (Show, Eq)

term :: Iso Term Expr
term =
  ( Term,
    \case
      Term t -> Just t
      _ -> Nothing
  )

plus :: Iso (Expr, Term) Expr
plus =
  ( uncurry Plus,
    \case
      Plus e t -> Just (e, t)
      _ -> Nothing
  )

minus :: Iso (Expr, Term) Expr
minus =
  ( uncurry Minus,
    \case
      Minus e t -> Just (e, t)
      _ -> Nothing
  )

factor :: Iso Factor Term
factor =
  ( Factor,
    \case
      Factor t -> Just t
      _ -> Nothing
  )

times :: Iso (Term, Factor) Term
times =
  ( uncurry Times,
    \case
      Times t f -> Just (t, f)
      _ -> Nothing
  )

div :: Iso (Term, Factor) Term
div =
  ( uncurry Div,
    \case
      Div t f -> Just (t, f)
      _ -> Nothing
  )

digits :: Iso Digits Factor
digits =
  ( Digits,
    \case
      Digits d -> Just d
      _ -> Nothing
  )

pos :: Iso Factor Factor
pos =
  ( Pos,
    \case
      Pos f -> Just f
      _ -> Nothing
  )

neg :: Iso Factor Factor
neg =
  ( Neg,
    \case
      Neg f -> Just f
      _ -> Nothing
  )

parens :: Iso Expr Factor
parens =
  ( Parens,
    \case
      Parens e -> Just e
      _ -> Nothing
  )

digit :: Iso Char Digits
digit =
  ( Digit,
    \case
      Digit i -> Just i
      _ -> Nothing
  )

more :: Iso (Char, Digits) Digits
more =
  ( uncurry More,
    \case
      More i d -> Just (i, d)
      _ -> Nothing
  )

genExpr :: BiGen g => Int -> g Expr Expr
genExpr = \case
  0 -> term <$$> genTerm 0
  n ->
    select
      "Expr"
      [ term <$$> genTerm (n - 1),
        plus <$$> genExpr (n - 1) <**> genTerm (n - 1),
        minus <$$> genExpr (n - 1) <**> genTerm (n - 1)
      ]

genTerm :: BiGen g => Int -> g Term Term
genTerm = \case
  0 -> factor <$$> genFactor 0
  n ->
    select
      "Term"
      [ factor <$$> genFactor (n - 1),
        times <$$> genTerm (n - 1) <**> genFactor (n - 1),
        div <$$> genTerm (n - 1) <**> genFactor (n - 1)
      ]

genFactor :: BiGen g => Int -> g Factor Factor
genFactor = \case
  0 -> digits <$$> genDigits 0
  n ->
    select
      "Factor"
      [ digits <$$> genDigits (n - 1),
        pos <$$> genFactor (n - 1),
        neg <$$> genFactor (n - 1),
        parens <$$> genExpr (n - 1)
      ]

genDigits :: BiGen g => Int -> g Digits Digits
genDigits = \case
  0 -> digit <$$> int
  n ->
    select
      "Digits"
      [ digit <$$> int,
        more <$$> int <**> genDigits (n - 1)
      ]
  where
    int = select "INT" (fmap pure ['0' .. '9'])