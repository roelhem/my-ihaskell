{-# LANGUAGE OverloadedStrings #-}

module Math.AD.Data.Bracketing where

import Control.Lens
import Text.LaTeX (Texy, texy, autoParens, dot, cdot, overline, mathDisplay)
import IHaskell.Display (IHaskellDisplay, display)
import Text.LaTeX.Helpers (displayTexyWith)
import Control.Monad.Writer

data Bracketing a = Start
                  | Tangent a (Bracketing a)
                  | Adjoint (Bracketing a) a
                  | Preacc  (Bracketing a) (Bracketing a) deriving (Eq)

data BracketingMode = TangentMode|AdjointMode|PreaccMode deriving (Eq, Show, Enum, Bounded)

instance Semigroup (Bracketing a) where
  Start <> ys = ys
  xs <> Start = xs
  (Tangent x xs) <> ys = Tangent x (xs <> ys)
  xs <> (Adjoint ys y) = Adjoint (xs <> ys) y
  xs <> (Preacc yl yr) = Preacc (xs <> yl) yr
  (Preacc xl xr) <> ys = Preacc xl (xr <> ys)
  xs <> ys = Preacc xs ys

instance Monoid (Bracketing a) where
  mempty = Start

instance Functor Bracketing where
  fmap _ Start = Start
  fmap f (Tangent x other) = Tangent (f x) (f <$> other)
  fmap f (Adjoint other x) = Adjoint (f <$> other) (f x)
  fmap f (Preacc  lhs rhs) = Preacc  (f <$> lhs) (f <$> rhs)

instance Foldable Bracketing where
  foldMap _ Start = mempty
  foldMap f (Tangent x other) = foldMap f other <> f x
  foldMap f (Adjoint other x) = f x <> foldMap f other
  foldMap f (Preacc lhs rhs)  = foldMap f rhs <> foldMap f lhs

instance Traversable Bracketing where
  traverse _ Start = pure Start
  traverse f (Tangent x other) = flip Tangent <$> traverse f other <*> f x
  traverse f (Adjoint other x) = flip Adjoint <$> f x <*> traverse f other
  traverse f (Preacc  lhs rhs) = flip Preacc <$> traverse f rhs <*> traverse f lhs

instance Show a => Show (Bracketing a) where
  show Start             = "I"
  show (Tangent f other) = "(T{" ++ show f ++ "} * " ++ show other ++ ")"
  show (Adjoint other f) = "(" ++ show other ++ " * A{" ++ show f ++ "})"
  show (Preacc  lhs rhs) = "(" ++ show lhs ++ " * " ++ show rhs ++ ")"

instance Texy a => Texy (Bracketing a) where
  texy Start = "I"
  texy (Tangent f other) = autoParens $ dot (texy f) `cdot` texy other
  texy (Adjoint other f) = autoParens $ texy other `cdot` overline (texy f)
  texy (Preacc  lhs rhs) = autoParens $ texy lhs `cdot` texy rhs

instance Texy a => IHaskellDisplay (Bracketing a) where
  display = displayTexyWith mathDisplay

-- LENSES

-- | A traversal to all the start entries of the bracketing.
starts :: Traversal' (Bracketing a) (Bracketing a)
starts f Start = f Start
starts f (Tangent x xs) = Tangent x <$> starts f xs
starts f (Adjoint xs x) = (`Adjoint` x) <$> starts f xs
starts f (Preacc xl xr) = flip Preacc <$> starts f xr <*> starts f xl

-- BRACKETING MUTATIONS
preaccumulate :: Bracketing a -> Bracketing a
preaccumulate Start = Start
preaccumulate (Tangent f other) = Tangent f Start <> other
preaccumulate (Adjoint other f) = other <> Adjoint Start f
preaccumulate x@(Preacc _ _) = x

-- GENERATING BRACKETINGS

-- | Generates a homogenious tangent bracketing.
homTangent :: [a] -> Bracketing a
homTangent fs = go (reverse fs) where
  go []     = Start
  go (x:xs) = Tangent x (go xs)

-- | Generates a homogenious adjoint bracketing.
homAdjoint :: [a] -> Bracketing a
homAdjoint = go where
  go []     = Start
  go (x:xs) = Adjoint (go xs) x

-- | Generates all homogenious tangents after homogenious adjoint bracketings.
genTangentAfterAdjoints :: [a] -> [Bracketing a]
genTangentAfterAdjoints [] = []
genTangentAfterAdjoints xs = f <$> splits where
  f (xs, ys) = homTangent xs <> homAdjoint ys
  splits = (`splitAt` xs) <$> [0..length xs-1]

-- | Generates all possible bracketings.
genEach :: [a] -> [Bracketing a]
genEach []   = [Start]
genEach fs   = concat [
    fmap (Tangent (last fs)) (genEach $ init fs),
    fmap (\x -> Adjoint x (head fs)) (genEach $ tail fs),
    concatMap acc (splitEach fs)
  ] where
  splitEach [] = []
  splitEach [_] = []
  splitEach xs = fmap (`splitAt` xs) [1..length xs-1]
  acc (lhs,rhs) = concatMap (<$> genEach rhs) (Preacc <$> genEach lhs)
