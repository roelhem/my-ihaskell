{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Math.AD.Real.FuncChain where

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.List
import Math.AD.Data.Bracketing
import Math.AD.Real.FuncDims
import Math.AD.Real.FuncDescription
import IHaskell.Display (IHaskellDisplay, display)
import IHaskell.Display.Hatex ()
import Text.LaTeX.Helpers (realPow, displayTexyWith)
import Text.LaTeX.Packages.Mathtools (xrightarrow)
import Text.LaTeX.Packages.AMSFonts (reals)
import Text.LaTeX.Base.Class (LaTeXC)
import Text.LaTeX (Texy, texy, (!:), (^:), rightarrow, math)
import GHC.Exts (IsList, Item, toList, fromList)

data FChain a b = a :->: [(a, b)]
type FChain' = FChain Integer Integer
{- FuncChain class -}

{- Description of a chain of functions. -}

instance (Eq a, Eq b) => Eq (FChain a b) where
  (x :->: xs) == (y :->: ys) = x == y && xs == ys

instance Bifunctor FChain where
  bimap f g (x :->: xs) = f x :->: (bimap f g <$> xs)

instance Bifoldable FChain where
  bifoldMap f g (x :->: xs) = go xs where
    go [] = mempty
    go ((m,e):xc) = g e <> f m <> go xc

instance Bitraversable FChain

instance Functor (FChain a) where
  fmap f = second f

instance Foldable (FChain a) where
  foldMap f (x :->: xs) = foldMap (f.snd) xs

instance Traversable (FChain a) where
  traverse f (x :->: xs) = (:->:) x <$> trav where
    trav = zip (fmap fst xs) <$> traverse (f.snd) xs

instance Semigroup (FChain a b) where
  (_ :->: []) <> ys = ys
  (x :->: xs) <> (_ :->: ys) = x :->: (xs ++ ys)

instance Num a => Monoid (FChain a b) where
  mempty = 0 :->: []

instance Num a => IsList (FChain a b) where
  type Item (FChain a b) = FDesc (Dims a a) b
  fromList [] = mempty
  fromList [F (m :-> n) e] = m :->: [(n,e)]
  fromList (F (m :-> n) e:xs) = m :->: ((n,e):ys) where
    (_:->:ys) = fromList xs
  toList (_ :->: []) = []
  toList (m :->: [(n,e)]) = [F (m :-> n) e]
  toList (m :->: ((n,e):xs)) = F (m :-> n) e:toList (n :->: xs)

instance (Integral a) => FuncDimensions (FChain a b) where
  inDim (x:->:_) = toInteger x
  outDim (x:->:[]) = toInteger x
  outDim (x:->:xs) = toInteger.fst.last $ xs

instance (Integral a) => CompDimensions (FChain a b) where
  toDimList (m:->:xs) = DimList (toInteger m) (toInteger.fst <$> init xs) (toInteger.fst.last $ xs)

instance (Integral a, Integral b) => FuncDescription (FChain a b) where
  edgeCount xs = sum (edgeCountList xs)

instance (Integral a, Integral b) => CompDescription (FChain a b) where
  edgeCountList (m:->:[]) = edgeCountList ()
  edgeCountList (m:->:xs) = fromMaybe (edgeCountList ()) $ nonEmpty l where
    l = toInteger.snd <$> xs

instance (Show a, Show b) => Show (FChain a b) where
  show (m:->:xs) = show m ++ sr xs where
    sr [] = ""
    sr ((n,e):x) = "-[" ++ show e ++ "]->" ++ show n ++ sr x

instance (Texy a, Texy b) => Texy (FChain a b) where
  texy (m:->:xs) = realPow m <> ar 1 xs where
    ar :: (LaTeXC l, Texy a, Texy b) => Integer -> [(a,b)] -> l
    ar i [] = mempty
    ar i ((n,e):x) = xrightarrow (texy e) ("F" !: texy i) <> realPow n <> ar (i + 1) x

instance (Texy a, Texy b) => IHaskellDisplay (FChain a b) where
  display = displayTexyWith math

zipFChain :: NonEmpty a -> [b] -> FChain a b
zipFChain (m:|ns) es = m :->: zip ns es

toFChain' :: CompDescription a => a -> FChain'
toFChain' xs = zipFChain (toNonEmpty.toDimList $ xs) (toList.edgeCountList $ xs)
