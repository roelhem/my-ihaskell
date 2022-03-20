{-# LANGUAGE FlexibleInstances, TypeFamilies, TemplateHaskell #-}

module Math.AD.Real.FuncDims where

import Prelude hiding (scanl)
import Math.AD.Data.Bracketing
import IHaskell.Display (IHaskellDisplay, display)
import Text.LaTeX.Helpers (displayTexyWith)
import Text.LaTeX.Base.Class (LaTeXC)
import Text.LaTeX.Packages.AMSFonts (reals)
import Text.LaTeX (Texy, (^:), texy, rightarrow, math)
import Control.Lens
import Control.Applicative
import qualified Data.List as L (intercalate)
import qualified Text.LaTeX.Helpers.List as T (intercalate)
import qualified Data.Foldable as F (toList)
import Data.List.NonEmpty hiding (repeat, zipWith, head, last, init, tail, toList)
import Data.Semigroup
import Data.Maybe
import GHC.Exts (IsList, Item, fromList, toList)

-- FuncDimensions Class
class Semigroup a => FuncDimensions a where
  dims :: a -> Dims'
  dims x = inDim x :-> outDim x
  inDim :: a -> Integer
  inDim = inDim.dims
  outDim :: a -> Integer
  outDim = outDim.dims
  {-# MINIMAL (inDim, outDim) | dims #-}

class FuncDimensions a => CompDimensions a where
  toDimList :: a -> DimList'
  toDimList = expand.unwrapDims where
    expand (x:|[]) = DimList (inDim x) [] (outDim x)
    expand (x:|xs) = DimList (inDim x) (inDim <$> xs) (outDim $ last xs)
  unwrapDims :: a -> NonEmpty Dims'
  unwrapDims xs = stitch $ toDimList xs where
    stitch (DimList h []     l) = h :-> l :| []
    stitch (DimList h (c:cs) l) = scanl next (h :-> c) (cs ++ [l])
    next (_ :-> m) n = m :-> n
  {-# MINIMAL toDimList | unwrapDims #-}


-- HELPER FUNCTIONS

-- | Compares the in dimension to the out dimension.
compareDims :: FuncDimensions a => a -> Ordering
compareDims x = inDim x `compare` outDim x

-- Instances

instance FuncDimensions () where
  dims _ = 0 :-> 0

instance CompDimensions () where
  unwrapDims _ = dims ():|[]

instance (Integral a, Integral b) => FuncDimensions (First a, Last b) where
  inDim  (m,_) = toInteger $ getFirst m
  outDim (_,n) = toInteger $ getLast n

instance (Integral a, Integral b, Semigroup c) => FuncDimensions (First a, Last b, c) where
  dims (m,n,_) = dims (m,n)

instance (Integral a, Integral b, Semigroup c, Semigroup d) => FuncDimensions (First a, Last b, c, d) where
  dims (m,n,_,_) = dims (m,n)

instance (Integral a, Integral b, Semigroup c, Semigroup d, Semigroup e) => FuncDimensions (First a, Last b, c, d, e) where
  dims (m,n,_,_,_) = dims (m,n)

instance FuncDimensions a => FuncDimensions (Maybe a) where
  dims Nothing = dims ()
  dims (Just x) = dims x

instance FuncDimensions a => FuncDimensions (Identity a) where
  dims (Identity x) = dims x

instance FuncDimensions a => CompDimensions (Identity a) where
  unwrapDims (Identity x) = dims x:|[]

instance FuncDimensions a => FuncDimensions (NonEmpty a) where
  inDim (x:|_) = inDim x
  outDim (x:|[]) = outDim x
  outDim (_:|xs) = outDim $ last xs

instance FuncDimensions a => CompDimensions (NonEmpty a) where
  unwrapDims xs = dims <$> xs

instance FuncDimensions a => FuncDimensions (Bracketing a) where
  inDim Start = 0
  inDim (Adjoint _ x) = inDim x
  inDim (Tangent x Start) = inDim x
  inDim (Tangent _ xs) = inDim xs
  inDim (Preacc lhs Start) = inDim lhs
  inDim (Preacc lhs rhs) = inDim rhs
  outDim Start = 0
  outDim (Adjoint Start x) = outDim x
  outDim (Adjoint xs _) = outDim xs
  outDim (Tangent x _) = outDim x
  outDim (Preacc Start rhs) = outDim rhs
  outDim (Preacc lhs _) = outDim lhs

instance FuncDimensions a => CompDimensions (Bracketing a) where
  unwrapDims xs = fromMaybe (unwrapDims ()) l where
    l = nonEmpty $ dims <$> F.toList xs

-- FuncDims Instance

data Dims a b = a :-> b
type Dims' = Dims Integer Integer

idDims :: a -> Dims a a
idDims n = n :-> n

emptyDims :: (Num a, Num b) => Dims a b
emptyDims = 0 :-> 0

instance (Eq a, Eq b) => Eq (Dims a b) where
  (xn :-> xm) == (yn :-> ym) = (xn == yn) && (xm == ym)

instance (Show a, Show b) => Show (Dims a b) where
  show (m :-> n) = show m ++ "->" ++ show n

instance (Texy a, Texy b) => Texy (Dims a b) where
  texy (m :-> n) = r m <> rightarrow <> r n where
    r x = reals ^: texy x

instance (Texy a, Texy b) => IHaskellDisplay (Dims a b) where
  display = displayTexyWith math

instance Semigroup (Dims a b) where
  (m :-> _) <> (_ :-> n) = m :-> n

instance Bifunctor Dims where
  bimap f g (m :-> n) = f m :-> g n

instance (Integral a, Integral b) => FuncDimensions (Dims a b) where
  inDim (m :-> _) = toInteger m
  outDim (_ :-> n) = toInteger n



-- DimList Data Structure

data DimList a = DimList {_headDim :: a, _inbetweenDims :: [a], _lastDim :: a}

makeLenses ''DimList

type DimList' = DimList Integer

instance Eq a => Eq (DimList a) where
  (DimList xh xc xl) == (DimList yh yc yl) = xh == yh && xc == yc && xl == yl

instance Functor DimList where
  fmap f (DimList h c l) = DimList (f h) (f <$> c) (f l)

instance Foldable DimList where
  foldMap f (DimList h c l) = f h <> foldMap f c <> f l

instance Applicative DimList where
  pure f = DimList f (repeat f) f
  liftA2 f (DimList xh xc xl) (DimList yh yc yl) = DimList (f xh yh) (zipWith f xc yc) (f xl yl)

instance Semigroup (DimList a) where
  (DimList xh xc xl) <> (DimList yh yc yl) = DimList xh (xc ++ [xl] ++ yc) yl

instance IsList (DimList a) where
  type Item (DimList a) = a
  fromList xs = DimList (head xs) (init $ tail xs) (last xs)
  toList xs = F.toList xs

instance Integral a => FuncDimensions (DimList a) where
  inDim (DimList h _ _) = toInteger h
  outDim (DimList _ _ l) = toInteger l

toNonEmpty :: DimList a -> NonEmpty a
toNonEmpty (DimList h c l) = h :| (c ++ [l])

fromNonEmpty :: NonEmpty a -> DimList a
fromNonEmpty (h:|[]) = DimList h [] h
fromNonEmpty (h:|hs) = DimList h (init hs) (last hs)

instance Show a => Show (DimList a) where
  show xs = L.intercalate "->" (show <$> toList xs)

instance Texy a => Texy (DimList a) where
  texy xs = T.intercalate rightarrow xs

instance Texy a => IHaskellDisplay (DimList a) where
  display = displayTexyWith math
