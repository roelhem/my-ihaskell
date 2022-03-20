{-# LANGUAGE TypeApplications, OverloadedStrings, FlexibleInstances, TypeFamilies #-}

module Math.AD.Real.FuncDescription where

import Prelude hiding (zipWith)
import Math.AD.Real.FuncDims
import Data.Bifunctor
import Math.AD.Data.Bracketing (Bracketing(..))
import IHaskell.Display (IHaskellDisplay, display)
import IHaskell.Display.Hatex ()
import Data.List.NonEmpty
import Data.Semigroup
import qualified Data.Foldable as F
import Data.Maybe
import Data.Functor.Identity
import Text.LaTeX.Helpers.Real (realFunc, realFuncWithEdges)
import Text.LaTeX.Base.Class (LaTeXC)
import Text.LaTeX (Texy, LaTeX, texy, math, mathtt, rendertex, protectString, fromString, raw)

{- FuncDesc class -}

class NamedFunc a where
  funcName :: a -> String
  funcName x = show (funcNameTex x :: LaTeX)
  funcNameTex :: LaTeXC l => a -> l
  funcNameTex x = mathtt (fromString $ protectString s) where
    s = funcName x
  {-# MINIMAL funcName | funcNameTex #-}

class FuncDimensions a => FuncDescription a where
  edgeCount :: a -> Integer
  edgeCount x = _fEdgeCount $ toFDesc x
  toFDesc :: a -> FDesc'
  toFDesc f = F (dims f) (edgeCount f)
  {-# MINIMAL edgeCount | toFDesc #-}

class (CompDimensions a, FuncDescription a) => CompDescription a where
  edgeCountList :: a -> NonEmpty Integer
  edgeCountList xs = _fEdgeCount <$> unwrapFDesc xs
  unwrapFDesc :: a -> NonEmpty FDesc'
  unwrapFDesc xs = zipWith F (unwrapDims xs) (edgeCountList xs)
  {-# MINIMAL edgeCountList | unwrapFDesc #-}

-- Instances

instance FuncDescription () where
  edgeCount _ = 0

instance CompDescription () where
  edgeCountList _ = 0:|[]

instance NamedFunc () where
  funcName _ = "∅"
  funcNameTex _ = raw "\\emptyset"

instance FuncDescription a => FuncDescription (Maybe a) where
  edgeCount Nothing = edgeCount ()
  edgeCount (Just x) = edgeCount x

instance (Integral a, Integral b) => FuncDescription (First a, Last b) where
  edgeCount _ = edgeCount ()

instance (Integral a, Integral b, Integral c) => FuncDescription (First a, Last b, Sum c) where
  edgeCount (_,_,e) = toInteger $ getSum e

instance (Integral a, Integral b, Integral c, Semigroup d) => FuncDescription (First a, Last b, Sum c, d) where
  edgeCount (m,n,e,_) = edgeCount (m,n,e)

instance (Integral a, Integral b, Integral c, Semigroup d, Semigroup e) => FuncDescription (First a, Last b, Sum c, d, e) where
  edgeCount (m,n,e,_,_) = edgeCount (m,n,e)

instance FuncDescription a => FuncDescription (Identity a) where
  edgeCount (Identity x) = edgeCount x

instance FuncDescription a => CompDescription (Identity a) where
  edgeCountList (Identity x) = edgeCount x:|[]
  unwrapFDesc (Identity x) = toFDesc x:|[]

instance FuncDescription a => FuncDescription (NonEmpty a) where
  edgeCount = sum.fmap edgeCount

instance FuncDescription a => CompDescription (NonEmpty a) where
  edgeCountList xs = edgeCount <$> xs
  unwrapFDesc xs = toFDesc <$> xs

instance FuncDescription a => FuncDescription (Bracketing a) where
  edgeCount xs = foldr (\x y -> edgeCount x + y) 0 xs

instance FuncDescription a => CompDescription (Bracketing a) where
  edgeCountList xs = fromMaybe (edgeCountList ()) l where
    l = nonEmpty $ edgeCount <$> F.toList xs

{- FDesc datatype -}

data FDesc a b = F {_fDims :: a, _fEdgeCount :: b}
type FDesc' = FDesc Dims' Integer

type Bracketing' = Bracketing FDesc'

idF :: Num b => a -> FDesc (Dims a a) b
idF n = F (idDims n) 0

emptyF :: (Num a, Num b, Num c) => FDesc (Dims a b) c
emptyF = F emptyDims 0

denseMatixF :: FuncDimensions a => a -> FDesc'
denseMatixF x = F d (m*n) where
  d@(m :-> n) = dims x

pointwiseF :: (Num a, Num a) => a -> a -> FDesc (Dims a a) a
pointwiseF eb n = F (idDims n) (eb * n)

crossF :: (FuncDimensions a) => Integer -> a -> FDesc'
crossF eb d = pointwiseF eb (inDim d) <> denseMatixF d

reduceF :: (Num a) => a -> a -> FDesc (Dims a a) a
reduceF eb m = F (m:->1) (eb*m)

instance Bifunctor FDesc where
  bimap f g (F d e) = F (f d) (g e)

instance Functor (FDesc a) where
  fmap = second

instance (Semigroup a, Num b) => Semigroup (FDesc a b) where
  (F xd xe) <> (F yd ye) = F (xd <> yd) (xe + ye)

instance (Show a, Show b) => Show (FDesc a b) where
  show (F d e) = "F:" ++ show d ++ " |" ++ show e ++ "|"

instance (FuncDimensions a, Texy b) => Texy (FDesc a b) where
  texy (F d e) = "F:" <> realFuncWithEdges m n e where
    (m :-> n) = dims d

instance (FuncDimensions a, Texy b) => IHaskellDisplay (FDesc a b) where
  display = display.math @LaTeX .texy

instance (FuncDimensions a, Num b) => FuncDimensions (FDesc a b) where
  dims (F d _)  = dims d

instance (FuncDimensions a, Integral b) => FuncDescription (FDesc a b) where
  edgeCount (F _ e) = toInteger e
