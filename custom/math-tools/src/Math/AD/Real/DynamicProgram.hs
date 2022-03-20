module Math.AD.Real.DynamicProgram where

import Math.AD.Data.Bracketing
import Math.AD.Real.FuncDims
import Math.AD.Real.FuncDescription
import Math.AD.Real.Properties

splitEach :: [a] -> [([a], [a])]
splitEach [] = []
splitEach [_] = []
splitEach xs = fmap (`splitAt` xs) [1..length xs-1]

data FmaRes a = FmaRes {_fmaVal :: Integer,
                        _fmaBracketing :: (Bracketing a)} deriving (Show)

instance Eq (FmaRes a) where
  x == y = _fmaVal x == _fmaVal y

instance Ord (FmaRes a) where
  x `compare` y = _fmaVal x `compare` _fmaVal y

instance Functor FmaRes where
  fmap f (FmaRes v b) = FmaRes v (fmap f b)

bFmaNaive :: FmaDescription a => [a] -> Integer
bFmaNaive [] = FmaRes 0 Start
bFmaNaive [x] = case compareDims x of
                  LT -> inDim x * edgeCount x
                  _ -> outDim x * edgeCount x
bFmaNaive xs = _
  where bOfSplit (l,r) = minimum [ bFmaNaive l + bFmaNaive r + fmaMatProd l r
                                   
                        ]

dpFmaNaive :: CompDescription a => a -> Integer
dpFmaNaive cxs = _

-- bestFmaNaive :: FuncDescription a => [a] -> FmaRes a
-- bestFmaNaive [] = (0, Start)
-- bestFmaNaive [x] = case compareDims x of
--   LT -> (inDim x * edgeCount x, Tangent x Start)
--   _  -> (outDim x * edgeCount x, Adjoint Start x)
-- bestFmaNaive xs = minimum $ f <$> splitEach xs where
--   preacc (xl,xr) = let (FmaRes xlv xlb) = bestFmaNaive xl
--                        (FmaRes xrv xrb) = bestFmaNaive xr in
--                        FmaRes (xlv + xrv + fmaMatProd xlb xrb) (Preacc xlb xrb)
--   tangent (xl,xr) = let (FmaRes xrv xrb) = bestFmaNaive xr,
--                         ht = homTangent xl in
--                        FmaRes (xrv + fma ht) (ht <> xrb)
--   adjoint (xl,xr) = let (FmaRes xlv xlb) = bestFmaNaive xl,
--                         ha = homAdjoint xr in
--                        FmaRes (xlv + fma ha) (xlb <> ha)
--   f xs = minimum [preacc xs, tangent xs, adjoint xs]
