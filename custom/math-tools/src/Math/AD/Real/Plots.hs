{-# LANGUAGE FlexibleInstances #-}

module Math.AD.Real.Plots where

import Math.AD.Real.FuncDims
import Math.AD.Real.BruteForce
import Control.Lens
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Data.List

recPoint :: Lens' TestRecord Point
recPoint f (TV v y x) = m <$> f p where
  p = Point (fromInteger x) (fromInteger y)
  m (Point x' y') = TV v (round y') (round x')

recsToPlotPoints :: [TestRecord] -> [(Double, Double)]
recsToPlotPoints [] = []
recsToPlotPoints rs = go $ sortBy order $ rmap <$> rs where
  rmap (TV _ y x) = (fromInteger x,fromInteger y)
  order (xa,ya) (xb,yb) = xa `compare` xb <> yb `compare` ya
  endPoint [] = (0,0)
  endPoint [(x,y)] = (x * 1.05, y)
  endPoint ((_,y):(x,_):ps) = (x,y)
  go [] = []
  go ps = [head ps, endPoint ps] ++ go (tail ps)

plotRecs :: String -> [TestRecord] -> EC (Layout Double Double) ()
plotRecs name recs = plot $ line name [recsToPlotPoints recs]

bestForConstraintLines :: [TestRecord] -> Path
bestForConstraintLines [] = mempty
bestForConstraintLines rs = line points where
  order (Point xa ya) (Point xb yb) = xa `compare` xb <> yb `compare` ya
  points = sortBy order $ view recPoint <$> rs
  line [] = mempty
  line ps = moveTo (head ps) <> lineTo (endPoint ps) <> line (tail ps)
  endPoint [] = Point 0 0
  endPoint [Point x y] = Point (x * 1.05) y
  endPoint ((Point _ y):(Point x _):ps) = Point x y

-- bestForConstraintLines (x:xs) = mconcat $ zipWith line (x:xs) xs where
--   line (TV _ y xa) (TV _ _ xb) = moveTo' (fromInteger xa) (fromInteger y) <> lineTo' (fromInteger xb) (fromInteger y)
