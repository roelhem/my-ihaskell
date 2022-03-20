{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Math.AD.Real.BruteForce where

import Math.AD.Data.Bracketing
import Math.AD.Real.FuncDescription
import Control.Comonad
import Data.List.NonEmpty (toList)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Lens
import IHaskell.Display (IHaskellDisplay, display)
import Text.LaTeX.Helpers (displayTexyWith)
import Text.LaTeX (LaTeX, Texy, texy, mathsf, (!:), mathDisplay)
import Data.Traversable.Enumerate
import Data.Maybe

type TestFn = Bracketing' -> Integer
type TestConstraint = Maybe Integer

data TestRecord = TV {_recTarget :: Bracketing',
                      _fitnessValue :: Integer,
                      _constraintValue :: Integer} deriving Show

makeLenses ''TestRecord

instance Eq TestRecord where
  (TV _ x cx) == (TV _ y cy) = x == y && cx == cy

instance Ord TestRecord where
  (TV _ x cx) `compare` (TV _ y cy) = case x `compare` y of
                                        EQ -> cx `compare` cy
                                        r -> r

instance Texy TestRecord where
  texy (TV x f c) = texy (withPlaceholders x) <> mathsf ", fma=" <> texy f <> mathsf ", mem=" <> texy c where
    withPlaceholders :: Bracketing a -> Bracketing LaTeX
    withPlaceholders x = enumMap f x 1 where
      f :: a -> Integer -> LaTeX
      f _ e = "F" !: fromInteger e

instance IHaskellDisplay TestRecord  where
  display = displayTexyWith mathDisplay

prepareTestInput :: CompDescription b => TestFn -> TestFn -> b -> [TestRecord]
prepareTestInput fit con src = makeRecord <$> input where
  makeRecord x = TV x (fit x) (con x)
  input = genEach (toList.unwrapFDesc $ src)

tellMinimal :: Ord a => [a] -> Writer [a] (Maybe a)
tellMinimal [] = return Nothing
tellMinimal xs = let x = minimum xs in tell [x] >> return (Just x)

recFilter :: Maybe Integer -> [TestRecord] -> [TestRecord]
recFilter c = filter (f c) where
  f Nothing _ = True
  f (Just c) (TV _ _ v) = v < c

bfBest :: CompDescription a => TestFn -> TestFn -> Maybe Integer -> a -> Maybe TestRecord
bfBest fit con c xs = listToMaybe $ execWriter (tellMinimal fxs) where
  fxs = recFilter c $ prepareTestInput fit con xs

bfEachBest :: CompDescription a => TestFn -> TestFn -> a -> [TestRecord]
bfEachBest fit con xs = execWriter $ w Nothing input where
  input = prepareTestInput fit con xs
  w :: Maybe Integer -> [TestRecord] -> Writer [TestRecord] ()
  w c xs = do
    let fxs = recFilter c xs
    x <- tellMinimal fxs
    case x of
      Nothing -> return ()
      Just (TV _ _ c) -> w (Just c) fxs
