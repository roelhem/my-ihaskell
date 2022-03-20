module Text.LaTeX.Helpers.Display (
  displayTexy,
  displayTexyWith,
  LaTeX, Texy, texy, math, mathDisplay, equation, equation_
) where

import IHaskell.Display (display, Display)
import IHaskell.Display.Hatex ()
import Text.LaTeX (LaTeX, Texy, texy, math, mathDisplay, equation, equation_)

displayTexyWith :: (Texy a) => (LaTeX -> LaTeX) -> a -> IO Display
displayTexyWith f = display . f . texy

displayTexy :: (Texy a) => a -> IO Display
displayTexy = displayTexyWith id
