module Text.LaTeX.Helpers.Real where

import Text.LaTeX.Base.Class (LaTeXC)
import Text.LaTeX
import Text.LaTeX.Packages.AMSFonts
import Text.LaTeX.Packages.Mathtools

realPow :: (Texy a, LaTeXC l) => a -> l
realPow n = reals ^: texy n

realFunc :: (Texy a, Texy b, LaTeXC l) => a -> b -> l
realFunc n m = realPow n <> rightarrow <> realPow m

realFuncWithEdges :: (Texy a, Texy b, Texy c, LaTeXC l) => a -> b -> c -> l
realFuncWithEdges n m e = realPow n <> xrightarrow (texy e) mempty <> realPow m
