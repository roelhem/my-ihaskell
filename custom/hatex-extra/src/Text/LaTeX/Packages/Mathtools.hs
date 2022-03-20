module Text.LaTeX.Packages.Mathtools where

import Text.LaTeX.Base.Class


xrightarrow :: LaTeXC l => l -> l -> l
xrightarrow sub sup = optFixComm "xrightarrow" 1 [sub,sup]
