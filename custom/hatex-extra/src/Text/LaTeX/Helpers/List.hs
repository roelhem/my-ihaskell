module Text.LaTeX.Helpers.List where

import Text.LaTeX.Base.Class (LaTeXC)
import Text.LaTeX (Texy, texy)
import Data.Foldable (toList)
import Data.List (intersperse)

intercalate :: (Foldable t, Texy a, LaTeXC l) => l -> t a -> l
intercalate sep xs = mconcat $ intersperse sep (texy <$> toList xs)
