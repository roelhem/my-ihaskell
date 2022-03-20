module Text.LaTeX.Helpers.Placeholders where

-- import Data.Traversable.Enumerate
-- import Text.LaTeX (LaTeX)
-- import Text.LaTeX.Base.Class (LaTeXC)

-- data PlaceholderSymbol a b c = PlaceholderSymbol { _symbol :: a, _index :: b, _original :: c }
--
-- instance (Show a, Show b) => Show (PlaceholderSymbol a b c) where
--   show (PlaceholderSymbol s i _) = show s ++ "_" ++ show i
--
-- instance (Eq b) => Eq (PlaceholderSymbol a b c) where
--   (PlaceholderSymbol _ i _) == (PlaceholderSymbol _ j _) = i == j
--
-- withPlaceholders :: (Traversable t, Enum e) => s -> e -> t a -> t (PlaceholderSymbol s e a)
-- withPlaceholders symb i xs = enumMap f xs i where
--   f x e = PlaceholderSymbol symb e x
