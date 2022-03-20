module Data.Traversable.Enumerate where

import Control.Monad.State(State, evalState, modify, get)

-- | Traverses a `Traversable` and puts an incrementing enum value at each
-- | item. The second argument determines the first element.
enumerate :: (Traversable t, Enum e) => t a -> e -> t (a, e)
enumerate target = evalState $ traverse preIncr target where
  preIncr x = get >>= (\s -> modify succ >> return (x, s))

enumMap :: (Traversable t, Enum e) => (a -> e -> b) -> t a -> e -> t b
enumMap f target start = uncurry f <$> enumerate target start

-- | Replaces each value of a `Traversable` with an enum.
enumReplace :: (Traversable t, Enum e) => t a -> e -> t e
enumReplace = enumMap (\_ y -> y)

-- PLACEHOLDERS

data Placeholder w e a = Placeholder {_class :: w, _index :: e, _value :: a}
