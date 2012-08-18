module Util.Prelude where

-- | Ternary if-then operator 
(?) :: Bool -> a -> a -> a
(?) True t _ = t
(?) False _ f = f

infix 3 ?

-- | Alias for comparison functions
type Comparison a = (a -> a -> Ordering)

-- | Convert from implicitly ordered projection, to comparison function
toComparison :: Ord b => (a -> b) -> Comparison a
toComparison f = \a b -> f a `compare` f b