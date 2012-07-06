module Util.Monad where

-- | Apply monadic computations to a value in sequence, returning the final result of passing the value through the pipeline
(~>) :: (Monad m) => a -> [(a -> m a)] -> m a
(~>) a [] = return a
a ~> (f:fs) = do f a >>= (~> fs)
                 