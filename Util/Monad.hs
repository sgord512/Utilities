module Util.Monad where

-- | Apply monadic computations to a value in sequence, returning the final result of passing the value through the pipeline
(~>) :: (Monad m) => a -> [(a -> m a)] -> m a
(~>) a [] = return a
a ~> (f:fs) = do f a >>= (~> fs)

unfoldrM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f state = do
  result <- f state
  case result of                
    Nothing -> return []
    Just (val, state') -> do 
      vals <- unfoldrM f state'
      return $ (val:vals)

untilM :: (Monad m) => (a -> m Bool) -> (a -> m a) -> a -> m a
untilM pred f x = do
  break <- pred x
  if break
    then return x
    else f x >>= untilM pred f