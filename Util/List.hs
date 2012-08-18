module Util.List where

import Data.List
import System.Random
import Util.Prelude

-- | Exactly analogous to forM in "Control.Monad". Equivalent to 'map' with the arguments reversed.
for :: [a] -> (a -> b) -> [b]
for [] f = []
for (x:xs) f = f x : for xs f

-- | Breaks a list into smaller lists of a given length, and returns the list of those sublists. If the original length is a not a multiple of the chunk length, there will be one sublist with remainder elements.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (chk, rest) = splitAt n xs
             in (chk : chunk n rest)

-- | Combines a filter and map operation, mapping a function over the filtered elements. Takes a function that returns a 'Data.Maybe', and all items that are Just will be returned, and all that are Nothing will be discarded. 
filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f [] = []
filterMap f (x:xs) = case f x of
  Nothing -> filterMap f xs
  Just v -> v : (filterMap f xs)

-- | Takes multiple functions of the same type and applies each one to a single item, and returns the list of results. This can be done with Applicative Functors as well. 
applyAll :: [(a -> b)] -> a -> [b]
applyAll fs a = zipWith ($) fs (repeat a)

-- | Randomly picks an element of a provided list using a provided instance of 'RandomGen'
randomElement :: (RandomGen g) => g -> [a] -> (a, g)
randomElement gen ls = let (ix, gen') = randomR (0, length ls - 1) gen
                       in (ls !! ix, gen')                    

-- | Split a list into 2 lists, the first containing the elements with even indices, and the second with odd indices
evensAndOdds :: [a] -> ([a], [a])
evensAndOdds = alternate e o ([], [])
  where e (es, os) e' = (e':es, os)
        o (es, os) o' = (es, o':os)

-- | Recursively fold over a list twice, alternating between the first and second folds at every element
alternate :: (b -> a -> b) -> (b -> a -> b) -> b -> [a] -> b
alternate f g init [] = init
alternate f g init xs = f' init xs
  where f' d (x:xs) = if null xs then f d x else g' (f d x) xs
        g' d (x:xs) = if null xs then g d x else f' (g d x) xs

-- | The items in the domain list that are not in the second argument list 
complementDomain :: Eq a => [a] -> [a] -> [a]
complementDomain d = (d \\)

-- | Remove the minimum element of a list, according to a user provided comparison function
removeMinBy :: Comparison a -> [a] -> Maybe (a, [a])
removeMinBy comp [] = Nothing
removeMinBy comp (x:xs) = Just $ removeMinBy' comp x xs []
  where removeMinBy' :: Comparison a -> a -> [a] -> [a] -> (a, [a])
        removeMinBy' comp curr [] visited = (curr, visited)
        removeMinBy' comp curr (x:xs) visited = case x `comp` curr of 
          LT -> removeMinBy' comp x xs (curr:visited)
          _ -> removeMinBy' comp curr xs (x:visited)
