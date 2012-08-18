module Util.Tuple where

mapSnd :: (b -> c) -> [(a,b)] -> [(a,c)]
mapSnd f [] = []
mapSnd f ((a,b):xs) = (a,f b):(mapSnd f xs)

mapFst :: (a -> c) -> [(a,b)] -> [(c,b)]
mapFst f [] = []
mapFst f ((a,b):xs) = (f a,b):(mapFst f xs)

fork :: (a -> b) -> (a -> c) -> a -> (b, c)
fork f g x = (f x, g x)

