module Util.Data where

differences :: Num a => [a] -> [a] 
differences [] = []
differences (_:[]) = []
differences (x:y:xs) = (y - x) : differences (y:xs)