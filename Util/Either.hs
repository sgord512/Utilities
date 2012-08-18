module Util.Either (either, lefts, left, partitionEithers, right, rights) where

import Data.Either

left :: Either a b -> a
left (Left a) = a
left _ = error "Cannot call left on value with Right constructor"

right :: Either a b -> b
right (Right b) = b 
right _ = error "Cannot call right on value with Left constructor"