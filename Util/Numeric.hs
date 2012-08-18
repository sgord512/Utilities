module Util.Numeric where

import Util.Display
import qualified Util.Unicode as U

subscriptNumber :: Integer -> String
subscriptNumber x = map U.subscriptDigit $ x `digitsInBase` 10

superscriptNumber :: Integer -> String
superscriptNumber x = map U.superscriptDigit $ x `digitsInBase` 10

showBinary :: Integral a => a -> String
showBinary x =  map (\d -> if d == 1 then '1' else '0') $ numBaseDigits $ asBinary x

numBaseDigits :: NumBase -> [Integer]
numBaseDigits (NumBase xs _) = xs

data NumBase = NumBase [Integer] Integer deriving Show

instance Display NumBase where 
  display nb = displayNumBase nb

displayNumBase (NumBase ds b) | b > 10 = error "Character representation of digits larger than 9 is undefined"
                              | otherwise = (concat $ map show ds) ++ (subscriptNumber b)
  

inBase :: Integer -> Integer -> NumBase
inBase x b = NumBase (x `digitsInBase` b) b
  
evaluate :: NumBase -> Integer
evaluate (NumBase xs b) = snd $ foldr f (1, 0) xs
  where f a (m, sum) = (m * b, sum + m * a)

asBinary :: Integral a => a -> NumBase
asBinary x = NumBase ((toInteger x) `digitsInBase` 2) 2

digitsInBase :: Integer -> Integer -> [Integer]
digitsInBase 0 b = [0]
digitsInBase x b = dIB x b 
  where dIB x b | x == 0 = []
                | otherwise = (x' `dIB` b) ++ [r]
          where (x', r) = x `divMod` b 

changeToBase :: NumBase -> Integer -> NumBase
changeToBase (NumBase xs _) b = NumBase xs b

powerRemainder :: Integer -> Integer -> (Integer, Integer)
powerRemainder x b = (m, x - (toInteger $ b ^ m))
  where m = floor $ logBase (fromInteger b) (fromInteger x)

unbool :: Integral a => Bool -> a
unbool True = 1
unbool False = 0

bool :: Integral a => a -> Bool
bool 0 = False
bool 1 = True
bool _ = error "Coercing integers other than 0 and 1 to boolean values is not allowed"