module Util.String.Align where

import Data.List
import Util.String

data RelativePosition = Before | After

data Alignment = Left | Right | Center

splitAtChar :: Char -> String -> [String]
splitAtChar c [] = []
splitAtChar c xs = 
  case break (== c) xs of
    (chunk, []) -> chunk : []
    (chunk, (_:rest)) -> chunk : splitAtChar c rest
                      
padStringsToLength :: Int -> [String] -> [String]                      
padStringsToLength n strs = strs ++ (replicate (n - length strs) "")

alignAtChar :: Char -> [String] -> [String]
alignAtChar c strs = let strRows = map (splitAtChar c) strs
                         maxRowLength = maximum $ map length strRows
                         strRows' = map (padStringsToLength maxRowLength) strRows
                         strCols = transpose strRows'
                         paddedCols = map (padToMaximumLength Before ' ') strCols
                         paddedRows = transpose paddedCols
                     in map (joinWith $ ' ':c:' ':[]) paddedRows

inTwoColumns :: [(String, String)] -> Int -> String
inTwoColumns list n = let (as, bs) = unzip list
                          maxA = maximum $ map length as
                          spacedAs = map (padEndUntilLength ' ' (maxA + n)) as
                      in unlines $ zipWith (++) spacedAs bs
                    
padToMaximumLength :: RelativePosition -> Char -> [String] -> [String]                         
padToMaximumLength pos ch ls = map (padFunc pos ch len) ls
  where len = maxLength ls
        padFunc :: RelativePosition -> Char -> Int -> String -> String
        padFunc Before = padFrontUntilLength
        padFunc After = padEndUntilLength
                         
maxLength :: [String] -> Int
maxLength = maximum . map length

-- | Should probably be named 'padBackUntilLength'
padEndUntilLength :: Char -> Int -> String -> String
padEndUntilLength ch col word | (length word) > col = error "Starting string was longer than length"
                              | otherwise = word ++ replicate (col - length word) ch 

padFrontUntilLength :: Char -> Int -> String -> String
padFrontUntilLength ch col word | (length word) > col = error "Starting string was longer than length"
                                | otherwise = (replicate (col - length word) ch) ++ word

