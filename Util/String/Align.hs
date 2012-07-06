--Filename: Toolbox.hs
--Project: Utility Library
--Author: Spencer Gordon
--Date: July 16th, 2011 

module Util.String.Align where

inTwoColumns :: [(String, String)] -> Int -> String
inTwoColumns list n = let (as, bs) = unzip list
                          maxA = maximum $ map length as
                          spacedAs = map (padEndUntilLength ' ' (maxA + n)) as
                      in unlines $ zipWith (++) spacedAs bs
                    
padEndUntilLength :: Char -> Int -> String -> String
padEndUntilLength ch col word | (length word) > col = error "Starting string was longer than length"
                              | otherwise = word ++ replicate (col - length word) ch 

padFrontUntilLength :: Char -> Int -> String -> String
padFrontUntilLength ch col word | (length word) > col = error "Starting string was longer than length"
                                | otherwise = (replicate (col - length word) ch) ++ word

