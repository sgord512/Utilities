module Util.String where

import Data.Char
import Data.List ( intersperse )

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White deriving (Enum, Bounded)

escapeString n = "\ESC[" ++ (show n) ++ "m"

reset = escapeString 0


blackNum = 30

colorStringThenSwitchToColor :: Color -> Color -> String -> String
colorStringThenSwitchToColor highlight standard str = (escapeString $ blackNum + fromEnum highlight) ++ str ++ (escapeString $ blackNum + fromEnum standard)

-- | Outputs the string argument in the color provided by the color argument. /Note that this assumes that the original text color was white/. If this is not the case use 'colorStringThenSwitchToColor'
color :: Color -> String -> String
color c str = (escapeString $ blackNum + fromEnum c) ++ str ++ reset 
                                                       
doubleQuotes :: String -> String
doubleQuotes str = "\"" ++ str ++ "\""

singleQuotes :: String -> String
singleQuotes str = "'" ++ str ++ "'"

curlyBraces :: String -> String 
curlyBraces str = "{" ++ str ++ "}"

parentheses :: String -> String
parentheses str = "(" ++ str ++ ")"

angleBrackets :: String -> String
angleBrackets str = "<" ++ str ++ ">"

stringOrNone :: String -> String
stringOrNone str | all isSpace str = "None"
                 | otherwise = str
joinWith :: String -> [String] -> String
joinWith sep ls = concat $ intersperse sep ls
