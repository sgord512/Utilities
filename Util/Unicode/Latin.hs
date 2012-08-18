module Util.Unicode.Latin where

import Data.Char

data Letter
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | J
  | K
  | L
  | M
  | N
  | O
  | P
  | Q
  | R
  | S
  | T
  | U
  | V
  | W
  | X
  | Y
  | Z
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

lowerA = 'a'
upperA = 'A'

uppercaseLetter :: Letter -> Char
uppercaseLetter l = chr $ fromEnum l + fromEnum upperA

lowercaseLetter :: Letter -> Char
lowercaseLetter l = chr $ fromEnum l + fromEnum lowerA