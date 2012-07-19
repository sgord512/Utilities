module Util.Unicode.Greek where

import Data.Char

data Letter
 = Alpha 
 | Beta 
 | Gamma 
 | Delta 
 | Epsilon 
 | Zeta 
 | Eta 
 | Theta 
 | Iota 
 | Kappa 
 | Lambda 
 | Mu 
 | Nu 
 | Xi 
 | Omicron 
 | Pi 
 | Rho 
 | Sigma 
 | Tau 
 | Upsilon 
 | Phi 
 | Chi 
 | Psi 
 | Omega
 deriving (Bounded, Enum, Eq, Ord, Read, Show)
          
lowerAlpha = '\x03B1'          

upperAlpha = '\x0391'
   
uppercaseLetter :: Letter -> Char
uppercaseLetter l = chr $ fromEnum l + fromEnum upperAlpha
lowercaseLetter :: Letter -> Char
lowercaseLetter l = chr $ fromEnum l + fromEnum lowerAlpha