module Util.Unicode where

import Data.Char
import Numeric

-- | Logical \'and\'
and = '\x2227'
-- | Locical \'or\'
or = '\x2228'
-- | Logical conditional
conditional = '\x2192'
-- | Logical biconditional 
biconditional = '\x2194'
-- | Big box
box = '\x2B1C'
-- | Set membership
memberOf = '\x2208'
notMemberOf = '\x2209'
-- | Three dots, also known as \"therefore\"
because = '\x2235'
-- | Colon equals \':=\', variable assignment in old-fashioned languages
assign = '\x2254'
-- | Checkmark
check = '\x2713'
-- | Null set
emptySet = '\x2205'

startItalic = fst $ head $ readHex "1D434"

upperA = 65

-- | Map character of Arabic alphabet to italic script character, using unicode code point 'startItalic' as \'A\'
italic :: Char -> Char
italic c = chr $ (ord c - upperA) + startItalic

-- | Convert 'Char' to 'String' by creating a singleton list. (I don't know if this is a good idea or not.)
c2s :: Char -> String 
c2s c = [c]