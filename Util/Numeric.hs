module Util.Numeric where

x `logBase` b = lb' x b 0
                                        
lb' x b count = let q = x `quot` b 
                in if q > 0 
                   then lb' q b (count + 1) 
                   else count