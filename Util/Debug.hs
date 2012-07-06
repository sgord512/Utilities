module Util.Debug where

import Debug.Trace

-- | Postfix tracing for easier modification, and readability
tracing :: a -> String -> a
tracing = flip trace