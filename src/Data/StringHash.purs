module Data.StringHash where

import Data.Hashable (hash)

-- TODO: implementation compatible with runtime hash function.
-- based on http://www.cse.yorku.ca/%7Eoz/hash.html
stringHash :: String -> Int
stringHash = hash
 