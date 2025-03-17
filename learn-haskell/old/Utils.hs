module Utils (
key,
value,
flipKvp,
enumerate,
justLookup
) where

import Data.List ()
import Data.Map as Map ( Map, lookup )
import Data.Maybe ( fromJust )

type KeyValuePair a b = (a, b)

key :: KeyValuePair a b -> a
key (k, v) = k

value :: KeyValuePair a b -> b
value (k, v) = v

flipKvp :: KeyValuePair a b -> KeyValuePair b a
flipKvp (k, v) = (v, k)

enumerate :: [a] -> [(Int, a)]
enumerate [] = []
enumerate xs = zip [0..(length xs - 1)] xs

justLookup :: Ord k => k -> Map k v -> v
justLookup k hsh = fromJust (Map.lookup k hsh)
