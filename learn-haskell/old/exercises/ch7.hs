{-
Express the comprehension [f x | x <- xs, p x] using map and filter
-}
origFunc :: (t -> a) -> (t -> Bool) -> [t] -> [a]
origFunc f p xs = [f x | x <- xs, p x]

newFunc :: (a -> b) -> (a -> Bool) -> [a] -> [b]
newFunc f p xs = map f (filter p xs)

{-
Redefine map using foldr

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = (f x) : map f xs
-}

newMap :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
newMap f = foldr (\x vs -> (f x) : vs) []

{-
Redefine filter using foldr

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs) | f x = x : filter f xs
                | otherwise = filter f xs
-}
myFilter :: Foldable t => (a -> Bool) -> t a -> [a]
myFilter f = foldr (\x vs -> if f x then x : vs else vs) []
